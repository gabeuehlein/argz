const std = @import("std");
const util = @import("util.zig");
const Args = @import("args.zig").Args;
const argz = @import("argz.zig");

const assert = std.debug.assert;

const Config = argz.Config;
const Flag = argz.Flag;
const Command = argz.Command;
const Positional = argz.Positional;
const FlagType = util.FlagType;

const Lexer = @This();

args: Args,
argi: usize = 1,
/// The current position in `args.get(argi)`. A value not equal to zero
/// indicates that we are currently in the middle of lexing a sequence
/// of short flags.
subargi: usize = 0,
found_force_stop: bool = false,

pub const WordMode = union(enum) {
    positionals,
    commands: struct {
        commands: []const Command,
        default: ?CommandIndex,
    },
};

pub const ArgIndex = enum(usize) {
    _,

    pub fn get(idx: ArgIndex, args: Args) []const u8 {
        return args.get(@intFromEnum(idx));
    }

    /// Returns a [std.fmt.Formatter] whose `format` method writes an English
    /// string pertaining to the `self`th ordinal number. For example, if `@intFromEnum(idx)`
    /// is `3`, then `idx.formatSuffixed().format(...)` will write `3rd` to the writer.
    pub fn formatSufixed(idx: ArgIndex) std.fmt.Formatter(formatArgvIndex) {
        return .{ .data = @intFromEnum(idx) };
    }

    fn formatArgvIndex(data: usize, comptime _: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        // Even though this is 32 bytes large, we only use 20 bytes at most (18 decimal digits + 2 bytes for the suffix).
        // The size is 32 bytes because 32 is simply a better number than 20.
        var scratch = @as([32]u8, undefined);
        const first_digit = data % 10;
        const int_offset = std.fmt.formatIntBuf(&scratch, data, 10, .lower, .{}) catch unreachable;
        @memcpy(scratch[int_offset..][0..2], switch (first_digit) {
            1 => "st",
            2 => "nd",
            3 => "rd",
            else => "th",
        });
        return std.fmt.formatBuf(scratch[0 .. int_offset + 2], options, writer);
    }
};

pub const FlagIndex = enum(usize) {
    _,

    pub fn get(comptime idx: FlagIndex, comptime flags: []const Flag) Flag {
        return flags[@intFromEnum(idx)];
    }
};

pub const CommandIndex = enum(usize) {
    _,

    pub fn get(comptime idx: CommandIndex, comptime commands: []const Command) Command {
        return commands[@intFromEnum(idx)];
    }
};

pub const Span = struct {
    argv_index: ArgIndex,
    start: usize,
    end: usize,

    pub fn len(span: Span) usize {
        assert(span.end >= span.start);
        return span.end - span.start;
    }

    pub fn getText(span: Span, args: Args) []const u8 {
        return span.argv_index.get(args)[span.start..span.end];
    }
};

pub const TokenTag = enum {
    long_flag,
    long_flag_with_value,
    short_flag,
    short_flag_with_value,
    command,
    positional,
    force_stop,
    err,
};

pub const Token = union(TokenTag) {
    long_flag: Token.Flag,
    long_flag_with_value: FlagWithValue,
    short_flag: Token.Flag,
    short_flag_with_value: FlagWithValue,
    command: Token.Command,
    positional: Token.Positional,
    force_stop,
    err: Error,

    pub const Flag = struct {
        index: FlagIndex,
    };

    pub const FlagWithValue = struct {
        index: FlagIndex,
        value_span: Span,
    };

    pub const Command = struct {
        index: CommandIndex,
    };

    pub const Positional = struct {
        argv_index: ArgIndex,
    };

    pub const ErrorTag = enum {
        unknown_short_flag,
        unknown_long_flag,
        unknown_command,
        missing_value_for_short_flag,
        missing_value_for_long_flag,
        expected_value_for_short_flag,
        expected_value_for_long_flag,
        unexpected_value_for_short_flag,
        unexpected_value_for_long_flag,
        unexpected_force_stop,
        empty_argument,
    };

    pub const Error = union(ErrorTag) {
        unknown_short_flag: UnknownFlag,
        unknown_long_flag: UnknownFlag,
        unknown_command: UnknownCommand,
        missing_value_for_short_flag: MissingValueForFlag,
        missing_value_for_long_flag: MissingValueForFlag,
        expected_value_for_short_flag: ExpectedValueForFlag,
        expected_value_for_long_flag: ExpectedValueForFlag,
        unexpected_value_for_short_flag: UnexpectedValueForFlag,
        unexpected_value_for_long_flag: UnexpectedValueForFlag,
        unexpected_force_stop,
        empty_argument: EmptyArgument,

        pub const UnknownFlag = struct {
            span: Span,
        };

        pub const UnknownCommand = struct {
            argv_index: ArgIndex,
        };

        pub const MissingValueForFlag = struct {
            index: FlagIndex,
        };

        pub const ExpectedValueForFlag = struct {
            index: FlagIndex,
        };

        pub const UnexpectedValueForFlag = struct {
            index: FlagIndex,
            value_span: Span,
        };

        pub const EmptyArgument = struct {
            argv_index: ArgIndex,
        };

        /// Returns a [FlagType] that best fits the cause of the error. If it
        /// pertains to a short flag (e.g. `.unknown_short_flag`), `.short` will be returned.
        /// If it pertains to a long flag (e.g. `.missing_value_for_long_flag`), `.long` will be returned.
        /// Any other variant is assumed to be unreachable.
        pub fn toFlagType(err: Error) FlagType {
            return switch (err) {
                .unknown_short_flag,
                .missing_value_for_short_flag,
                .expected_value_for_short_flag,
                .unexpected_value_for_short_flag,
                => .short,
                .unknown_long_flag,
                .missing_value_for_long_flag,
                .expected_value_for_long_flag,
                .unexpected_value_for_long_flag,
                => .long,
                else => unreachable,
            };
        }
    };
};

pub fn init(args: Args) !Lexer {
    if (args.len == 0)
        return error.NoArguments;
    for (1..args.len) |i| {
        const arg = args.get(i);
        if (!std.unicode.utf8ValidateSlice(arg))
            return error.InvalidUtf8;
    }
    return .{ .args = args };
}

pub fn nextToken(lexer: *Lexer, comptime flags: []const Flag, comptime word_mode: WordMode) ?Token {
    if (lexer.argi >= lexer.args.len)
        return null;
    if (lexer.subargi != 0) {
        if (lexer.subargi == lexer.args.get(lexer.argi).len) {
            // We already ate the entire short chain, skip to the next argument
            if (!lexer.loadNextArg())
                return null;
        } else {
            // Handle short chain
            return lexer.shortFlag(flags, lexer.subargi);
        }
    }
    if (lexer.found_force_stop) {
        defer lexer.argi += 1;
        return .{ .positional = .{ .argv_index = @enumFromInt(lexer.argi) } };
    }
    const first_arg = lexer.currentArg() orelse return null;
    if (lexer.maybe('-')) {
        if (lexer.maybe('-')) {
            if (lexer.subargi == first_arg.len) {
                // We found a "--" token; figure out what to do with it
                _ = lexer.loadNextArg();
                return switch (word_mode) {
                    .positionals => blk: {
                        lexer.found_force_stop = true;
                        break :blk .force_stop;
                    },
                    .commands => .{ .err = .unexpected_force_stop },
                };
            } else return lexer.longFlag(flags, first_arg);
        } else if (first_arg.len == 1) {
            defer _ = lexer.loadNextArg();
            return switch (word_mode) {
                .positionals => .{ .positional = .{ .argv_index = @enumFromInt(lexer.argi) } },
                .commands => |data| inline for (data.commands, 0..) |cmd, i| {
                    // Inlining the comparison is more efficient than a comptime
                    // call to std.mem.eql
                    if (cmd.cmd.len == 1 and cmd.cmd[0] == '-')
                        break .{ .command = .{ .index = @enumFromInt(i) } };
                } else if (data.default) |default|
                    .{ .command = .{ .index = @enumFromInt(default) } }
                else
                    .{ .err = .{ .unknown_command = .{ .argv_index = @enumFromInt(lexer.argi) } } },
            };
        } else {
            return lexer.shortFlag(flags, 1);
        }
    } else {
        // Positional or command
        defer lexer.argi += 1;
        return switch (word_mode) {
            .positionals => if (first_arg.len == 0 and !lexer.found_force_stop)
                .{ .err = .{ .empty_argument = .{ .argv_index = @enumFromInt(lexer.argi) } } }
            else
                .{ .positional = .{ .argv_index = @enumFromInt(lexer.argi) } },
            .commands => |data| inline for (data.commands, 0..) |cmd, i| {
                if (std.mem.eql(u8, cmd.cmd, first_arg))
                    break .{ .command = .{ .index = @enumFromInt(i) } };
            } else if (data.default) |default|
                .{ .command = .{ .index = default } }
            else
                .{ .err = .{ .unknown_command = .{ .argv_index = @enumFromInt(lexer.argi) } } },
        };
    }
}

fn shortFlag(lexer: *Lexer, comptime flags: []const Flag, subargi: usize) Token {
    const arg = lexer.currentArg().?;
    assert(subargi < arg.len);

    const char_len, const char = decodeCharAtArgPos(arg, subargi);
    lexer.subargi = subargi + char_len;

    inline for (flags, 0..) |flag, i| {
        if (char == flag.short) {
            switch (@typeInfo(flag.type)) {
                .void => return .{ .short_flag = .{
                    .index = @enumFromInt(i),
                } },
                .optional => if (lexer.maybe('=')) {
                    defer _ = lexer.loadNextArg();
                    return .{ .short_flag_with_value = .{
                        .index = @enumFromInt(i),
                        .value_span = Span{
                            .argv_index = @enumFromInt(lexer.argi),
                            .start = subargi + char_len + 1,
                            .end = arg.len,
                        },
                    } };
                } else {
                    return .{ .short_flag = .{
                        .index = @enumFromInt(i),
                    } };
                },
                else => if (subargi + char_len == arg.len) {
                    if (!lexer.loadNextArg()) {
                        return .{ .err = .{ .expected_value_for_short_flag = .{
                            .index = @enumFromInt(i),
                        } } };
                    }
                    const next_arg = lexer.currentArg().?;
                    defer lexer.argi += 1;
                    return .{ .short_flag_with_value = .{
                        .index = @enumFromInt(i),
                        .value_span = Span{
                            .argv_index = @enumFromInt(lexer.argi),
                            .start = 0,
                            .end = next_arg.len,
                        },
                    } };
                } else {
                    defer lexer.argi += 1;
                    return .{ .short_flag_with_value = .{
                        .index = @enumFromInt(i),
                        .value_span = Span{
                            .argv_index = @enumFromInt(lexer.argi),
                            .start = subargi + char_len,
                            .end = arg.len,
                        },
                    } };
                },
            }
        }
    }
    return .{ .err = .{ .unknown_short_flag = .{ .span = Span{
        .argv_index = @enumFromInt(lexer.argi),
        .start = subargi,
        .end = subargi + char_len,
    } } } };
}

fn longFlag(lexer: *Lexer, comptime flags: []const Flag, arg: []const u8) Token {
    assert(arg.len > 2);
    assert(lexer.subargi == 2);
    lexer.subargi = 0;

    const flag_end = std.mem.indexOfScalar(u8, arg, '=');
    var found = false;
    defer {
        if (found)
            _ = lexer.loadNextArg();
    }

    inline for (flags, 0..) |flag, i| {
        if (flag.long) |long| {
            if (std.mem.eql(u8, long, arg[2 .. flag_end orelse arg.len])) {
                found = true;
                if (flag.type == argz.FlagHelp)
                    return if (flag_end) |eq_idx| .{ .long_flag_with_value = .{
                        .index = @enumFromInt(i),
                        .value_span = Span{ .argv_index = @enumFromInt(lexer.argi), .start = eq_idx + 1, .end = arg.len },
                    } } else .{ .long_flag = .{
                        .index = @enumFromInt(i),
                    } };
                switch (@typeInfo(flag.type)) {
                    .void => return if (flag_end) |eq_idx|
                        .{ .err = .{ .unexpected_value_for_long_flag = .{
                            .index = @enumFromInt(i),
                            .value_span = Span{ .argv_index = @enumFromInt(lexer.argi), .start = eq_idx + 1, .end = arg.len },
                        } } }
                    else
                        .{ .long_flag = .{
                            .index = @enumFromInt(i),
                        } },
                    .optional => return if (flag_end) |eq_idx| .{ .long_flag_with_value = .{
                        .index = @enumFromInt(i),
                        .value_span = Span{ .argv_index = @enumFromInt(lexer.argi), .start = eq_idx + 1, .end = arg.len },
                    } } else .{ .long_flag = .{
                        .index = @enumFromInt(i),
                    } },
                    else => return if (flag_end) |eq_idx| .{ .long_flag_with_value = .{
                        .index = @enumFromInt(i),
                        .value_span = Span{
                            .argv_index = @enumFromInt(lexer.argi),
                            .start = eq_idx + 1,
                            .end = arg.len,
                        },
                    } } else blk: {
                        if (!lexer.loadNextArg())
                            break :blk .{ .err = .{ .expected_value_for_long_flag = .{ .index = @enumFromInt(i) } } };
                        const next_arg = lexer.currentArg().?;
                        break :blk .{ .long_flag_with_value = .{ .index = @enumFromInt(i), .value_span = Span{ .argv_index = @enumFromInt(lexer.argi), .start = 0, .end = next_arg.len } } };
                    },
                }
            }
        }
    }
    return .{ .err = .{ .unknown_long_flag = .{ .span = Span{
        .argv_index = @enumFromInt(lexer.argi),
        .start = 2,
        .end = flag_end orelse arg.len,
    } } } };
}

fn loadNextArg(lexer: *Lexer) bool {
    if (lexer.argi < lexer.args.len) {
        lexer.argi += 1;
        lexer.subargi = 0;
        return true;
    }
    return false;
}

fn peek(lexer: *const Lexer) ?u21 {
    return if (lexer.argi >= lexer.args.len or lexer.subargi >= lexer.args.get(lexer.argi).len)
        null
    else
        decodeCharAtArgPos(lexer.args.get(lexer.argi), lexer.subargi)[1];
}

fn maybe(lexer: *Lexer, char: u21) bool {
    const arg = lexer.currentArg() orelse return false;
    if (lexer.subargi >= arg.len) return false;

    const char_len, const next_char = decodeCharAtArgPos(arg, lexer.subargi);
    if (next_char == char) {
        lexer.subargi += char_len;
        return true;
    } else return false;
}

fn currentArg(lexer: *const Lexer) ?[]const u8 {
    return if (lexer.argi < lexer.args.len) lexer.args.get(lexer.argi) else null;
}

fn decodeCharAtArgPos(arg: []const u8, pos: usize) struct { u3, u21 } {
    const char_len = std.unicode.utf8ByteSequenceLength(arg[pos]) catch unreachable;
    return .{ char_len, switch (char_len) {
        1 => arg[pos],
        2 => std.unicode.utf8Decode2(arg[pos..][0..2].*) catch unreachable,
        3 => std.unicode.utf8Decode3(arg[pos..][0..3].*) catch unreachable,
        4 => std.unicode.utf8Decode4(arg[pos..][0..4].*) catch unreachable,
        else => unreachable,
    } };
}

comptime {
    std.testing.refAllDeclsRecursive(Lexer);
}

test Lexer {
    const argv: []const [:0]const u8 = &.{
        "program",
        "--bar=foo",
        "-qwerty=99",
    };
    var sys_args = argz.OwnedArgs.init(argv);
    const args = sys_args.args();
    var lexer = try Lexer.init(args);
    const flags = &[_]Flag{
        .{ .short = null, .long = "bar", .type = u32 },
        .{ .short = 'q', .long = null },
        .{ .short = 'w', .long = null },
        .{ .short = 'e', .long = null },
        .{ .short = 'r', .long = null },
        .{ .short = 't', .long = null },
        .{ .short = 'y', .long = null, .type = ?u32 },
    };
    while (lexer.nextToken(flags, .positionals)) |tok| {
        std.log.err("{any}", .{tok});
    }
}
