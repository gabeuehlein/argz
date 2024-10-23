const std = @import("std");
const util = @import("util.zig");
const Args = @import("args.zig").Args;
const argz = @import("argz.zig");

const Config = argz.Config;
const Flag = argz.Flag;
const Command = argz.Command;
const Positional = argz.Positional;

const Lexer = @This();

args: Args,
argi: usize = 1,
subargi: ?usize = null,
found_force_stop: bool = false,

pub const WordMode = union(enum) { positionals, commands: []const Command };

pub const Span = struct {
    argv_index: usize,
    start: usize,
    end: usize,

    /// Returns a [std.fmt.Formatter] whose `format` method outputs a English numeric
    /// string pertaining to the `self.argv_index`th number. For example, if `self.argv_index`
    /// is `3`, then `self.argvIndexFormatter().format(...)` will write `3rd` to the writer.
    pub fn argvIndexFormatter(self: @This()) std.fmt.Formatter(formatArgvIndex) {
        return .{ .data = self.argv_index };
    }

    pub fn len(self: @This()) usize {
        util.runtimeCheck(self.end >= self.start, "span's endpoint is before its start (start = {d}, end = {d})", .{ self.start, self.end });
        return self.end - self.start;
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

pub const TokenTag = enum { long_flag, long_flag_with_value, short_flag, short_flag_with_value, command, positional, force_stop, err };

pub const Token = union(TokenTag) {
    long_flag: Token.Flag,
    long_flag_with_value: FlagWithValue,
    short_flag: Token.Flag,
    short_flag_with_value: FlagWithValue,
    command: Token.Command,
    positional: Token.Positional,
    force_stop,
    err: Error,

    pub const Flag = struct { index: usize };
    pub const FlagWithValue = struct { index: usize, value_span: Span };
    pub const Command = struct { index: usize };
    pub const Positional = struct {
        span: Span,
    };

    pub const ErrorTag = enum { unknown_short_flag, unknown_long_flag, unknown_command, missing_value_for_short_flag, missing_value_for_long_flag, expected_value_for_short_flag, expected_value_for_long_flag, unexpected_value_for_short_flag, unexpected_value_for_long_flag, unexpected_force_stop, empty_argument };

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
            span: Span,
        };

        pub const MissingValueForFlag = struct {
            index: usize,
        };

        pub const ExpectedValueForFlag = struct { index: usize };
        pub const UnexpectedValueForFlag = struct { index: usize, value_span: Span };
        pub const EmptyArgument = struct { argv_index: usize };
    };
};

pub fn init(args: Args) !Lexer {
    const len = args.len;
    if (len > 1) {
        for (1..len) |i| {
            const arg = args.get(i);
            if (!std.unicode.utf8ValidateSlice(arg))
                return error.InvalidUtf8;
        }
    }

    return .{ .args = args };
}

pub fn nextToken(lexer: *Lexer, comptime flags: []const Flag, comptime word_mode: WordMode) ?Token {
    if (lexer.argi >= lexer.args.len)
        return null;

    if (lexer.subargi) |subargi| {
        if (subargi < lexer.args.get(lexer.argi).len)
            return lexer.shortChain(flags, lexer.args.get(lexer.argi), subargi)
        else if (lexer.argi + 1 >= lexer.args.len)
            return null
        else
            lexer.argi += 1;
    }

    const arg = lexer.args.get(lexer.argi);
    if (lexer.found_force_stop) {
        defer lexer.argi += 1;
        return .{ .positional = .{ .span = .{ .argv_index = lexer.argi, .start = 0, .end = arg.len } } };
    }

    if (arg.len == 0)
        return .{ .err = .{ .empty_argument = .{ .argv_index = lexer.argi } } };

    switch (arg[0]) {
        '-' => if (arg.len == 1)
            return switch (word_mode) {
                .commands => |cmds| inline for (cmds, 0..) |cmd, i| {
                    if (std.mem.eql(u8, cmd.cmd, "-")) {
                        defer lexer.argi += 1;
                        break .{ .command = .{ .index = i } };
                    }
                } else .{ .err = .{ .unknown_command = .{ .span = .{ .argv_index = lexer.argi, .start = 0, .end = arg.len } } } },
                .positionals => blk: {
                    defer lexer.argi += 1;
                    break :blk .{ .positional = .{ .span = .{ .argv_index = lexer.argi, .start = 0, .end = arg.len } } };
                },
            }
        else switch (arg[1]) {
            '-' => if (arg.len == 2)
                return switch (word_mode) {
                    .commands => |cmds| inline for (cmds, 0..) |cmd, i| {
                        if (std.mem.eql(u8, cmd.cmd, "-")) {
                            defer lexer.argi += 1;
                            break .{ .command = .{ .index = i } };
                        }
                    } else .{ .err = .{ .unknown_command = .{ .span = .{ .argv_index = lexer.argi, .start = 0, .end = arg.len } } } },
                    .positionals => blk: {
                        lexer.found_force_stop = true;
                        defer lexer.argi += 1;
                        break :blk .force_stop;
                    },
                }
            else
                return lexer.longFlag(flags, arg),
            else => return lexer.shortFlag(flags, arg),
        },
        else => return switch (word_mode) {
            .commands => |cmds| inline for (cmds, 0..) |cmd, i| {
                if (std.mem.eql(u8, cmd.cmd, arg)) {
                    defer lexer.argi += 1;
                    break .{ .command = .{ .index = i } };
                }
            } else .{ .err = .{ .unknown_command = .{ .span = .{ .argv_index = lexer.argi, .start = 0, .end = arg.len } } } },
            .positionals => blk: {
                defer lexer.argi += 1;
                break :blk .{ .positional = .{ .span = .{ .argv_index = lexer.argi, .start = 0, .end = arg.len } } };
            },
        },
    }
}

pub fn longFlag(lexer: *Lexer, comptime flags: []const Flag, arg: []const u8) Token {
    const eq_index = std.mem.indexOfScalar(u8, arg, '=');
    const flag_end = eq_index orelse arg.len;
    return inline for (flags, 0..) |flag, i| {
        const long = flag.long orelse continue;
        if (std.mem.eql(u8, long, arg[2..flag_end])) {
            if (flag.type == void or flag.type == argz.FlagHelp) {
                if (eq_index) |idx|
                    break .{ .err = .{ .unexpected_value_for_long_flag = .{ .index = i, .value_span = .{ .argv_index = lexer.argi, .start = idx + 1, .end = arg.len } } } };
                lexer.argi += 1;
                break .{ .long_flag = .{ .index = i } };
            } else if (@typeInfo(flag.type) == .optional) {
                if (eq_index) |idx| {
                    if (idx + 1 == arg.len)
                        break .{ .err = .{ .missing_value_for_long_flag = .{ .index = i } } };
                    defer lexer.argi += 1;
                    break .{ .long_flag_with_value = .{ .index = i, .value_span = .{ .argv_index = lexer.argi, .start = idx + 1, .end = arg.len } } };
                }
                defer lexer.argi += 1;
                break .{ .long_flag = .{ .index = i } };
            } else {
                if (eq_index) |idx| {
                    if (idx + 1 == arg.len)
                        break .{ .err = .{ .missing_value_for_long_flag = .{ .index = i } } };
                    defer lexer.argi += 1;
                    break .{ .long_flag_with_value = .{ .index = i, .value_span = .{ .argv_index = lexer.argi, .start = idx + 1, .end = arg.len } } };
                } else {
                    if (lexer.argi + 1 == lexer.args.len) {
                        break .{ .err = .{ .expected_value_for_long_flag = .{ .index = i } } };
                    } else {
                        defer lexer.argi += 2;
                        break .{ .long_flag_with_value = .{ .index = i, .value_span = .{ .argv_index = lexer.argi + 1, .start = 0, .end = lexer.args.get(lexer.argi + 1).len } } };
                    }
                }
            }
        }
    } else .{ .err = .{ .unknown_long_flag = .{ .span = .{ .argv_index = lexer.argi, .start = 2, .end = flag_end } } } };
}

pub fn shortFlag(lexer: *Lexer, comptime flags: []const Flag, arg: []const u8) Token {
    const char_len, const char = decodeCharAtArgPos(arg, 1);
    return inline for (flags, 0..) |flag, i| {
        if (flag.short == char) {
            if (flag.type == void or flag.type == argz.FlagHelp) {
                lexer.subargi = char_len + 1;
                break .{ .short_flag = .{ .index = i } };
            } else if (@typeInfo(flag.type) == .optional) {
                if (char_len + 1 < arg.len and arg[char_len + 1] == '=') {
                    if (char_len + 2 == arg.len) {
                        break .{ .err = Token.Error{ .missing_value_for_short_flag = .{ .index = i } } };
                    }
                    defer lexer.argi += 1;
                    break .{ .short_flag_with_value = Token.FlagWithValue{ .index = i, .value_span = .{ .argv_index = lexer.argi, .start = char_len + 1, .end = arg.len } } };
                } else {
                    defer lexer.subargi = char_len + 1;
                    break .{ .short_flag = Token.Flag{ .index = i } };
                }
            } else {
                if (char_len + 1 == arg.len) {
                    break if (lexer.argi + 1 == lexer.args.len)
                        .{ .err = .{ .expected_value_for_short_flag = .{
                            .index = i,
                        } } }
                    else blk: {
                        lexer.argi += 2;
                        lexer.subargi = null;
                        break :blk .{ .short_flag_with_value = .{ .index = i, .value_span = .{ .argv_index = lexer.argi - 1, .start = 0, .end = lexer.args.get(lexer.argi - 1).len } } };
                    };
                } else {
                    defer {
                        lexer.argi += 1;
                        lexer.subargi = null;
                    }
                    break .{ .short_flag_with_value = .{ .index = i, .value_span = .{ .argv_index = lexer.argi, .start = char_len + 1, .end = arg.len } } };
                }
            }
        }
    } else .{ .err = .{ .unknown_short_flag = .{ .span = .{ .argv_index = lexer.argi, .start = 1, .end = char_len } } } };
}

pub fn shortChain(lexer: *Lexer, comptime flags: []const Flag, arg: []const u8, subargi: usize) Token {
    const char_len, const char = decodeCharAtArgPos(arg, subargi);
    return inline for (flags, 0..) |flag, i| {
        if (flag.short == char) {
            if (flag.type == void or flag.type == argz.FlagHelp) {
                lexer.subargi = subargi + char_len + 1;
                break .{ .short_flag = .{ .index = i } };
            } else if (@typeInfo(flag.type) == .optional) {
                if (subargi + char_len + 1 < arg.len and arg[subargi + char_len + 1] == '=') {
                    if (subargi + char_len + 2 == arg.len) {
                        break .{ .err = Token.Error{ .missing_value_for_short_flag = .{ .index = i } } };
                    }
                    defer {
                        lexer.argi += 1;
                        lexer.subargi = null;
                    }
                    break .{ .short_flag_with_value = Token.FlagWithValue{ .index = i, .value_span = .{ .argv_index = lexer.argi, .start = subargi + char_len + 1, .end = arg.len } } };
                } else {
                    defer lexer.subargi = subargi + char_len + 1;
                    break .{ .short_flag = Token.Flag{ .index = i } };
                }
            } else {
                if (subargi + char_len == arg.len) {
                    break if (lexer.argi + 1 == lexer.args.len)
                        .{ .err = .{ .expected_value_for_short_flag = .{
                            .index = i,
                        } } }
                    else blk: {
                        lexer.argi += 2;
                        lexer.subargi = null;
                        break :blk .{ .short_flag_with_value = .{ .index = i, .value_span = .{ .argv_index = lexer.argi - 1, .start = 0, .end = lexer.args.get(lexer.argi).len } } };
                    };
                } else {
                    defer {
                        lexer.argi += 1;
                        lexer.subargi = null;
                    }
                    break .{ .short_flag_with_value = .{ .index = i, .value_span = .{ .argv_index = lexer.argi, .start = subargi + char_len, .end = arg.len } } };
                }
            }
        }
    } else .{ .err = .{ .unknown_short_flag = .{ .span = .{ .argv_index = lexer.argi, .start = subargi, .end = subargi + char_len } } } };
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

test Lexer {
    const builtin = @import("builtin");
    if (!builtin.link_libc) {
        switch (builtin.os.tag) {
            .windows, .wasi => return error.SkipZigTest,
            else => {},
        }
    }
    const flags = &[_]Flag{
        .{ .long = "flag1", .type = void },
        .{ .long = "flag2", .type = []const u8 },
        .{ .short = 's', .type = ?u8 },
        .{ .short = 'a', .type = void },
        .{ .short = 'h', .type = ?u16 },
    };
    const argv: []const [:0]const u8 = &.{
        "\xff\x21not valid UTF8\x12\xff",
        "--flag1",
        "--flag2=bar",
        "-ash=123",
        "--flag2",
        "quux",
        "positional0",
        "positional1",
        "--",
        "",
        "the previous argument was empty",
        "--actuallyapositional",
        "-thistoo",
    };
    var sys_args = @import("args.zig").OwnedArgs.init(argv);
    const args = sys_args.args();
    var lexer = try Lexer.init(args);
    while (lexer.nextToken(flags, .positionals)) |tok| {
        std.log.err("{any}\n", .{tok});
    }
}
