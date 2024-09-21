const std = @import("std");
const root = @import("root.zig");
const util = @import("util.zig");
const Args = @import("args.zig").Args;

pub const Span = packed struct {
    argv_index: usize,
    start: usize,
    end: usize,

    /// Returns a [std.fmt.Formatter] whose `format` method outputs a English numeric
    /// string pertaining to the `self.argv_index`th number. For example, if `self.argv_index`
    /// is `3`, then the `self.argvIndexFormatter().format(...)`will write `3rd` to the writer.
    pub fn argvIndexFormatter(self: @This()) std.fmt.Formatter(formatArgvIndex) {
        return .{ .data = self.argv_index };
    }

    pub fn len(self: @This()) usize {
        util.runtimeCheck(self.end >= self.start, "span's endpoint is before its start (start = {d}, end = {d})", .{ self.start, self.end });
        return self.end - self.start;
    }

    fn formatArgvIndex(data: usize, comptime _: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        // Even though this is 32 bytes large, we only use 20 bytes at most (18 decimal digits + 2 bytes for the suffix).\
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

// TODO ideally, flag variants should use indexes instead of the raw text representing the flag itself. That way,
// a bunch of string comparisons wouldn't be necessary when figuring out which flag we just found.
pub const Token = union(TokenTag) {
    long_flag: usize,
    long_flag_with_value: struct { index: usize, value_span: Span },
    short_flag: usize,
    short_flag_with_value: struct { index: usize, value_span: Span },
    command: usize,
    positional: usize,
    force_stop: usize,
    err: Error,

    const Error = union(ErrorTag) {
        value_for_flag_with_no_arg: struct {
            flag_span: Span,
            value_span: Span,
        },
        expected_value_for_flag: usize,
        unexpected_positional: usize,
        unknown_command: usize,
        unknown_long_flag: Span,
        unknown_short_flag: Span,
        unexpected_force_stop: usize,
        empty_argument: usize,
        short_flag_invalid_utf8: usize,
    };

    const ErrorTag = enum {
        value_for_flag_with_no_arg,
        expected_value_for_flag,
        unexpected_positional,
        unknown_command,
        unknown_long_flag,
        unknown_short_flag,
        unexpected_force_stop,
        empty_argument,
        short_flag_invalid_utf8,
    };
};

pub const Lexer = struct {
    args: Args,
    argi: usize = 1,
    subargi: ?usize = null,
    positional_index: usize = 0,
    found_force_stop: bool = false,

    pub fn init(args: Args) Lexer {
        return .{
            .args = args,
        };
    }

    pub fn next(lexer: *Lexer, comptime top_level_flags: []const root.Flag, comptime extra_flags: ?[]const root.Flag, comptime mode: root.Mode) ?Token {
        if (lexer.argi >= lexer.args.len) {
            return null;
        } else if (lexer.subargi) |subargi| {
            return lexer.nextShortChain(subargi, top_level_flags, extra_flags, mode);
        } else if (lexer.found_force_stop) {
            util.runtimeCheck(mode == .standard, "lexer in invalid state (handling force stop when mode is command-based)", .{});
            const pos_len = mode.standard.len;
            if (comptime pos_len == 0)
                return .{ .err = .{ .unexpected_positional = lexer.argi } }
            else if (lexer.positional_index < pos_len - 1) {
                defer lexer.argi += 1;
                lexer.positional_index += 1;
                return .{ .positional = lexer.argi };
            } else if (lexer.positional_index == pos_len - 1) {
                defer {
                    if (!util.typeIsDynamic(mode.standard[pos_len - 1].type))
                        lexer.positional_index += 1;
                }
                defer lexer.argi += 1;
                return .{ .positional = lexer.argi };
            } else return .{ .err = .{ .unexpected_positional = lexer.argi } };
        } else {
            const arg = lexer.args.get(lexer.argi);
            if (arg.len == 0)
                return .{ .err = .{ .empty_argument = lexer.argi } }
            else if (arg[0] == '-') {
                if (arg.len == 1) {
                    switch (mode) {
                        .commands => return .{ .err = .{ .unknown_command = lexer.argi } },
                        .standard => |positionals| {
                            const pos_len = positionals.len;
                            if (comptime pos_len == 0)
                                return .{ .err = .{ .unexpected_positional = lexer.argi } }
                            else if (lexer.positional_index < pos_len - 1) {
                                defer lexer.argi += 1;
                                lexer.positional_index += 1;
                                return .{ .positional = lexer.argi };
                            } else if (lexer.positional_index == pos_len - 1) {
                                defer {
                                    if (!util.typeIsDynamic(mode.standard[pos_len - 1].type))
                                        lexer.argi += 1;
                                }
                                return .{ .positional = lexer.argi };
                            } else return .{ .err = .{ .unexpected_positional = lexer.argi } };
                        },
                    }
                } else if (arg[1] == '-') {
                    if (arg.len == 2) {
                        switch (mode) {
                            .commands => return .{ .err = .{ .unexpected_force_stop = lexer.argi } },
                            .standard => {
                                defer lexer.argi += 1;
                                lexer.found_force_stop = true;
                                return .{ .force_stop = lexer.argi };
                            },
                        }
                    } else {
                        return lexer.longFlag(top_level_flags, extra_flags);
                    }
                } else {
                    return lexer.shortFlag(top_level_flags, extra_flags);
                }
            } else {
                switch (mode) {
                    .commands => |commands| {
                        const index = inline for (commands, 0..) |cmd, i| {
                            if (std.mem.eql(u8, arg, cmd.cmd)) {
                                break i;
                            }
                        } else return .{ .err = .{ .unknown_command = lexer.argi } };
                        lexer.argi += 1;
                        return .{ .command = index };
                    },
                    .standard => |positionals| {
                        const index = inline for (0..positionals.len) |i| {
                            if (i == lexer.positional_index)
                                break i;
                        } else return .{ .err = .{ .unexpected_positional = lexer.argi } };
                        if (!util.typeIsDynamic(mode.standard[index].type)) {
                            lexer.positional_index += 1;
                        }
                        lexer.argi += 1;
                        return .{ .positional = index };
                    },
                }
            }
            unreachable;
        }
    }

    fn longFlag(lexer: *Lexer, comptime top_level_flags: []const root.Flag, comptime extra_flags: ?[]const root.Flag) ?Token {
        const arg = lexer.args.get(lexer.argi);
        util.runtimeCheck(arg.len > 2 and std.mem.startsWith(u8, arg, "--"), "tried lexing a long flag that doesn't start with '--'", .{});

        const eq_index = std.mem.indexOfScalar(u8, arg, '=');
        const long = if (eq_index) |idx| arg[2..idx] else arg[2..];

        const all_flags = if (extra_flags) |extra| .{ extra, top_level_flags } else .{top_level_flags};
        comptime var index = 0;
        return top: inline for (all_flags) |flags| {
            inline for (flags) |flag| {
                const flag_long = flag.long orelse continue;

                if (std.mem.eql(u8, flag_long, long)) {
                    switch (@typeInfo(flag.type)) {
                        .Void => {
                            if (eq_index) |idx| {
                                break :top .{ .err = .{ .value_for_flag_with_no_arg = .{ .flag_span = .{ .argv_index = lexer.argi, .start = 0, .end = idx }, .value_span = .{ .argv_index = lexer.argi, .start = idx + 1, .end = arg.len } } } };
                            } else {
                                lexer.argi += 1;
                                break :top .{ .long_flag = index };
                            }
                        },
                        .Optional => {
                            if (eq_index) |idx| {
                                defer lexer.argi += 1;
                                break :top .{ .long_flag_with_value = .{ .index = index, .value_span = .{ .argv_index = lexer.argi, .start = idx + 1, .end = arg.len } } };
                            } else {
                                lexer.argi += 1;
                                break :top .{ .long_flag = index };
                            }
                        },
                        else => {
                            if (eq_index) |idx| {
                                defer lexer.argi += 1;
                                break :top .{ .short_flag_with_value = .{ .index = index, .value_span = .{ .argv_index = lexer.argi, .start = idx + 1, .end = arg.len } } };
                            } else {
                                if (lexer.argi + 1 >= lexer.args.len) {
                                    break :top .{ .err = .{ .expected_value_for_flag = index } };
                                } else {
                                    const next_arg = lexer.args.get(lexer.argi + 1);
                                    if (std.mem.startsWith(u8, next_arg, "--") or (next_arg.len > 1 and next_arg[0] == '-')) {
                                        break :top .{ .err = .{ .expected_value_for_flag = index } };
                                    } else {
                                        defer lexer.argi += 2;
                                        break :top .{ .long_flag_with_value = .{ .index = index, .value_span = .{ .argv_index = lexer.argi + 1, .start = 0, .end = next_arg.len } } };
                                    }
                                }
                            }
                        },
                    }
                }
                index += 1;
            }
        } else .{ .err = .{ .unknown_short_flag = .{ .argv_index = lexer.argi, .start = 1, .end = eq_index orelse arg.len } } };
    }

    fn shortFlag(lexer: *Lexer, comptime top_level_flags: []const root.Flag, comptime extra_flags: ?[]const root.Flag) ?Token {
        const arg = lexer.args.get(lexer.argi);
        util.runtimeCheck(arg.len > 1 and arg[0] == '-', "tried lexing a short flag without a leading '-'", .{});

        const cp_len, const short = switch (lexer.decodeShortFlagOrErr(1, arg)) {
            .err => |e| return .{ .err = e },
            .codepoint => |data| data,
        };

        const all_flags = if (extra_flags) |extra| .{ extra, top_level_flags } else .{top_level_flags};
        comptime var index = 0;
        return top: inline for (all_flags) |flags| {
            inline for (flags) |flag| {
                if (flag.short == short) {
                    switch (@typeInfo(flag.type)) {
                        .Void => {
                            defer lexer.subargi = 1 + cp_len;
                            break :top .{ .short_flag = index };
                        },
                        .Optional => {
                            if (cp_len + 1 < arg.len and arg[cp_len + 1] == '=') {
                                defer lexer.argi += 1;
                                break :top .{ .short_flag_with_value = .{ .index = index, .value_span = .{ .argv_index = lexer.argi, .start = cp_len + 2, .end = arg.len } } };
                            } else {
                                lexer.subargi = cp_len + 1;
                                break :top .{ .short_flag = index };
                            }
                        },
                        else => {
                            if (cp_len + 1 < arg.len and arg[cp_len + 1] == '=') {
                                defer lexer.argi += 1;
                                break :top .{ .short_flag_with_value = .{ .index = index, .value_span = .{ .argv_index = lexer.argi, .start = cp_len + 2, .end = arg.len } } };
                            } else if (cp_len + 1 == arg.len) {
                                if (lexer.argi + 1 >= lexer.args.len) {
                                    break :top .{ .err = .{ .expected_value_for_flag = index } };
                                } else {
                                    const next_arg = lexer.args.get(lexer.argi + 1);
                                    if (std.mem.startsWith(u8, next_arg, "--") or (next_arg.len > 1 and next_arg[0] == '-')) {
                                        break :top .{ .err = .{ .expected_value_for_flag = index } };
                                    } else {
                                        defer lexer.argi += 2;
                                        break :top .{ .short_flag_with_value = .{ .index = index, .value_span = .{ .argv_index = lexer.argi + 1, .start = 0, .end = next_arg.len } } };
                                    }
                                }
                            } else {
                                defer lexer.argi += 1;
                                break :top .{ .short_flag_with_value = .{ .index = index, .value_span = .{ .argv_index = lexer.argi, .start = 1 + cp_len, .end = arg.len } } };
                            }
                        },
                    }
                }
                index += 1;
            }
        } else .{ .err = .{ .unknown_short_flag = .{ .argv_index = lexer.argi, .start = 1, .end = cp_len } } };
    }

    fn nextShortChain(lexer: *Lexer, subargi: usize, comptime top_level_flags: []const root.Flag, comptime extra_flags: ?[]const root.Flag, comptime mode: root.Mode) ?Token {
        const arg = lexer.args.get(lexer.argi);
        if (subargi >= arg.len) {
            lexer.argi += 1;
            lexer.subargi = null;
            // we can't (shouldn't) ever reach this call point again, so
            // there is no risk of deep recursion
            return lexer.next(top_level_flags, extra_flags, mode);
        }
        const cp_len, const short = switch (lexer.decodeShortFlagOrErr(subargi, arg)) {
            .err => |e| return .{ .err = e },
            .codepoint => |data| data,
        };

        const all_flags = if (extra_flags) |extra| .{ extra, top_level_flags } else .{top_level_flags};
        comptime var index = 0;
        return top: inline for (all_flags) |flags| {
            inline for (flags) |flag| {
                if (flag.short == short) {
                    switch (@typeInfo(flag.type)) {
                        .Void => {
                            lexer.subargi = subargi + cp_len;
                            break :top .{ .short_flag = index };
                        },
                        .Optional => {
                            if (subargi + 1 < arg.len and arg[subargi + 1] == '=') {
                                defer lexer.argi += 1;
                                defer lexer.subargi = null;
                                break :top .{ .short_flag_with_value = .{ .index = index, .value_span = .{ .argv_index = lexer.argi, .start = subargi + 2, .end = arg.len } } };
                            } else {
                                lexer.subargi = subargi + cp_len;
                                break :top .{ .short_flag = index };
                            }
                        },
                        else => {
                            if (subargi + 1 < arg.len and arg[subargi + 1] == '=') {
                                defer lexer.argi += 1;
                                break :top .{ .short_flag_with_value = .{ .index = index, .value_span = .{ .argv_index = lexer.argi, .start = subargi + 2, .end = arg.len } } };
                            } else if (subargi + 1 == arg.len) {
                                if (lexer.argi + 1 >= lexer.args.len) {
                                    break :top .{ .err = .{ .expected_value_for_flag = index } };
                                } else {
                                    const next_arg = lexer.args.get(lexer.argi + 1);
                                    if (std.mem.startsWith(u8, next_arg, "--") or (next_arg.len > 1 and next_arg[0] == '-')) {
                                        break :top .{ .err = .{ .expected_value_for_flag = index } };
                                    } else {
                                        defer lexer.argi += 2;
                                        defer lexer.subargi = null;
                                        break :top .{ .short_flag_with_value = .{ .index = index, .value_span = .{ .argv_index = lexer.argi + 1, .start = 0, .end = next_arg.len } } };
                                    }
                                }
                            } else {
                                defer lexer.argi += 1;
                                defer lexer.subargi = null;
                                break :top .{ .short_flag_with_value = .{ .index = index, .value_span = .{ .argv_index = lexer.argi, .start = subargi + cp_len, .end = arg.len } } };
                            }
                        },
                    }
                }
                index += 1;
            }
        } else .{ .err = .{ .unknown_short_flag = .{ .argv_index = lexer.argi, .start = subargi, .end = subargi + cp_len } } };
    }

    fn decodeShortFlagOrErr(lexer: *Lexer, subargi: usize, arg: []const u8) union(enum) { err: Token.Error, codepoint: struct { u3, u21 } } {
        const utf8_len = std.unicode.utf8ByteSequenceLength(arg[subargi]) catch return .{ .err = .{ .short_flag_invalid_utf8 = lexer.argi } };
        if (subargi + utf8_len > arg.len) return .{ .err = .{ .short_flag_invalid_utf8 = lexer.argi } };

        const cp = std.unicode.utf8Decode(arg[subargi .. subargi + utf8_len]) catch return .{ .err = .{ .short_flag_invalid_utf8 = lexer.argi } };
        return .{ .codepoint = .{ utf8_len, cp } };
    }

    pub fn inShortChain(self: *const Lexer) bool {
        return self.subargi != null;
    }
};
