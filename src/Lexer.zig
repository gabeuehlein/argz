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
found_eq: bool = false,
found_force_stop: bool = false,

pub const WordMode = enum {
    positionals,
    commands,
};

pub const TokenTag = enum {
    /// "--" string ('=' string)?
    ///      ^^^^^^
    long_flag,
    /// '-' char ('='? string) | char?+)
    ///     ^^^^
    short_flag,
    /// '-' char [('='? string) | char?+)]
    ///                           ^^^^
    short_chain,
    /// (string flag* command?) | (string [flag | positional | force-stop]*)
    ///  ^^^^^^                    ^^^^^^
    command,
    /// string
    positional,
    /// "--"
    force_stop,
    /// flag '=' string
    ///      ^^^
    flag_eq,
    err,
};

pub const Token = union(TokenTag) {
    long_flag: []const u8,
    short_flag: u21,
    short_chain: u21,
    command: []const u8,
    positional: []const u8,
    force_stop,
    flag_eq,
    err: Error,

    pub const ErrorTag = enum {
        unexpected_force_stop,
        empty_argument,
    };

    pub const Error = union(ErrorTag) {
        unexpected_force_stop,
        empty_argument: usize,
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

pub fn nextToken(lexer: *Lexer, word_mode: WordMode) ?Token {
    if (lexer.argi == lexer.args.len)
        return null;
    const arg = lexer.args.get(lexer.argi);
    if (lexer.subargi != 0) {
        if (lexer.subargi == arg.len) {
            return if (!lexer.loadNextArg())
                null
            else
                // See https://github.com/ziglang/zig/issues/19398 for why this uses `@call` with `.auto`.
                // It should really use `.always_tail` when it doesn't emit a broken LLVM module.
                @call(.auto, nextToken, .{ lexer, word_mode });
        } else {
            const len, const ch = decodeCharAtArgPos(arg, lexer.subargi);
            lexer.subargi += len;
            return if (ch == '=') blk: {
                lexer.found_eq = true;
                break :blk .flag_eq;
            } else .{ .short_chain = ch };
        }
    }
    if (lexer.found_force_stop)
        return .{ .positional = lexer.argument(true) catch unreachable };
    if (arg.len == 0) {
        defer _ = lexer.loadNextArg();
        if (lexer.found_force_stop)
            return .{ .positional = arg };
        return .{ .err = .{ .empty_argument = lexer.argi } };
    } else switch (arg[0]) {
        '-' => {
            if (arg.len != 1) return switch (arg[1]) {
                '-' => return if (arg.len == 2) blk: {
                    defer _ = lexer.loadNextArg();
                    switch (word_mode) {
                        .positionals => {
                            lexer.found_force_stop = true;
                            break :blk .force_stop;
                        },
                        .commands => break :blk .{ .command = arg },
                    }
                } else {
                    const idx = std.mem.indexOfScalar(u8, arg[2..], '=');
                    if (idx) |i| {
                        lexer.subargi = i + 2;
                    } else _ = lexer.loadNextArg();
                    return .{ .long_flag = arg[2..if (idx) |i| 2 + i else arg.len] };
                },
                else => blk: {
                    const len, const ch = decodeCharAtArgPos(arg, 1);
                    lexer.subargi = 1 + len;
                    break :blk .{ .short_flag = ch };
                },
            } else {
                _ = lexer.loadNextArg();
                return switch (word_mode) {
                    .commands => .{ .command = arg },
                    .positionals => .{ .positional = arg },
                };
            }
        },
        else => {
            _ = lexer.loadNextArg();
            return switch (word_mode) {
                .commands => .{ .command = arg },
                .positionals => .{ .positional = arg },
            };
        },
    }
}

pub fn argument(lexer: *Lexer, accept_leading_dash: bool) ![]const u8 {
    var arg = lexer.currentArg() orelse return error.MissingArgument;
    if (lexer.subargi != 0) {
        if (lexer.subargi >= arg.len) {
            if (!lexer.loadNextArg())
                return error.MissingArgument
            else
                arg = lexer.currentArg().?;
        } else {
            defer _ = lexer.loadNextArg();
            return arg[lexer.subargi..];
        }
    }
    if (arg.len == 0) {
        if (lexer.found_force_stop) {
            _ = lexer.loadNextArg();
            return arg;
        } else return error.EmptyArgument;
    } else return switch (arg[0]) {
        '-' => if (lexer.found_force_stop or accept_leading_dash) blk: {
            _ = lexer.loadNextArg();
            break :blk arg;
        } else return error.ExpectedArgument,
        else => blk: {
            _ = lexer.loadNextArg();
            break :blk arg;
        },
    };
}

pub fn peek(lexer: *const Lexer, word_mode: WordMode) ?Token {
    var copy = lexer.*;
    return copy.nextToken(word_mode);
}

pub fn maybe(lexer: *Lexer, tag: TokenTag, word_mode: WordMode) ?Token {
    var copy = lexer.*;
    const tok = copy.nextToken(word_mode) orelse return null;
    if (tok == tag) {
        lexer.* = copy;
        return tok;
    }
    return null;
}

fn loadNextArg(lexer: *Lexer) bool {
    lexer.subargi = 0;
    lexer.found_eq = false;
    if (lexer.argi >= lexer.args.len)
        return false;
    lexer.argi += 1;
    return lexer.argi != lexer.args.len;
}

fn peekChar(lexer: *const Lexer) ?u21 {
    return if (lexer.argi >= lexer.args.len or lexer.subargi >= lexer.args.get(lexer.argi).len)
        null
    else
        decodeCharAtArgPos(lexer.args.get(lexer.argi), lexer.subargi)[1];
}

fn maybeChar(lexer: *Lexer, char: u21) bool {
    const arg = lexer.currentArg() orelse return false;
    if (lexer.subargi >= arg.len) return false;

    const char_len, const next_char = decodeCharAtArgPos(arg, lexer.subargi);
    if (next_char == char) {
        lexer.subargi += char_len;
        return true;
    } else return false;
}

fn currentArg(lexer: *const Lexer) ?[]const u8 {
    return if (lexer.argi >= lexer.args.len) null else lexer.args.get(lexer.argi);
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
    const argv: []const [:0]const u8 = &.{
        "program",
        "build",
        "--bar=foo",
        "foobar",
        "-qwerty=99",
        "",
        "--",
        "--these",
        "-are",
        "--not",
        "",
        "--flags",
    };
    var sys_args = argz.OwnedArgs.init(argv);
    const args = sys_args.args();
    var lexer = try Lexer.init(args);
    while (lexer.nextToken(.positionals)) |tok| {
        std.log.err("{any}", .{tok});
    }
}
