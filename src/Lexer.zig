//! A generic CLI tokenizer that should be suitable for most applications.
//! Usage of this is not strictly required in an implementation, but it is
//! suggested to use this if implementing a standard CLI parser to avoid
//! differences in behavior between this one and a hand-rolled tokenizer.
//!
//! This tokenizes UNIX style options with GNU extensions (i.e. long options
//! like `--foo=bar`). If support for Windows-style options is needed
//!
//! This tokenizer requires that all arguments are valid UTF-8.

const std = @import("std");
const Args = @import("args.zig").Args;
const argz = @import("argz.zig");

const assert = std.debug.assert;
const Option = argz.Option;
const Positional = argz.Positional;
const Parser = @import("Parser.zig");

const Lexer = @This();

args: Args,
argi: usize = 1,
/// The current position in `args.get(argi)`. A value not equal to zero
/// indicates that we are currently in the middle of lexing a sequence
/// of short options.
subargi: usize = 0,
found_force_stop: bool = false,

const State = enum {
    first_byte,
    one_dash,
    two_dash,
    short_option,
    long_option,
    word,
};

pub fn init(args: Args) error{NoArguments,InvalidUtf8}!Lexer {
    if (args.len == 0)
        return error.NoArguments;
    for (1..args.len) |i| {
        const arg = args.get(i);
        if (!std.unicode.utf8ValidateSlice(arg))
            return error.InvalidUtf8;
    }
    return .{ .args = args };
}

pub fn nextToken(lexer: *Lexer, tokenize_as_word: bool, allow_empty_word: bool, two_dash_is_word: bool) ?Parser.Token {
    if (lexer.argi == lexer.args.len )
        return null
    else if (lexer.subargi == lexer.args.get(lexer.argi).len) {
        _ = lexer.loadNextArg();
        if (lexer.argi == lexer.args.len)
            return null;
    }

    const arg = lexer.args.get(lexer.argi);

    const initial_state: State = if (tokenize_as_word)
        .word
    else if (lexer.subargi != 0)
        .short_option
    else if (arg.len == 0)
        .word
    else
        .first_byte;

    state: switch (initial_state) {
        .first_byte => {
            if (arg[0] == '-') {
                lexer.subargi += 1;
                continue :state .one_dash;
            } else {
                lexer.subargi += 1;
                continue :state .word;
            }
        },
        .one_dash => {
            if (arg.len == 1) {
                return .{ .word = arg };
            } else if (arg[1] == '-') {
                lexer.subargi += 1;
                continue :state .two_dash;
            } else {
                continue :state .short_option;
            }
        },
        .two_dash => {
            if (arg.len == 2) {
                if (two_dash_is_word) {
                    continue :state .word;
                } else {
                    defer _ = lexer.loadNextArg();
                    return .force_stop;
                }
            } else {
                continue :state .long_option;
            }
        },
        .short_option => {
            const char = lexer.nextUtf8Char().?;
            const attached_arg: ?[]const u8 = if (lexer.peekUtf8Char() == '=') blk: {
                defer _ = lexer.loadNextArg();
                lexer.subargi += 1;
                break :blk lexer.currentArg().?[lexer.subargi..];
            } else null;

            return .{ .short_option = .{
                .repr = char,
                .attached_arg = attached_arg,
            } };
        },
        .long_option => {
            _ = lexer.loadNextArg();
            const rest = arg[2..];

            if (std.mem.indexOfScalar(u8, rest, '=')) |index| {
                return .{ .long_option = .{
                    .repr = rest[0..index],
                    .attached_arg = rest[index + 1..],
                } };
            }

            return .{ .long_option = .{
                .repr = rest,
                .attached_arg = null,
            } };
        },
        .word => {
            defer _ = lexer.loadNextArg();
            if (arg.len == 0 and !allow_empty_word)
                return .{ .err = .{ .empty_argument = lexer.argi } };
            return .{ .word = arg }; 
        },
    }
}

pub fn argument(lexer: *Lexer, accept_leading_dash: bool) ?[]const u8 {
    var arg = lexer.currentArg() orelse return null;
    if (lexer.subargi != 0) {
        if (lexer.subargi == arg.len) {
            _ = lexer.loadNextArg();
            return lexer.currentArg() orelse return null;
        } else {
            assert(lexer.subargi < arg.len);
            defer _ = lexer.loadNextArg();
            return arg[lexer.subargi..];
        }
    }

    if (arg.len == 0) {
        _ = lexer.loadNextArg();
        return "";
    } else return switch (arg[0]) {
        '-' => if (lexer.found_force_stop or accept_leading_dash) blk: {
            _ = lexer.loadNextArg();
            break :blk arg;
        } else return null,
        else => blk: {
            _ = lexer.loadNextArg();
            break :blk arg;
        },
    };
}

pub inline fn maybe(lexer: *Lexer, comptime tags: []const Parser.Token.Tag) ?Parser.Token {
    var copy = lexer.*;
    const tok = copy.nextToken() orelse return null;
    inline for (tags) |tag| {
        if (tok == tag) {
            lexer.* = copy;
            return tok;
        }
    }
    return null;
}

fn loadNextArg(lexer: *Lexer) bool {
    lexer.subargi = 0;
    if (lexer.argi >= lexer.args.len)
        return false;
    lexer.argi += 1;
    return lexer.argi != lexer.args.len;
}

fn peekUtf8Char(lexer: *const Lexer) ?u21 {
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

fn nextUtf8Char(lexer: *Lexer) ?u21 {
    const arg = lexer.args.get(lexer.argi);
    if (lexer.subargi == arg.len)
        return null;
    const ret = decodeCharAtArgPos(arg, lexer.subargi);
    lexer.subargi += ret[0];
    return ret[1];
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
        "--options",
    };
    var sys_args = argz.OwnedArgs.init(argv);
    const args = sys_args.args();
    var lexer = try Lexer.init(args);
    var found_force_stop = false;
    while (lexer.nextToken(found_force_stop, true, false)) |tok| {
        std.log.err("{any}", .{tok});
        found_force_stop |= tok == .force_stop;
    }
}
