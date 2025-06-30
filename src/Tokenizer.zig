args: Args,
argi: usize = 1,
/// If this is not `0`, it is assumed that there is still
/// at least part of an argument to be parsed (i.e. `argi < args.len`).
subargi: usize = 0,

pub fn init(args: Args) error{ NoArgs, InvalidUtf8 }!Tokenizer {
    if (args.len == 0)
        return error.NoArgs;

    for (1..args.len) |i|
        if (!unicode.utf8ValidateSlice(args.get(i)))
            return error.InvalidUtf8;

    return .{ .args = args };
}

pub fn next(tokenizer: *Tokenizer) ?Token {
    var arg = tokenizer.currentArg() orelse return null;
    if (tokenizer.subargi != 0) blk: {
        if (tokenizer.subargi == arg.len) {
            if (!tokenizer.advance(.full))
                return null;
            arg = tokenizer.currentArg().?;
            break :blk;
        }

        defer _ = tokenizer.advance(.char);
        const char: u21 = switch (unicode.utf8ByteSequenceLength(arg[tokenizer.subargi]) catch unreachable) {
            1 => arg[tokenizer.subargi],
            2 => unicode.utf8Decode2(arg[tokenizer.subargi..][0..2].*) catch unreachable,
            3 => unicode.utf8Decode3(arg[tokenizer.subargi..][0..3].*) catch unreachable,
            4 => unicode.utf8Decode4(arg[tokenizer.subargi..][0..4].*) catch unreachable,
            else => unreachable,
        };
        return .{ .short_flag = char };
    }
    switch (arg.len) {
        0, 1 => {
            _ = tokenizer.advance(.full);
            return .{ .word = arg };
        },
        2 => if (std.mem.eql(u8, arg, "--"))
            return .stop,
        else => {},
    }
    if (arg[0] == '-') {
        if (arg[1] == '-') {
            defer _ = tokenizer.advance(.full);
            const long = arg[2..];
            const repr: []const u8, const flag_arg: ?[]const u8 = if (std.mem.indexOfScalar(u8, long, '=')) |eq_index|
                .{ long[0..eq_index], long[eq_index + 1 ..] }
            else
                .{ long, null };

            return .{ .long_flag = .{
                .repr = repr,
                .arg = flag_arg,
            } };
        } else {
            tokenizer.subargi = 1 + (unicode.utf8ByteSequenceLength(arg[1]) catch unreachable);
            const char: u21 = switch (unicode.utf8ByteSequenceLength(arg[1]) catch unreachable) {
                1 => arg[1],
                2 => unicode.utf8Decode2(arg[1..][0..2].*) catch unreachable,
                3 => unicode.utf8Decode3(arg[1..][0..3].*) catch unreachable,
                4 => unicode.utf8Decode4(arg[1..][0..4].*) catch unreachable,
                else => unreachable,
            };
            return .{ .short_flag = char };
        }
    } else {
        _ = tokenizer.advance(.full);
        return .{ .word = arg };
    }
}

pub fn argument(tokenizer: *Tokenizer) error{ ExpectedArgument, LeadingDashInArgument }![]const u8 {
    defer _ = tokenizer.advance(.full);
    if (tokenizer.subargi != 0) {
        if (tokenizer.subargi == tokenizer.currentArg().?.len)
            if (!tokenizer.advance(.full))
                return error.ExpectedArgument;

        const arg = tokenizer.currentArg().?[tokenizer.subargi..];

        return arg;
    } else {
        const arg = tokenizer.currentArg() orelse return error.ExpectedArgument;
        if (arg[0] == '-') {
            return error.LeadingDashInArgument;
        } else {
            return arg;
        }
    }
}

pub fn optionalArgument(tokenizer: *Tokenizer) ?[]const u8 {
    const arg = tokenizer.currentArg() orelse return null;
    if (tokenizer.subargi != 0) {
        if (tokenizer.subargi == arg.len)
            return null;
        if (arg[tokenizer.subargi] == '=') {
            defer _ = tokenizer.advance(.full);
            return arg[tokenizer.subargi + 1 ..];
        } else {
            return null;
        }
    } else {
        return null;
    }
}

pub fn skip(tokenizer: *Tokenizer) ?[]const u8 {
    defer _ = tokenizer.advance(.full);
    return (tokenizer.currentArg() orelse return null)[tokenizer.subargi..];
}

fn currentArg(tokenizer: *const Tokenizer) ?[]const u8 {
    return if (tokenizer.argi != tokenizer.args.len)
        tokenizer.args.get(tokenizer.argi)
    else
        null;
}

fn advance(tokenizer: *Tokenizer, comptime by: enum { char, full }) bool {
    state: switch (by) {
        .char => {
            const arg = tokenizer.currentArg() orelse return false;
            if (tokenizer.subargi == arg.len) {
                continue :state .full;
            } else {
                tokenizer.subargi += unicode.utf8ByteSequenceLength(arg[tokenizer.subargi]) catch unreachable;
                return true;
            }
        },
        .full => {
            tokenizer.subargi = 0;
            if (tokenizer.argi != tokenizer.args.len) {
                tokenizer.argi += 1;
                return true;
            }
            return false;
        },
    }
}

const std = @import("std");
const unicode = std.unicode;
const Tokenizer = @This();
const Args = @import("args.zig").Args;

pub const Token = union(enum) {
    long_flag: struct {
        repr: []const u8,
        /// Non-`null` if the argument found was of the form `--repr=arg`.
        arg: ?[]const u8,
    },
    short_flag: u21,
    word: []const u8,
    stop,
};

test Tokenizer {
    const testing = std.testing;
    const OwnedArgs = @import("args.zig").OwnedArgs;
    try testing.expectError(error.NoArgs, Tokenizer.init(Args.empty));

    const arguments = [_][]const []const u8{
        &.{ "program", "--foo", "bar" },
        &.{ "program", "--foo", "bar", "-ab♯f" },
    };
    const expected_tokens = [_][]const Token{
        &.{ .{ .long_flag = .{ .repr = "foo", .arg = null } }, .{ .word = "bar" } },
        &.{
            .{ .long_flag = .{ .repr = "foo", .arg = null } },
            .{ .word = "bar" },
            .{ .short_flag = 'a' },
            .{ .short_flag = 'b' },
            .{ .short_flag = '♯' },
            .{ .short_flag = 'f' },
        },
    };
    inline for (arguments, expected_tokens) |args, tokens| {
        const owned_args = OwnedArgs.init(args);

        var tokenizer = try Tokenizer.init(owned_args.args());
        var i: usize = 0;
        while (tokenizer.next()) |token| : (i += 1) {
            try std.testing.expectEqualDeep(token, tokens[i]);
        }
    }
}
