const std = @import("std");
const util = @import("util.zig");
const Config = @import("root.zig").Config;

pub const Args = struct {
    ctx: *anyopaque,
    next_arg_ptr: *const fn (*anyopaque) ?[]const u8,

    pub fn nextArg(self: *@This()) ?[]const u8 {
        return self.next_arg_ptr(self.ctx);
    }
};

pub const OwnedArgs = struct {
    argv: []const [:0]const u8,
    index: usize = 0,

    pub fn init(argv: []const [:0]const u8) OwnedArgs {
        return .{ .argv = argv, .index = 0 };
    }

    fn nextArgFn(ptr: *anyopaque) ?[]const u8 {
        var self = @as(*@This(), @ptrCast(@alignCast(ptr)));
        if (self.index >= self.argv.len)
            return null;

        const arg = self.argv[self.index];
        self.index += 1;

        return arg;
    }

    pub fn args(self: *@This()) Args {
        return .{ .ctx = @ptrCast(self), .next_arg_ptr = nextArgFn };
    }
};

/// Represents an iterator over `std.os.argv`. Only supported on systems
/// that support it.
pub const SystemArgs = struct {
    argv: []const [*:0]const u8,
    index: usize = 0,

    pub fn init() SystemArgs {
        return .{ .argv = std.os.argv, .index = 0 };
    }

    fn nextArgFn(ptr: *anyopaque) ?[]const u8 {
        var self = @as(*@This(), @ptrCast(@alignCast(ptr)));
        if (self.index >= self.argv.len)
            return null;

        const arg = self.argv[self.index];
        self.index += 1;

        var len = @as(usize, 0);
        // Manual implementation of `strlen`. Probably not worth
        // optimizing because command line arguments are typically shorter
        // than twice a vector register's width, and aligning things is fairly expensive.
        while (arg[len] != 0) : (len += 1) {}

        return arg[0..len];
    }

    pub fn args(self: *@This()) Args {
        return .{ .ctx = @ptrCast(self), .next_arg_ptr = nextArgFn };
    }
};

pub const TokenTag = enum {
    short,
    long,
    equals,
    new_arg,
    other,
    force_end,
    pub fn stringRepr(self: @This()) []const u8 {
        return switch (self) {
            .short => "short flag",
            .long => "long flag",
            .equals => "equals sign",
            .new_arg => "end of argument",
            .other => "word",
            .force_end => "force stop",
        };
    }
};

pub const TokenKind = union(TokenTag) {
    short: u21,
    long: []const u8,
    /// An `=` byte.
    equals,
    /// The start of a new argument as passed to the program.
    new_arg,
    /// Something that wasn't recognized as a short or long argument. This may
    /// be a command, a positional, a parameter for an option or an error
    other: []const u8,
    force_end,
};

pub fn Tokenizer(comptime cfg: Config) type {
    return struct {
        argv_it: Args,
        argi: usize = 0,
        subargi: usize = 0,
        in_short_chain: bool = false,
        program_name: []const u8,
        current_arg: ?[]const u8 = null,
        current_token: ?TokenKind = null,

        pub const TokenizerError = (if (cfg.force_utf8)
            error{InvalidUtf8Error}
        else
            error{});

        pub fn init(_argv: Args) (TokenizerError || error{NoArguments})!@This() {
            var argv = _argv;
            // don't check this for valid UTF-8 right now, since the program might
            // be located at a path containing invalid UTF-8
            const prog_name = argv.nextArg() orelse return error.NoArguments;
            const first_arg = argv.nextArg();

            if (cfg.force_utf8)
                if (first_arg) |arg|
                    if (!std.unicode.utf8ValidateSlice(arg))
                        return error.InvalidUtf8Error;

            return .{ .program_name = prog_name, .argv_it = argv, .current_arg = first_arg };
        }

        pub fn restOfCurrentToken(self: *@This()) ?[]const u8 {
            return if (self.current_arg) |carg|
                if (self.subargi >= carg.len)
                    null
                else
                    carg[self.subargi..]
            else
                null;
        }

        pub fn peek(self: *@This()) ?TokenKind {
            if (self.current_token == null)
                self.current_token = self.nextToken();

            return self.current_token;
        }

        pub fn nextToken(self: *@This()) ?TokenKind {
            if (self.current_token) |tok| {
                self.current_token = null;
                return tok;
            }

            const current_arg = self.current_arg orelse return null;
            if (self.subargi >= current_arg.len) {
                _ = self.nextArg(false);
                return .new_arg;
            } else if (current_arg[self.subargi] == '=') {
                self.subargi += 1;
                self.in_short_chain = false;
                return .equals;
            } else if (self.in_short_chain) {
                return self.shortArg();
            } else if (self.subargi == 0) {
                if (current_arg.len == 0) {
                    defer _ = self.nextArg(false);
                    return .{ .other = current_arg };
                }

                if (current_arg.len == 1) {
                    defer _ = self.nextArg(true);
                    return .{ .other = current_arg };
                }

                if (current_arg[0] == '-') {
                    if (current_arg[1] == '-') {
                        if (current_arg.len == 2) {
                            defer _ = self.nextArg(true);
                            return .{ .force_end = {} };
                        } else if (std.mem.indexOfScalar(u8, current_arg, '=')) |idx| {
                            const arg = current_arg[2..idx];
                            self.subargi = idx;
                            return .{ .long = arg };
                        } else {
                            self.moveCursorToEnd();
                            return .{ .long = current_arg[2..] };
                        }
                    }

                    self.subargi += 1;
                    return self.shortArg();
                } else {
                    defer _ = self.nextArg(true);
                    return .{ .other = current_arg };
                }
            } else {
                defer _ = self.nextArg(true);
                return .{ .other = current_arg[self.subargi..] };
            }
        }

        /// Forces a short-form option to be parsed, regardless of the tokenizer's current state.
        /// Requires there to *not* be a lookahead token currently in the queue and for there to
        /// be characters left to read in the current argument, if it isn't `null`.
        pub fn shortArg(self: *@This()) ?TokenKind {
            util.runtimeCheck(self.current_token == null, "shortArg() called with peeked value still queued", .{});
            const current_arg = self.current_arg orelse return null;
            const utf8_char_len = std.unicode.utf8ByteSequenceLength(current_arg[self.subargi]) catch {
                if (cfg.force_utf8) {
                    // we already checked for valid UTF-8, so we can't
                    // (shouldn't) get here normally
                    unreachable;
                } else {
                    defer _ = self.nextArg(true);
                    return .{ .other = current_arg[self.subargi..] };
                }
            };

            if (cfg.force_utf8) {
                const ch = std.unicode.utf8Decode(current_arg[self.subargi .. self.subargi + utf8_char_len]) catch unreachable;
                self.subargi += utf8_char_len;
                self.in_short_chain = true;
                return .{ .short = ch };
            } else {
                if (self.subargi + utf8_char_len >= current_arg.len) {
                    defer _ = self.nextArg(true);
                    return .{ .other = current_arg[self.subargi..] };
                } else {
                    const ch = std.unicode.utf8Decode(current_arg[self.subargi .. self.subargi + utf8_char_len]) catch {
                        defer _ = self.nextArg(true);
                        return .{ .other = current_arg };
                    };

                    self.in_short_chain = true;
                    self.subargi += utf8_char_len;
                    return .{ .short = ch };
                }
            }
        }

        pub fn eatByteMaybe(self: *@This(), byte: u8) bool {
            if (self.current_arg) |carg| {
                if (self.subargi >= carg.len)
                    return false
                else
                    return carg[self.subargi] == byte;
            } else return false;
        }

        fn moveCursorToEnd(self: *@This()) void {
            if (self.current_arg) |arg|
                self.subargi = arg.len;
        }

        pub fn nextArg(self: *@This(), comptime force: bool) bool {
            if (!force)
                if (self.current_arg) |carg|
                    util.runtimeCheck(self.subargi >= carg.len, "nextArg() called when there was still data to parse at argv index {d}: '{s}'", .{ self.argi, carg[self.subargi..] });

            self.subargi = 0;
            self.argi += 1;
            self.in_short_chain = false;
            self.current_arg = self.argv_it.nextArg();
            return self.current_arg != null;
        }
    };
}
