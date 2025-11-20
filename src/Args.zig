const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;

const Args = @This();

get_fn: *const fn (*const anyopaque, usize) []const u8,
context: *const anyopaque,
len: usize,

pub const empty: Args = .{
    .get_fn = struct {
        fn get(_: *const anyopaque, _: usize) []const u8 {
            unreachable;
        }
    }.get,
    .context = undefined,
    .len = 0,
};

pub fn get(args: Args, index: usize) []const u8 {
    return args.get_fn(args.context, index);
}

/// An implementation of [Args] that uses a user-provided `argv`. It is assumed
/// that the `argv` will live for at least as long as the `args` implementation
/// when any methods are called on an instance of `Owned`.
pub const Owned = struct {
    argv: []const []const u8,

    pub fn init(argv: []const []const u8) Owned {
        return .{ .argv = argv };
    }

    pub fn args(self: *const Owned) Args {
        return .{
            .get_fn = struct {
                fn get(ctx: *const anyopaque, index: usize) []const u8 {
                    const me = @as(*const Owned, @ptrCast(@alignCast(ctx))).*;
                    return me.argv[index];
                }
            }.get,
            .context = self,
            .len = self.argv.len
        };
    }
};

/// Returns an implementation of `Args` based on `std.os.argv`. This comes with
/// the implication that this function is unsupported on Windows and WASI without
/// linking libc.
///
/// The returned `Args` is not based on any allocations made after `main` is called
/// during startup code. It does, however, require a call to [std.mem.indexOfSentinel
pub fn system() Args {
    comptime {
        if (!builtin.link_libc) {
            switch (builtin.os.tag) {
                .windows => @compileError("Args.system() isn't supported on Windows without libc; use Args.Owned instead"),
                .wasi => @compileError("Args.system() isn't supported on WASI without libc; use Args.Owned instead"),
                else => {},
            }
        }
    }

    return .{
        .get_fn = struct {
            fn get(_: *const anyopaque, index: usize) []const u8 {
                return std.mem.span(std.os.argv[index]);
            }
        }.get,
        .context = undefined,
        .len = std.os.argv.len,
    };
}


