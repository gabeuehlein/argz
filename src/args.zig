// TODO this module should be the implementation of `Args`, not the container for it.
const std = @import("std");
const builtin = @import("builtin");

fn emptyArgsGetFn(_: *const anyopaque, _: usize) []const u8 {
    unreachable;
}

pub const Args = struct {
    get_fn: *const fn (*const anyopaque, usize) []const u8,
    context: *const anyopaque,
    len: usize,

    pub const empty: Args = .{
        .get_fn = emptyArgsGetFn,
        .context = undefined,
        .len = 0,
    };

    pub fn get(args: Args, index: usize) []const u8 {
        return args.get_fn(args.context, index);
    }
};

pub const OwnedArgs = struct {
    argv: []const [:0]const u8,

    pub fn init(argv: []const [:0]const u8) OwnedArgs {
        return .{ .argv = argv };
    }

    pub fn args(self: *const OwnedArgs) Args {
        return .{ .get_fn = vArgvGet, .context = self, .len = self.argv.len };
    }

    fn vArgvGet(ctx: *const anyopaque, index: usize) []const u8 {
        const me = @as(*const OwnedArgs, @ptrCast(@alignCast(ctx))).*;
        return me.argv[index];
    }
};


pub fn system() Args {
    comptime {
        if (!builtin.link_libc) {
            switch (builtin.os.tag) {
                .windows => @compileError("SystemArgs isn't supported on Windows without libc; use OwnedArgs instead"),
                .wasi => @compileError("SystemArgs isn't supported on WASI without libc; use OwnedArgs instead"),
                else => {},
            }
        }
    }

    return .{
        .get_fn = systemArgsGet,
        .context = undefined,
        .len = std.os.argv.len,
    };
}

pub fn systemArgsGet(_: *const anyopaque, index: usize) []const u8 {
    const arg = std.os.argv[index];
    const len: usize = std.mem.indexOfSentinel(u8, 0, arg);
    return arg[0..len];
}
