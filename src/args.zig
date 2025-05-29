const std = @import("std");
const builtin = @import("builtin");
const Span = @import("Lexer.zig").Span;

fn emptyArgsGetFn(_: *const anyopaque, _: usize) []const u8 {
    unreachable;
}

pub const Args = struct {
    v_argv_get: *const fn (*const anyopaque, usize) []const u8,
    context: *const anyopaque,
    len: usize,

    pub const empty = Args{
        .v_argv_get = emptyArgsGetFn,
        .context = undefined,
        .len = 0,
    };

    pub fn get(args: Args, index: usize) []const u8 {
        return args.v_argv_get(args.context, index);
    }

    pub fn getSpanText(args: Args, span: Span) []const u8 {
        std.debug.assert(@intFromEnum(span.argv_index) < args.len);
        const arg = span.argv_index.get(args);
        return arg[span.start..span.end];
    }
};

pub const OwnedArgs = struct {
    argv: []const [:0]const u8,

    pub fn init(argv: []const [:0]const u8) OwnedArgs {
        return .{ .argv = argv };
    }

    pub fn args(self: *const OwnedArgs) Args {
        return .{ .v_argv_get = vArgvGet, .context = self, .len = self.argv.len };
    }

    fn vArgvGet(ctx: *const anyopaque, index: usize) []const u8 {
        const me = @as(*const OwnedArgs, @ptrCast(@alignCast(ctx))).*;
        return me.argv[index];
    }
};

// TODO is there a better name for this?
pub const SystemArgs = if (builtin.link_libc) SystemArgsImpl else switch (builtin.os.tag) {
    .windows => @compileError("SystemArgs isn't supported on Windows without libc; use OwnedArgs instead"),
    .wasi => @compileError("SystemArgs isn't supported on WASI without libc; use OwnedArgs instead"),
    else => SystemArgsImpl,
};

pub const SystemArgsImpl = struct {

    pub fn init() Args {
        return .{ .v_argv_get = vArgvGet, .context = undefined, .len = std.os.argv.len };
    }

    fn vArgvGet(_: *const anyopaque, index: usize) []const u8 {
        const arg = std.os.argv[index];
        const len: usize = std.mem.indexOfSentinel(u8, 0, arg);
        return arg[0..len];
    }
};
