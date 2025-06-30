const std = @import("std");
const argz = @import("argz");
const builtin = @import("builtin");

const cli = struct {
    pub const flags = [_]argz.Flag{
        .init(void, "help", .{ .short = 'h', .long = "help", .help_msg = "show this help", .required = false }),
        .init(void, "no_newline", .{ .short = 'n', .help_msg = "separate arguments with a space instead of a newline", .required = false }),
        .init(void, "stderr", .{ .short = 'E', .help_msg = "separate arguments with a space instead of a newline", .required = false }),
    };

    pub const positionals = [1]argz.Positional{
        .init([:0]const u8, "arg", "arg", .{ .repeatable = true, .required = false }),
    };

    pub const Context = argz.Parser.ParseContext(&flags, &positionals);
};

var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

pub fn main() !void {
    const gpa, const is_debug = switch (builtin.mode) {
        .Debug, .ReleaseSafe => .{ debug_allocator.allocator(), true },
        .ReleaseFast, .ReleaseSmall => .{ std.heap.smp_allocator, false },
    };
    defer if (is_debug) {
        _ = debug_allocator.deinit();
    };

    var args = std.ArrayListUnmanaged([:0]const u8).empty;
    defer {
        for (args.items) |arg|
            gpa.free(arg);
        args.deinit(gpa);
    }
    var emit_to_stderr = false;
    var no_newline = false;

    var p = try argz.Parser.init(argz.SystemArgs.init(), .{
        .allocator = gpa,
    });
    var context: cli.Context = .{};
    while (try p.nextArg(&cli.flags, &cli.positionals, &context)) |itm| {
        switch (itm) {
            .flag => |flag| switch (flag) {
                .help => @panic("TODO"),
                .no_newline => no_newline = true,
                .stderr => emit_to_stderr = true,
            },
            .positional => |positional| switch (positional) {
                .arg => |arg| try args.append(gpa, arg),
            },
        }
    }
    try context.checkRequirements(&p);

    var file = if (emit_to_stderr)
        std.io.getStdErr()
    else
        std.io.getStdOut();
    const w = file.writer();
    const char: u8 = if (no_newline) ' ' else '\n';
    for (args.items, 0..) |arg, i| {
        try w.print("{s}", .{arg});
        if (i + 1 != args.items.len)
            try w.writeByte(char);
    }
    try w.writeByte('\n');
}
