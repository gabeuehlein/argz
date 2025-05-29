const std = @import("std");
const argz = @import("argz");
const builtin = @import("builtin");

const config: argz.Config = .{
    .top_level_flags = &.{
        .help,
        .init(void, 'E', "stderr", null, "output to standard error instead of standard out", .{}),
    },
    .mode = .{
        .positionals = &.{
            // Note that sentinel-terminated strings are used here. it isn't
            // necessary, but it shows that `argz` will append a sentienl to
            // strings (via dynamic allocation) when requested.
            .init(argz.types.Multi([:0]const u8, .dynamic), "ARG", "the arguments to print", .{
                .field_name = "arg",
            }),
        },
    },
    .support_allocation = true,
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
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    const argv = argz.SystemArgs.init();
    var p = try argz.Parser.init(argv, .{
        .program_name = "echo",
        .program_description = "print the provided arguments to stdout",
        .allocator = arena.allocator(),
    });

    var opts = try p.parse(config);
    defer p.deinit(config, &opts);
    var out = if (opts.flags.stderr)
        std.io.getStdErr()
    else
        std.io.getStdOut();

    for (opts.positionals.arg.items) |arg|
        try out.writer().print("{s}\n", .{arg});
}
