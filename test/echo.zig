// args: foo bar baz ban -- quux "this is a string" --eeeee
// expected(stderr): foo
// expected(stderr): bar
// expected(stderr): baz
// expected(stderr): ban
// expected(stderr): quux
// expected(stderr): this is a string
// expected(stderr): --eeeee

const std = @import("std");
const argz = @import("argz");

const config: argz.Config = .{
    .top_level_flags = &.{
        .help,
        .init(void, 'E', "stderr", null, "output to standard error instead of standard out", .{}),
    },
    .mode = .{
        .positionals = &.{
            .init([][:0]const u8, "ARG", "the arguments to print", .{
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

    const argv = argz.SystemArgs.init();
    var p = try argz.Parser.init(argv.args(), .{
        .program_name = "echo",
        .program_description = "print the provided arguments to stdout",
        .allocator = gpa,
    });

    const opts = try p.parse(config);
    var out = if (opts.flags.stderr)
        std.io.getStdErr()
    else
        std.io.getStdOut();

    for (opts.positionals.arg) |arg|
        try out.writer().print("{s}\n", .{arg});
}
