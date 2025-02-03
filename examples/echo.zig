const std = @import("std");
const argz = @import("argz");

const Positional = argz.Positional;
const Flag = argz.Flag;

// zig fmt: off
const cfg = argz.Config{
    .top_level_flags = &.{
        .help,
        .init(void, 'e', "stderr", null, "output to standard error instead of standard out", .{}),
    },
    .mode = .{ .positionals = &.{
        Positional{
            .type = [][:0]const u8,
            .display = "ARGS",
            .help_msg = "the arguments to print",
            .field_name = "args"
        },
    } },
    .program_name = "echo",
    .program_description = "print the provided arguments to stdout",
    .support_allocation = true,
};
// zig fmt: on

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var argv = argz.SystemArgs.init();
    var parser = try argz.argParser(cfg, argv.args(), allocator);
    const opts = try parser.parse();
    defer parser.deinitOptions(opts);
    var out = if (opts.flags.stderr)
        std.io.getStdErr()
    else
        std.io.getStdOut();

    for (opts.positionals.args) |arg|
        try out.writer().print("{s}\n", .{arg});
}
