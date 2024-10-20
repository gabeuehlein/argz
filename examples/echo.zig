const std = @import("std");
const argz = @import("argz");

const Positional = argz.Positional;
const Flag = argz.Flag;

// zig fmt: off
const cfg = argz.Config{
    .top_level_flags = &[_]Flag{
        .{ .short = 'h', .long = "help", .type = argz.FlagHelp, .field_name = "help", .help_msg = "show this help" },
    },
    .mode = .{ .standard = &[_]Positional{
        Positional{
            .type = [][]const u8,
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
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = false, .retain_metadata = false }){};
    const gpa_allocator = gpa.allocator();
    defer std.debug.assert(!gpa.detectLeaks());
    var arena = std.heap.ArenaAllocator.init(gpa_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var argv = argz.SystemArgs.init();
    var parser = argz.argParser(cfg, argv.args(), allocator) catch unreachable;
    const opts = try parser.parse();
    var stdout = std.io.getStdOut();
    for (opts.positionals.args) |arg| {
        try stdout.writer().print("{s}\n", .{arg});
    }
}
