const std = @import("std");
const argz = @import("argz");

const config: argz.Config = .{
    .mode = .{ .positionals = &.{} },
    .top_level_flags = &.{
        .init(f64, null, "min", 0, "the minimum value", .{}),
        .init(f64, null, "max", 10, "the maximum value", .{}),
        .init(void, 'i', "integer", {}, "generate integers", .{}),
        .init(void, 'c', "coin-flip", {}, "simulate a coin flip (implies --min=0, --max=1, and -i)", .{}),
        .help,
    },
    .program_name = "random",
    .program_description = "generate a random number",
    .support_allocation = false,
};

pub fn main() !void {
    const argv: argz.SystemArgs = .init();
    var p = try argz.argParser(config, argv.args(), null);
    const opts = try p.parse();

    var xrng: std.Random.DefaultPrng = .init(@bitCast(std.time.milliTimestamp()));
    const rng = xrng.random();

    var stdout = std.io.getStdOut();
    const out = stdout.writer();
    if (opts.flags.@"coin-flip") {
        switch (rng.boolean()) {
            true => try out.writeAll("heads\n"),
            false => try out.writeAll("tails\n"),
        }
        return;
    }
    if (opts.flags.min > opts.flags.max)
        p.fatal("minimum value must be greater than maximum value", .{});
    if (opts.flags.integer) {
        if (@mod(opts.flags.min, 1.0) != 0.0)
            p.fatal("minimum value must be an integer with the -i flag set", .{});
        if (@mod(opts.flags.max, 1.0) != 0.0)
            p.fatal("maximum value must be an integer with the -i flag set", .{});
        const min_int: u64 = @intFromFloat(opts.flags.min);
        const max_int: u64 = @intFromFloat(opts.flags.max);
        try out.print("{d}\n", .{rng.intRangeAtMost(u64, min_int, max_int)});
    } else {
        try out.print("{d}\n", .{rng.float(f64) * (opts.flags.max - opts.flags.min) + opts.flags.min});
    }
}
