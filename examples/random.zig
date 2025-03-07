const std = @import("std");
const argz = @import("argz");

const config: argz.Config = .{
    .mode = .{ .positionals = &.{} },
    .top_level_flags = &.{
        .init(f64, 'm', "min", 0, "the minimum value", .{}),
        .init(f64, 'M', "max", 10, "the maximum value", .{}),
        .init(u64, 'q', "quantity", 1, "number of random numbers to generate", .{}),
        .init(void, 'i', "integer", null, "generate integers", .{}),
        .init(void, 'c', "coin-flip", null, "simulate an unbiased coin flip", .{}),
        .init(void, null, "same-line", true, "emit all random numbers on the same line", .{}),
        .help,
    },
    .support_allocation = false,
};

pub fn main() !void {
    const argv: argz.SystemArgs = .init();
    var p = try argz.Parser.init(argv.args(), .{
        .program_name = "random",
        .program_description = "generate a random number",
        .allocator = null,
    });
    const opts = try p.parse(config);

    if (opts.flags.integer and opts.flags.@"coin-flip")
        p.fatal("cannot generate both an integer and a coin flip result simultaneously", .{});

    var xrng: std.Random.DefaultPrng = .init(@bitCast(std.time.milliTimestamp()));
    const rng = xrng.random();

    var stdout = std.io.getStdOut();
    const out = stdout.writer();
    for (0..opts.flags.quantity) |i| {
        if (opts.flags.@"coin-flip") {
            switch (rng.boolean()) {
                true => try out.writeAll("heads"),
                false => try out.writeAll("tails"),
            }
        } else {
            if (opts.flags.min > opts.flags.max)
                p.fatal("minimum value must be greater than maximum value", .{});
            if (opts.flags.integer) {
                if (@mod(opts.flags.min, 1.0) != 0.0)
                    p.fatal("minimum value must be an integer with the -i flag set", .{});
                if (@mod(opts.flags.max, 1.0) != 0.0)
                    p.fatal("maximum value must be an integer with the -i flag set", .{});
                const min_int: u64 = @intFromFloat(opts.flags.min);
                const max_int: u64 = @intFromFloat(opts.flags.max);
                try out.print("{d}", .{rng.intRangeAtMost(u64, min_int, max_int)});
            } else {
                try out.print("{d}", .{rng.float(f64) * (opts.flags.max - opts.flags.min) + opts.flags.min});
            }
        }
        if (opts.flags.@"same-line") {
            if (i + 1 != opts.flags.quantity)
                try out.writeByte(' ')
            else
                try out.writeByte('\n');
        } else try out.writeByte('\n');
    }
}
