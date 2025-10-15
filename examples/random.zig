const std = @import("std");
const argz = @import("argz");

const config: argz.Config = .{
    .mode = .{ .positionals = &.{} },
    .top_level_options = &.{
        .init(f64, 'm', "min", 0, "the minimum value", .{}),
        .init(f64, 'M', "max", 10, "the maximum value", .{}),
        .init(u64, 'q', "quantity", 1, "number of random numbers to generate", .{}),
        .init(void, 'i', "integer", null, "generate integers", .{}),
        .init(void, 'c', "coin-flip", null, "simulate an unbiased coin flip", .{}),
        .init(void, null, "same-line", false, "emit all random numbers on the same line", .{}),
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

    if (opts.options.integer and opts.options.@"coin-flip")
        p.fatal("cannot generate both an integer and a coin flip result simultaneously", .{});

    var xrng: std.Random.DefaultPrng = .init(@bitCast(std.time.milliTimestamp()));
    const rng = xrng.random();

    var stdout = std.io.getStdOut();
    const out = stdout.writer();
    for (0..opts.options.quantity) |i| {
        if (opts.options.@"coin-flip") {
            switch (rng.boolean()) {
                true => try out.writeAll("heads"),
                false => try out.writeAll("tails"),
            }
        } else {
            if (opts.options.min > opts.options.max)
                p.fatal("minimum value must be less than maximum value", .{});
            if (opts.options.integer) {
                if (@mod(opts.options.min, 1.0) != 0.0)
                    p.fatal("minimum value must be an integer with the -i option set", .{});
                if (@mod(opts.options.max, 1.0) != 0.0)
                    p.fatal("maximum value must be an integer with the -i option set", .{});
                const min_int: u64 = @intFromFloat(opts.options.min);
                const max_int: u64 = @intFromFloat(opts.options.max);
                try out.print("{d}", .{rng.intRangeAtMost(u64, min_int, max_int)});
            } else {
                try out.print("{d}", .{rng.float(f64) * (opts.options.max - opts.options.min) + opts.options.min});
            }
        }
        if (opts.options.@"same-line") {
            if (i + 1 != opts.options.quantity)
                try out.writeByte(' ')
            else
                try out.writeByte('\n');
        } else try out.writeByte('\n');
    }
}
