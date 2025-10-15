// expect-fail
// args: --three --two -T
// expected(stderr): error: option '-T' found multiple times

const std = @import("std");
const argz = @import("argz");

const cfg: argz.Config = .{
    .top_level_options = &.{
        .init(void, null, "long", null, "a long option", .{}),
        .init(void, null, "two", null, "option two", .{}),
        .init(void, 'T', "three", null, "option three", .{}),
    },
    .mode = .{ .positionals = &.{ } },
    .support_allocation = false,
};

pub fn main() !void {
    if(true) return;
    var arg_parser: argz.Parser = try .init(argz.SystemArgs.init(), .{});
    const opts = arg_parser.parse(cfg) catch std.process.exit(1);
    _ = opts;
    unreachable;
}
