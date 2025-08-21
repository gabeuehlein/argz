// expect-fail
// args: --three --two -T
// expected(stderr): error: flag '-T' found multiple times

const std = @import("std");
const argz = @import("argz");

const cfg: argz.Config = .{
    .top_level_flags = &.{
        .init(void, null, "long", null, "a long flag", .{}),
        .init(void, null, "two", null, "flag two", .{}),
        .init(void, 'T', "three", null, "flag three", .{}),
    },
    .mode = .{ .positionals = &.{ } },
    .support_allocation = false,
};

pub fn main() !void {
    var arg_parser: argz.Parser = try .init(argz.SystemArgs.init(), .{});
    const opts = arg_parser.parse(cfg) catch std.process.exit(1);
    _ = opts;
    unreachable;
}
