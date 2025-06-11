const std = @import("std");
const argz = @import("argz");

const cfg: argz.Config = .{
    .top_level_flags = &.{},
    .mode = .{ .positionals = &.{} },
    .support_allocation = false,
};

pub fn main() !void {
    var arg_parser: argz.Parser = try .init(argz.SystemArgs.init(), .{});
    const opts = try arg_parser.parse(cfg);
    _ = opts;
}
