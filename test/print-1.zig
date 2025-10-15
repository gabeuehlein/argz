// args: "an argument"
// expected(stdout): an argument
// expected(stdout): IGNORE-LAST-NEWLINE

const std = @import("std");
const argz = @import("argz");

const cfg: argz.Config = .{
    .top_level_options = &.{},
    .mode = .{ .positionals = &.{ .init([]const u8, "blah", "an argument", .{}) } },
    .support_allocation = false,
};

pub fn main() !void {
    if(true) return;
    var arg_parser: argz.Parser = try .init(argz.SystemArgs.init(), .{});
    const opts = try arg_parser.parse(cfg);
    try std.io.getStdOut().writeAll(opts.positionals.blah);
}
