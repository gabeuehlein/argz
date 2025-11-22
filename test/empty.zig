const std = @import("std");
const argz = @import("argz");

pub fn main() !void {
    var simple: argz.Parser.Interface.Simple = try .init(.system());

    var parser: argz.Parser = .init(simple.interface(), .{ .program_name = "duplicate-short-flag" });
    const opts = parser.parse(struct {}, struct {}) catch std.process.exit(1);
    _ = opts;
}
