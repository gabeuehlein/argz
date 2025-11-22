// args: "an argument"
// expected(stdout): an argument

const std = @import("std");
const argz = @import("argz");

pub const MyCli = struct {
    positionals: struct {
        blah: []const u8,
    },
};

pub fn main() !void {
    var simple: argz.Parser.Interface.Simple = try .init(.system());

    var parser: argz.Parser = .init(simple.interface(), .{ .program_name = "duplicate-long-flag" });
    const opts: MyCli = try parser.parse(MyCli, struct {});
    var stdout: std.fs.File = .stdout();
    try stdout.writeAll(opts.positionals.blah);
}
