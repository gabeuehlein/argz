// expect-exit-code: 1
// args: --three --two -T
// expected(stderr): error: option '-T' found multiple times

const std = @import("std");
const argz = @import("argz");

pub const MyCli = struct {
    options: struct {
        long: bool,
        two: bool,
        three: bool,
    },
};

pub const config = struct {
    pub const options = struct {
        pub const short_mappings = struct {
            pub const three = 'T';
        };
    };
};

pub fn main() !void {
    var simple: argz.Parser.Interface.Simple = try .init(.system());

    var parser: argz.Parser = .init(simple.interface(), .{ .program_name = "duplicate-flag-short-and-long" });
    const opts = parser.parse(MyCli, config) catch std.process.exit(1);
    _ = opts;
    unreachable;
}
