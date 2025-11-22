// args: --long --two --long
// expect-exit-code: 1
// expected(stderr): error: option '--long' found multiple times

const std = @import("std");
const argz = @import("argz");

pub const MyCli = struct {
    options: struct {
        long: bool,
        two: bool,
    },
};

pub const config = struct {
    pub const options = struct {
        pub const info_messages = struct {
            pub const long = "a long option";
            pub const two = "option two";
        };
    };
};

pub fn main() !void {
    var simple: argz.Parser.Interface.Simple = try .init(.system());

    var parser: argz.Parser = .init(simple.interface(), .{ .program_name = "duplicate-long-flag" });
    const opts = parser.parse(MyCli, config) catch std.process.exit(1);
    _ = opts;
    unreachable;
}
