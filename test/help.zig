// args: --three --i-need-some-help --two
// expected(stdout): help - test for monitoring 'help' flag consistency
// expected(stdout): 
// expected(stdout): usage: help [options]
// expected(stdout): options:
// expected(stdout):     --long [bool]      a long-form option that has no use
// expected(stdout):     --two [bool]       option number two
// expected(stdout):     -T, --three [bool] three is a prime number
// expected(stdout):     --i-need-some-help display a help message and exit
// expected(stdout):

const std = @import("std");
const argz = @import("argz");

pub const MyCli = struct {
    options: struct {
        long: bool,
        two: bool,
        three: bool,
        i_need_some_help: void,
    },
};

pub const config = struct {
    pub const options = struct {
        pub const help_option = "i_need_some_help";

        pub const short_mappings = struct {
            pub const three = 'T';
        };

        pub const info_messages = struct {
            pub const long = "a long-form option that has no use";
            pub const two = "option number two";
            pub const three = "three is a prime number";
            pub const i_need_some_help = "display a help message and exit";
        };
    };
};

pub fn main() !void {
    var simple: argz.Parser.Interface.Simple = try .init(.system());

    var parser: argz.Parser = .init(simple.interface(), .{
        .program_name = "help",
        .program_description = "test for monitoring 'help' flag consistency",
    });
    const opts = parser.parse(MyCli, config) catch std.process.exit(1);
    _ = opts;
    unreachable;
}
