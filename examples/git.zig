const std = @import("std");
const argz = @import("argz");

const Flag = argz.Flag;

pub const flags = &[_]Flag{
    Flag{ .long = "foobar", .help_msg = "blah balah blah" },
    Flag{ .short = 'q', .long = "quux", .help_msg = "ofirwjfojwufjrefureji" },
    Flag{ .short = 's', .long = "qqqqqqqqqqqqqqqqq", },
};

pub fn main() !void {
    var stdout = std.io.getStdOut();
    const w = stdout.writer();

}
