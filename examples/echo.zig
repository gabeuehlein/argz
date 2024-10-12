const std = @import("std");
const argz = @import("argz");

const Positional = argz.Positional;
const Flag = argz.Flag;

// zig fmt: off
const cfg = argz.Config{
    .top_level_flags = &[_]Flag{
        .{ .short = 'q', .long = "quiet", .type = i32, .field_name = "quiet", },
        .{ .short = 'b', .long = "bare", .type = void, .field_name = "bare", },
        .{ .short = 'c', .long = "color", .type = ?bool, .field_name = "color" },
        .{ .short = 'W', .long = null, .type = argz.DynamicMulti([]const u8), .field_name = "f_flags" }
//        .{ .short = 'h', .long = "help", .type = argz.FlagHelp, .field_name = "help" }
    },
    .mode = .{ .standard = &[_]Positional{
        Positional{
            .type = []const u8,
            .display = "ARG",
            .help_msg = "the arguments to print",
            .field_name = "args"
        },
    } },
    .support_allocation = true
};
// zig fmt: on

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = false, .retain_metadata = false }){};
    const gpa_allocator = gpa.allocator();
    defer std.debug.assert(!gpa.detectLeaks());
    var arena = std.heap.ArenaAllocator.init(gpa_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var argv = argz.SystemArgs.init();
    var parser = argz.argParser(cfg, argv.args(), allocator) catch unreachable;
    std.debug.print("{any}\n", .{try parser.parse()});
}