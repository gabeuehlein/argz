const std = @import("std");
const argz = @import("argz");

const Command = argz.Command;
const Positional = argz.Positional;
const Flag = argz.Flag;

// zig fmt: off
const cfg = argz.Config{
    .top_level_flags = &[_]Flag{
        .{ .short = 'h', .long = "help", .type = argz.FlagHelp, .field_name = "help", .help_msg = "show this help" },
    },
    .mode = .{ .commands = &[_]Command{
        Command{
            .cmd = "clone",
            .info = "clone a repository",
            .mode = .{ .standard = &.{
                Positional{
                    .type = []const u8,
                    .display = "PATHSPEC",
                    .help_msg = "path to the repository",
                    .field_name = "path_spec",
                }
            } },
            .flags = &.{
                Flag{
                    .long = "recurse-submodules",
                    .type = void,
                    .help_msg = "recursively clone submodules",
                    .field_name = "recurse_submodules",
                    .default_value = &@as(bool, false),
                },
                Flag{
                    .long = "help",
                    .short = 'h',
                    .help_msg = "show this help",
                    .field_name = "__help",
                    .type = argz.FlagHelp 
                }
            },
        },
    } },
    .program_name = "git",
    .program_description = "an imitation of the Git SCM tool",
    .support_allocation = true,
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
    const opts = try parser.parse();
    var stdout = std.io.getStdOut();
    try stdout.writer().print("{any}\n", .{opts});
}
