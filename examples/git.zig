const std = @import("std");
const argz = @import("argz");
const builtin = @import("builtin");

const cfg: argz.Config = .{
    .top_level_flags = &.{.help},
    .mode = .{ .commands = &.{
        .init(
            "clone",
            &.{
                .init(void, null, "recurse-submodules", null, "recursively clone submodules", .{}),
                .help,
            },
            .{ .positionals = &.{
                .init([]const u8, "PATHSPEC", "path of the repository", .{ .field_name = "path_spec" }),
            } },
            "clone a repository",
            .{},
        ),
        .init(
            "checkout",
            &.{.help},
            .{ .positionals = &.{
                .init([]const u8, "BRANCH", "the branch to checkout", .{
                    .field_name = "branch",
                }),
            } },
            "check out a branch in a repository",
            .{
                .field_name = "branch",
            },
        ),
        .init(
            "help",
            &.{.init(void, 'a', "all", null, "show all help about a topic", .{})},
            .{ .positionals = &.{.init(?[]const u8, "TOPIC", "display help for TOPIC", .{
                .field_name = "topic",
            })} },
            "show help",
            .{},
        ),
    } },
    .support_allocation = false,
};

pub fn main() !void {
    var argv = argz.SystemArgs.init();
    var arg_parser: argz.Parser = try .init(argv.args(), .{
        .program_name = "git",
        .program_description = "an imitation of the Git SCM tool's CLI",
        .allocator = null,
    });
    const opts = try arg_parser.parse(cfg);
    var stdout = std.io.getStdOut();
    try stdout.writer().print("The information parsed is {any}\n", .{opts});
}
