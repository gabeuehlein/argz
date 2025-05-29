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
        .help,
    } },
    .support_allocation = false,
};

pub fn main() !void {
    var arg_parser: argz.Parser = try .init(argz.SystemArgs.init(), .{
        .program_name = "git",
        .program_description = "an imitation of the Git SCM tool's CLI",
        .allocator = null,
        .make_suggestions = true,
    });
    const opts = try arg_parser.parse(cfg);
    _ = opts;
}
