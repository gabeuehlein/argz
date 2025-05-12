// args: clone --help
// expected(stdout): Usage: git clone [FLAGS] PATHSPEC
// expected(stdout): FLAGS:
// expected(stdout):     --recurse-submodules recursively clone submodules
// expected(stdout):     --help, -h           display this help

const std = @import("std");
const argz = @import("argz");

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
    _ = opts;
    unreachable;
}
