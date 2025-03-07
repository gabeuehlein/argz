const std = @import("std");
const argz = @import("argz");
const builtin = @import("builtin");

const config: argz.Config = .{
    .mode = .{ .positionals = &.{
        .init(?[]const u8, "PROGRAM", "path to the program to execute", .{
            .field_name = "program",
        }),
        .init([]const []const u8, "ARG", "the arguments to the program", .{
            .field_name = "arg",
        }),
    } },
    .top_level_flags = &.{
        .init(argz.Multi(argz.Pair([]const u8, []const u8, '='), .dynamic), 's', "set", null, "set environment variable KEY to VAL", .{
            .alt_type_name = "KEY=VAL",
            .field_name = "set_vars",
        }),
        .init(argz.Multi([]const u8, .dynamic), 'u', "unset", null, "unset an environment variable if it exists", .{
            .field_name = "unsets",
        }),
        .init(void, null, "clear", null, "clear the environment", .{}),
        .help,
    },
    .support_allocation = true,
};

var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

pub fn main() !void {
    const gpa, const is_debug = switch (builtin.mode) {
        .Debug, .ReleaseSafe => .{ debug_allocator.allocator(), true },
        .ReleaseFast, .ReleaseSmall => .{ std.heap.smp_allocator, false },
    };
    defer if (is_debug) {
        _ = debug_allocator.deinit();
    };
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    const argv: argz.SystemArgs = .init();
    var p = try argz.Parser.init(argv.args(), .{
        .program_name = "print-env",
        .program_description = "print environment variables or run a program with the given environment",
        .allocator = arena.allocator(),
    });
    const opts = try p.parse(config);

    var env_map = try std.process.getEnvMap(arena.allocator());
    if (opts.flags.clear) {
        var it = env_map.iterator();
        while (it.next()) |entry| {
            env_map.remove(entry.key_ptr.*);
        }
    }
    for (opts.flags.unsets.items) |unset| {
        env_map.remove(unset);
    }
    for (opts.flags.set_vars.items) |def| {
        try env_map.put(def[0], def[1]);
    }
    if (opts.positionals.program) |process| {
        const result = try std.mem.concat(arena.allocator(), []const u8, &.{ &.{process}, opts.positionals.arg });
        return std.process.execve(arena.allocator(), result, &env_map);
    } else {
        var stdout = std.io.getStdOut();
        const w = stdout.writer();
        var it = env_map.iterator();
        while (it.next()) |entry| {
            try w.print("{s}={s}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
        }
    }
}
