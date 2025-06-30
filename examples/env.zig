const std = @import("std");
const argz = @import("argz");
const builtin = @import("builtin");

const cli = struct {
    pub const flags = [_]argz.Flag{
        .init(argz.Pair([]const u8, []const u8, '=', .start), "set", .{
            .short = 's',
            .long = "set",
            .required = false,
            .repeatable = true,
        }),
        .init([]const u8, "unset", .{
            .short = 'u',
            .long = "unset",
            .required = false,
            .repeatable = true,
        }),
        .init(void, "clear", .{
            .long = "clear",
            .required = false,
        }),
    };

    pub const positionals = [_]argz.Positional{
        .init([]const u8, "program", "PROGRAM", .{
            .required = false,
        }),
        .init([]const u8, "arg", "ARG", .{
            .required = false,
            .repeatable = true,
        }),
    };

    pub const Context = argz.Parser.ParseContext(&flags, &positionals);
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

    var exec_program = false;
    var exec_argv: std.ArrayListUnmanaged([]const u8) = .empty;
    defer exec_argv.deinit(gpa);
    var sets: std.ArrayListUnmanaged(struct { []const u8, []const u8 }) = .empty;
    defer sets.deinit(gpa);
    var unsets: std.ArrayListUnmanaged([]const u8) = .empty;
    defer unsets.deinit(gpa);
    var clear_env = false;

    var p = try argz.Parser.init(argz.SystemArgs.init(), .{
        .program_name = "env",
        .program_description = "print environment variables or run a program with the given environment",
    });
    var context: cli.Context = .{};

    while (try p.nextArg(&cli.flags, &cli.positionals, &context)) |itm| {
        switch (itm) {
            .flag => |flag| switch (flag) {
                .set => |set| try sets.append(gpa, .{ set[0], set[1] }),
                .unset => |unset| try unsets.append(gpa, unset),
                .clear => clear_env = true,
            },
            .positional => |positional| switch (positional) {
                .program => |program| {
                    try exec_argv.append(gpa, program);
                    exec_program = true;
                },
                .arg => |arg| try exec_argv.append(gpa, arg),
            },
        }
    }

    var env_map = if (clear_env)
        std.process.EnvMap.init(gpa)
    else
        try std.process.getEnvMap(gpa);

    defer env_map.deinit();

    for (unsets.items) |unset|
        env_map.remove(unset);
    for (sets.items) |def|
        try env_map.put(def[0], def[1]);
    if (exec_program) {
        return std.process.execve(gpa, exec_argv.items, &env_map);
    } else {
        var stdout = std.io.getStdOut();
        const w = stdout.writer();
        var it = env_map.iterator();
        while (it.next()) |entry| {
            try w.print("{s}={s}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
        }
    }
}
