const std = @import("std");
const root = @import("root.zig");
const util = @import("util.zig");
const ansi = @import("ansi.zig");

const T = ansi.T;
const C = ansi.C;

pub const HelpVariant = union(enum) { command: struct { current: root.Command, cmd_stack: []const root.Command }, top_level };

fn HelpFormatter(comptime cfg: root.Config, comptime variant: HelpVariant, comptime enable_ansi: bool) type {
    return struct {
        argv_0: []const u8,

        /// Dumps detailed, newline-separated flag information to the writer provided.
        /// Assumes a newline has already written to the writer. Writes a trailing newline to
        /// the writer.
        fn writeFlagInfo(_: @This(), comptime flags: []const root.Flag, comptime help: root.Help, writer: anytype) !void {
            const flag_padding = blk: {
                comptime var max = 0;
                if (help.short) max = 2;
                if (help.long) max = 6;
                if (help.long and help.short) max = 11;
                inline for (flags) |flag| {
                    comptime var total = if (flag.short != null) 2 else 0;
                    if (flag.long) |long| total += 2 + (comptime std.unicode.utf8CountCodepoints(long) catch 0);
                    if (flag.short != null and flag.long != null) total += 2;
                    max = @max(max, total);
                }
                break :blk max;
            };
            const fmt_with_pad = std.fmt.comptimePrint("{{s:<{d}}}", .{flag_padding});
            if (flags.len != 0 or (help.short or help.long))
                try writer.print("{}\n", .{T(.{C(.{"OPTIONS:"}, "{s}", .blue, enable_ansi)}, "{}", .bold, enable_ansi)});
            if (help.short or help.long) {
                const mode = (@as(u2, @intFromBool(help.long)) << 1) | @intFromBool(help.short);
                const string = switch (mode) {
                    1 => "-s",
                    2 => "--help",
                    3 => "-h, --help",
                    0 => unreachable,
                };
                try writer.print("  {} | show this help\n", .{T(.{C(.{string}, fmt_with_pad, .cyan, enable_ansi)}, "{}", .bold, enable_ansi)});
            }
            inline for (flags) |flag| {
                const mode = (@as(u2, @intFromBool(flag.long != null)) << 1) | @intFromBool(flag.short != null);
                const string = switch (mode) {
                    1 => std.fmt.comptimePrint("-{u}", .{flag.short.?}),
                    2 => std.fmt.comptimePrint("--{s}", .{flag.long.?}),
                    3 => std.fmt.comptimePrint("-{u}, --{s}", .{ flag.short.?, flag.long.? }),
                    0 => unreachable,
                };
                try writer.print("  {} | [{s}]", .{ T(.{C(.{string}, fmt_with_pad, .cyan, enable_ansi)}, "{}", .bold, enable_ansi), flag.alt_type_name orelse util.nameForType(flag.type) });
                if (flag.help_msg) |help_msg| {
                    try writer.print(" {s}\n", .{help_msg});
                } else try writer.writeByte('\n');
            }
        }

        fn writeModeInfo(_: @This(), mode: root.Mode, writer: anytype) !void {
            switch (mode) {
                .commands => |cmds| {
                    try writer.print(" {}", .{T(.{C(.{"COMMAND"}, "{s}", .magenta, enable_ansi)}, "{}", .bold, enable_ansi)});
                    try writer.print("\n{}\n", .{T(.{C(.{"Commands:"}, "{s}", .blue, enable_ansi)}, "{}", .bold, enable_ansi)});
                    const cmd_padding = blk: {
                        comptime var max = 0;
                        if (cfg.help.command) max = 4;
                        inline for (cmds) |cmd0| {
                            max = @max(max, comptime std.unicode.utf8CountCodepoints(cmd0.cmd) catch 0);
                        }
                        break :blk max;
                    };
                    const fmt_with_pad = std.fmt.comptimePrint("{{s:<{d}}}", .{cmd_padding});
                    if (cfg.help.command) {
                        try writer.print("  {} | show this help\n", .{T(.{C(.{"help"}, fmt_with_pad, .cyan, enable_ansi)}, "{}", .bold, enable_ansi)});
                    }
                    inline for (cmds) |cmd0| {
                        if (cmd0.help_msg) |help| {
                            try writer.print("  {} | {s}\n", .{ T(.{C(.{cmd0.cmd}, fmt_with_pad, .cyan, enable_ansi)}, "{}", .bold, enable_ansi), help });
                        } else {
                            try writer.print("  {}\n", .{T(.{C(.{cmd0.cmd}, "{s}", .cyan, enable_ansi)}, "{}", .bold, enable_ansi)});
                        }
                    }
                },
                .standard => |positionals| {
                    try writer.writeByte('\n');
                    inline for (positionals) |positional| {
                        try writer.print(" {}", .{
                            T(.{C(.{positional.display ++ (comptime util.suffixForType(positional.type))}, "{s}", .magenta, enable_ansi)}, "{}", .bold, enable_ansi),
                        });
                    }
                    if (positionals.len != 0)
                        try writer.writeAll(",\nwhere\n");
                    const positional_padding = blk: {
                        comptime var max = 0;
                        inline for (positionals) |positional| {
                            max = @max(max, comptime std.unicode.utf8CountCodepoints(positional.display) catch 0);
                        }
                        break :blk max;
                    };
                    inline for (positionals) |positional| {
                        const fmt_with_pad = std.fmt.comptimePrint("{{s:<{d}}}", .{positional_padding});
                        try writer.print("  {} | [{s}]", .{ T(.{C(.{positional.display}, fmt_with_pad, .cyan, enable_ansi)}, "{}", .bold, enable_ansi), util.nameForType(positional.type) });
                        if (positional.help_msg) |help| {
                            try writer.print(" {s}", .{help});
                        }
                        try writer.writeByte('\n');
                    }
                },
            }
        }

        pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
            switch (variant) {
                .command => |cmd_info| {
                    const cmd = cmd_info.current;
                    const cmd_stack = cmd_info.cmd_stack;
                    const cmds_string = blk: {
                        if (cmd_stack.len == 0)
                            break :blk cmd.cmd;
                        comptime var string = @as([]const u8, "");
                        inline for (cmd_stack) |scmd| {
                            string = string ++ scmd.cmd ++ " ";
                        }
                        break :blk string ++ cmd.cmd;
                    };
                    if (cmd.help_msg) |desc| {
                        try writer.print("`{s} {s}` - {s}\n\n", .{ cfg.program_name orelse self.argv_0, cmds_string, desc });
                    }
                    try writer.print("{} {} {}", .{
                        T(.{C(.{"Usage:"}, "{s}", .blue, enable_ansi)}, "{}", .bold, enable_ansi),
                        T(.{C(.{cfg.program_name orelse self.argv_0}, "{s}", .green, enable_ansi)}, "{}", .bold, enable_ansi),
                        T(.{C(.{cmds_string}, "{s}", .green, enable_ansi)}, "{}", .bold, enable_ansi),
                    });
                    if (cmd.flags.len != 0) {
                        try writer.print(" {}", .{
                            T(.{C(.{"[OPTIONS]"}, "{s}", .magenta, enable_ansi)}, "{}", .bold, enable_ansi),
                        });
                    }
                    try self.writeModeInfo(cmd.mode, writer);
                    try self.writeFlagInfo(cmd.flags, cmd.help, writer);
                },
                .top_level => {
                    if (cfg.program_description) |desc| {
                        try writer.print("`{s}` - {s}\n\n", .{ cfg.program_name orelse self.argv_0, desc });
                    }
                    try writer.print("{} {}", .{
                        T(.{C(.{"Usage:"}, "{s}", .blue, enable_ansi)}, "{}", .bold, enable_ansi),
                        T(.{C(.{cfg.program_name orelse self.argv_0}, "{s}", .green, enable_ansi)}, "{}", .bold, enable_ansi),
                    });
                    if (cfg.top_level_flags.len != 0) {
                        try writer.print(" {}", .{
                            T(.{C(.{"[OPTIONS]"}, "{s}", .magenta, enable_ansi)}, "{}", .bold, enable_ansi),
                        });
                    }
                    try self.writeModeInfo(cfg.mode, writer);
                    try self.writeFlagInfo(cfg.top_level_flags, cfg.help, writer);
                },
            }
        }
    };
}

pub fn HelpPrinter(argv_0: []const u8, comptime cfg: root.Config, comptime variant: HelpVariant, comptime enable_ansi: bool) HelpFormatter(cfg, variant, enable_ansi) {
    return .{ .argv_0 = argv_0 };
}
