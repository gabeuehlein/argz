const argz = @import("argz.zig");
const builtin = @import("builtin");
const std = @import("std");

pub const Formatter = std.fmt.Formatter;

pub const Color = std.io.tty.Color;

const Flag = argz.Flag;
const Command = argz.Command;
const Mode = argz.Mode;

pub const AllFlagsFormatFn = fn (
    std.io.tty.Config,
    comptime []const Flag, anytype) anyerror!void;

pub const AllCommandsFormatFn = fn (std.io.tty.Config, comptime []const Command, anytype) anyerror!void;

pub const PrologueFormatFn = fn (
    config: std.io.tty.Config,
    comptime main_cfg: argz.Config,
    comptime cmd_stack: []const Command,
    comptime current_mode: Mode,
    comptime current_flags: []const Flag,
    program_name: []const u8,
    description: ?[]const u8,
    writer: anytype,
) anyerror!void;

pub const ExpandedHelpFormatFn = fn ( 
    config: std.io.tty.Config,
    comptime mode: Mode,
    comptime flags: []const Flag,
    help_category: []const u8,
    help_topic: []const u8,
    writer: anytype,
) anyerror!void;

pub const CommandFormatFn = fn (
    config: std.io.tty.Config,
    comptime cmd: Command,
    writer: anytype,
    /// Extra data
    extra: anytype,
) anyerror!void;

pub const FlagFormatFn = fn (
    config: std.io.tty.Config,
    comptime Flag,
    bool,
    anytype,
    /// Extra data
    anytype,
) anyerror!void;

pub fn formatAllFlagsDefault(
    config: std.io.tty.Config,
    comptime flags: []const Flag,
    writer: anytype,
) @TypeOf(writer).Error!void {
    if (flags.len == 0) return;
    try config.setColor(writer, .green);
    try config.setColor(writer, .bold);
    try writer.writeAll("FLAGS:");
    try config.setColor(writer, .reset);
    try writer.writeByte('\n');
    const max_flag_pad, const max_flag_type_pad = comptime blk: {
        var max_flag_pad = 0;
        var max_flag_type_pad = 0;
        for (flags) |flag| {
            var tmp = 0;
            if (flag.short != null) tmp += 2;
            if (flag.long) |long| tmp += 2 + (std.unicode.utf8CountCodepoints(long) catch unreachable);
            if (flag.short != null and flag.long != null) tmp += 2;
            max_flag_pad = @max(max_flag_pad, tmp);

            if (flag.type != void and flag.type != argz.FlagHelp) {
                max_flag_type_pad = @max(
                    max_flag_type_pad,
                    1 + (std.unicode.utf8CountCodepoints(flag.typeString(false)) catch unreachable) +
                        @intFromBool(@typeInfo(flag.type) == .optional) + 2,
                );
            }
        }
        break :blk .{ max_flag_pad, max_flag_type_pad };
    };
    inline for (flags) |flag| {
        try formatFlagDefault(config, flag, writer, .{
            .flag_desc_padding = max_flag_pad,
            .max_flag_type_padding = max_flag_type_pad,
        });
    }
}

pub fn formatFlagDefault(
    config: std.io.tty.Config,
    comptime flag: Flag,
    writer: anytype,
    extra: anytype,
) @TypeOf(writer).Error!void {
    // excludes leading whitespace
    comptime var total_written = 0;
    try writer.writeAll(" " ** 4);
    if (flag.long) |long| {
        try config.setColor(writer, .green);
        try writer.writeAll("--" ++ long);
        try config.setColor(writer, .reset);
        total_written += 2 + comptime std.unicode.utf8CountCodepoints(long) catch unreachable;
        if (flag.short != null) {
            try writer.writeAll(", ");
            total_written += 2;
        }
    }
    if (flag.short != null) {
        try config.setColor(writer, .green);
        try writer.writeAll(flag.flagString(.short));
        try config.setColor(writer, .reset);
        total_written += 2;
    }
    const flag_padding, const flag_type_padding = .{ extra.flag_desc_padding, extra.max_flag_type_padding };
    if (flag.type != void and flag.type != argz.FlagHelp) {
        try writer.writeAll(" " ** (flag_padding - total_written + 1));
        try config.setColor(writer, .cyan);
        try config.setColor(writer, .bold);
        try writer.writeAll(if (@typeInfo(flag.type) == .optional)
            "[=" ++ comptime flag.typeString(false) ++ "]"
        else
            "<" ++ comptime flag.typeString(false) ++ ">");
        try config.setColor(writer, .reset);
        if (flag.help_msg) |help| {
            try writer.writeAll(" " ** (flag_type_padding - (2 + (std.unicode.utf8CountCodepoints(flag.typeString(false)) catch unreachable) + @intFromBool(@typeInfo(flag.type) == .optional))) ++ help);
        }
    } else if (flag.help_msg) |help| {
        try writer.writeAll(" " ** (flag_padding - total_written + 1) ++ " " ** flag_type_padding ++ help);
    }
    if (flag.type != void and flag.type != argz.FlagHelp) {
        if (flag.defaultValue()) |default| {
            try writer.writeAll(" (default ");
            try formatValue(default, writer);
            try writer.writeByte(')');
        }
    }
    try writer.writeByte('\n');
}

fn formatValue(value: anytype, writer: anytype) !void {
    switch (@typeInfo(@TypeOf(value))) {
        .array => |arr| {
            try writer.writeByte('[');
            for (0.., &value) |i, itm| {
                if (@typeInfo(arr.child) == .optional)
                    if (itm == null)
                        break;
                try formatValue(itm, writer);
                if (i + 1 != arr.len) {
                    try writer.writeAll(", ");
                }
            }
            try writer.writeByte(']');
        },
        .pointer => switch (@TypeOf(value)) {
            []const u8, [:0]const u8 => try writer.print("\"{s}\"", .{value}),
            else => {
                try writer.writeByte('[');
                for (0.., value) |i, itm| {
                    try formatValue(itm, writer);
                    if (i + 1 != value.len) {
                        try writer.writeAll(", ");
                    }
                }
                try writer.writeByte(']');
            },
        },
        .int, .float => try writer.print("{d}", .{value}),
        .@"enum" => try writer.print("{s}", .{@tagName(value)}),
        .optional => if (value) |v| try formatValue(v, writer) else try writer.writeAll("null"),
        else => try writer.print("{any}", .{ value }),
    }
}

pub fn formatAllCommandsDefault(
    config: std.io.tty.Config,
    comptime commands: []const Command,
    writer: anytype,
) @TypeOf(writer).Error!void {
    try config.setColor(writer, .green);
    try config.setColor(writer, .bold);
    try writer.writeAll("COMMAND:\n");
    try config.setColor(writer, .reset);
    if (commands.len == 0) {
        try config.setColor(writer, .green);
        try writer.writeAll("    <no commands available>\n");
        try config.setColor(writer, .yellow);
        try writer.writeAll("Note:");
        try config.setColor(writer, .reset);
        try writer.writeAll(
            \\ Note: this project doesn't provide any valid values for 'COMMAND.' This may be a bug, and
            \\       as such you may want to report this issue to the author(s).
        );
        return;
    } else {
        const cmd_padding = comptime blk: {
            var max = 0;
            for (commands) |cmd| {
                max = @max(max, std.unicode.utf8CountCodepoints(cmd.cmd) catch unreachable);
            }
            break :blk max;
        };
        inline for (commands) |cmd| {
            try formatCommandDefault(
                config,
                cmd,
                writer,
                .{ .cmd_padding = cmd_padding },
            );
        }
    }
}

pub fn formatCommandDefault(
    config: std.io.tty.Config,
    comptime cmd: Command,
    writer: anytype,
    /// Extra data
    extra: anytype,
) @TypeOf(writer).Error!void {
    try writer.writeAll(" " ** 4);
    const cmd_padding = extra.cmd_padding;
    try writer.writeAll(" " ** 4);
    try config.setColor(writer, .green);
    try writer.writeAll(cmd.cmd);
    try config.setColor(writer, .reset);
    if (cmd.help_msg) |help| {
        try writer.writeAll(" " ** (cmd_padding - (std.unicode.utf8CountCodepoints(cmd.cmd) catch unreachable) + 1) ++ help);
    }
    try writer.writeByte('\n');
}

pub fn formatPrologueDefault(
    config: std.io.tty.Config,
    comptime _: argz.Config,
    comptime cmd_stack: []const Command,
    comptime current_mode: Mode,
    comptime current_flags: []const Flag,
    program_name: []const u8,
    description: ?[]const u8,
    writer: anytype,
) @TypeOf(writer).Error!void {
    if (cmd_stack.len == 0) {
        if (description) |desc| {
            try writer.print("{s} - {s}\n\n", .{ program_name, desc });
        }
    }
    const cmd_string = comptime blk: {
        var result = @as([]const u8, "");
        for (cmd_stack) |cmd| {
            result = result ++ " " ++ cmd.cmd;
        }
        break :blk result;
    };
    try config.setColor(writer, .green);
    try config.setColor(writer, .bold);
    try writer.writeAll("Usage:");
    try config.setColor(writer, .reset);
    try writer.writeByte(' ');
    try config.setColor(writer, .blue);
    try config.setColor(writer, .bold);
    try writer.writeAll(program_name);
    try writer.writeAll(cmd_string);
    if (current_flags.len != 0) {
        try writer.writeByte(' ');
        try config.setColor(writer, .cyan);
        try writer.writeAll("[FLAGS]");
    }
    try config.setColor(writer, .reset);
    switch (current_mode) {
        .positionals => |positionals| {
            comptime var num_optional_positionals = 0;
            inline for (positionals) |positional| {
                if (positional.type == argz.Trailing) break;
                try writer.writeByte(' ');
                try config.setColor(writer, .cyan);
                try config.setColor(writer, .bold);
                if (@typeInfo(positional.type) == .optional) {
                    try writer.writeByte('[');
                    num_optional_positionals += 1;
                }
                try writer.writeAll(positional.displayString());
                try config.setColor(writer, .reset);
            }
            try config.setColor(writer, .cyan);
            try config.setColor(writer, .bold);
            try writer.writeAll("]" ** num_optional_positionals);
            try config.setColor(writer, .reset);
            if (positionals.len != 0 and positionals[positionals.len - 1].type == argz.Trailing) {
                try writer.writeByte(' ');
                try config.setColor(writer, .cyan);
                try config.setColor(writer, .bold);
                try writer.writeByte('[');
                try writer.setColor(writer, .reset);
                try writer.writeByte(' ');
                try config.setColor(writer, .cyan);
                try config.setColor(writer, .bold);
                try writer.writeAll("--");
                try writer.setColor(writer, .reset);
                try writer.writeByte(' ');
                try config.setColor(writer, .cyan);
                try config.setColor(writer, .bold);
                try writer.writeAll(positionals[positionals.len - 1].displayString() ++ "]");
                try writer.writeByte(']');
                try config.setColor(writer, .reset);
            }
        },
        .commands => {
            try writer.writeByte(' ');
            try config.setColor(writer, .cyan);
            try config.setColor(writer, .bold);
            try writer.writeAll("COMMAND");
            try config.setColor(writer, .reset);
        },
    }
    try writer.writeByte('\n');
}

pub fn formatExpandedHelpDefault(
    config: std.io.tty.Config,
    comptime mode: Mode,
    comptime flags: []const Flag,
    help_category: []const u8,
    help_topic: []const u8,
    writer: anytype,
) (@TypeOf(writer).Error || error{ DifferentModeActive, NoHelpAvailable, UnknownHelpCategory, UnknownHelpTopic })!void {
    if (help_category.len > 32)
        return error.UnknownHelpCategory;
    const Category = enum { cmd, command, pos, positional, flag };
    var tmp: [32]u8 = undefined;
    for (0.., help_category) |i, chr| {
        tmp[i] = std.ascii.toLower(chr);
    }
    switch (std.meta.stringToEnum(Category, tmp[0..help_category.len]) orelse return error.UnknownHelpCategory) {
        .command, .cmd => switch (mode) {
            .commands => |cmds| {
                inline for (cmds) |cmd| {
                    if (std.mem.eql(u8, cmd.cmd, help_topic)) {
                        try config.setColor(writer, .white);
                        try config.setColor(writer, .bold);
                        try writer.writeAll("Help for command '" ++ cmd.cmd ++ "':");
                        try config.setColor(writer, .reset);
                        const help = cmd.info orelse cmd.help_msg orelse return error.NoHelpAvailable;
                        try writer.writeAll("\n" ++ help ++ "\n");
                        return;
                    }
                }
                return error.UnknownHelpTopic;
            },
            .positionals => return error.DifferentModeActive,
        },
        .positional, .pos => switch (mode) {
            .positionals => |positionals| {
                inline for (positionals) |pos| {
                    if (std.mem.eql(u8, pos.displayString(), help_topic)) {
                        const help = pos.info orelse (pos.help_msg orelse return error.NoHelpAvailable);
                        try config.setColor(writer, .white);
                        try config.setColor(writer, .bold);
                        try writer.writeAll("Help for command '" ++ pos.displayString() ++ "':");
                        try config.setColor(writer, .reset);
                        try writer.writeAll("\n" ++ help ++ "\n");
                        return;
                    }
                }
                return error.UnknownHelpTopic;
            },
            .commands => return error.DifferentModeActive,
        },
        .flag => {
            inline for (flags) |flag| {
                const matches_long = if (flag.long) |long|
                    std.mem.eql(u8, long, help_topic)
                else
                    false;
                const matches_short = if (flag.short) |short|
                    std.mem.eql(u8, &comptime std.unicode.utf8EncodeComptime(short), help_topic)
                else
                    false;
                if (matches_long or matches_short) {
                    const help = flag.info orelse (flag.help_msg orelse return error.NoHelpAvailable);
                    try config.setColor(writer, .white);
                    try config.setColor(writer, .bold);
                    try writer.writeAll("Help for flag '");
                    try writer.writeAll(flag.flagString(if (matches_long) .long else .short));
                    try writer.writeAll("':");
                    try config.setColor(writer, .reset);
                    try writer.writeAll("\n" ++ help ++ "\n");
                    return;
                }
            }
            return error.UnknownHelpTopic;
        },
    }
}
