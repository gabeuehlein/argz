const ansi = @import("ansi.zig");
const argz = @import("argz.zig");
const builtin = @import("builtin");
const std = @import("std");

pub const Formatter = std.fmt.Formatter;

pub const Color = ansi.TerminalColor;

pub const TextModifier = ansi.TextModifier;

const Flag = argz.Flag;
const Command = argz.Command;
const Mode = argz.Mode;

pub const AllFlagsFormatFn = fn (comptime []const Flag, bool, anytype) anyerror!void;

pub const AllCommandsFormatFn = fn (comptime []const Command, bool, anytype) anyerror!void;

pub const PrologueFormatFn = fn (
    comptime cmd_stack: []const Command,
    comptime current_mode: Mode,
    comptime current_flags: []const Flag,
    use_ansi_escape_codes: bool,
    program_name: []const u8,
    description: ?[]const u8,
    writer: anytype,
) anyerror!void;

pub const CommandFormatFn = fn (
    comptime Command,
    ?Color,
    ?TextModifier,
    bool,
    anytype,
    /// Extra data
    anytype,
) anyerror!void;

pub const FlagFormatFn = fn (
    comptime Flag,
    ?Color,
    ?TextModifier,
    bool,
    anytype,
    /// Extra data
    anytype,
) anyerror!void;

pub fn formatAllFlagsDefault(
    comptime flags: []const Flag,
    emit_ansi_codes: bool,
    writer: anytype,
) @TypeOf(writer).Error!void {
    if (flags.len == 0) return;
    try writer.print("{s}\n", .{ansi.ansiFormatter("FLAGS:", emit_ansi_codes, .green, .bold)});
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
                    (std.unicode.utf8CountCodepoints(flag.typeString(true)) catch unreachable) +
                        @intFromBool(@typeInfo(flag.type == .optional)),
                );
            }
        }
        break :blk .{ max_flag_pad, max_flag_type_pad };
    };
    inline for (flags) |flag| {
        try formatFlagDefault(flag, .green, null, emit_ansi_codes, writer, .{
            .flag_desc_padding = max_flag_pad,
            .max_flag_type_padding = max_flag_type_pad,
        });
    }
}

pub fn formatFlagDefault(
    comptime flag: Flag,
    color: ?Color,
    text_mod: ?TextModifier,
    emit_ansi_codes: bool,
    writer: anytype,
    extra: anytype,
) @TypeOf(writer).Error!void {
    // excludes leading whitespace
    comptime var total_written = 0;
    const a = ansi.ansiFormatter;
    try writer.writeAll(" " ** 4);
    if (flag.long) |long| {
        try writer.print("{s}", .{ansi.ansiFormatter("--" ++ long, emit_ansi_codes, color, text_mod)});
        total_written += 2 + comptime std.unicode.utf8CountCodepoints(long) catch unreachable;
        if (flag.short != null) {
            try writer.writeAll(", ");
            total_written += 2;
        }
    }
    if (flag.short) |short| {
        try writer.print("{s}", .{ansi.ansiFormatter("-" ++ comptime std.unicode.utf8EncodeComptime(short), emit_ansi_codes, color, text_mod)});
        total_written += 2;
    }
    const flag_padding, const flag_type_padding = .{ extra.flag_desc_padding, extra.max_flag_type_padding };
    if (flag.type != void and flag.type != argz.FlagHelp) {
        try writer.writeAll(" " ** (flag_padding - total_written + 1));
        try writer.print("={s}", .{a(if (@typeInfo(flag.type) == .optional)
            "[={s}]"
        else
            "<{s}>", emit_ansi_codes, .cyan, .bold)});
        if (flag.help_msg) |help| {
            try writer.writeAll(" " ** (flag_type_padding - (std.unicode.utf8CountCodepoints(flag.typeString(false)) catch unreachable) + if (@typeInfo(flag.type) == .optional) 0 else 1) ++ help);
        }
    } else if (flag.help_msg) |help| {
        try writer.writeAll(" " ** (flag_padding - total_written + 1) ++ " " ** flag_type_padding ++ help);
    }
    if (flag.type != void and flag.type != argz.FlagHelp) {
        if (flag.default_value) |default| {
            // FIXME: this ugly code is needed to properly coerce things like `enum`s. It can
            // probably be written in a less ugly way.
            var tmp: flag.type = undefined;
            @memcpy(@as(*[1]flag.type, &tmp), @as(*const [1]flag.type, @ptrCast(@alignCast(default))));
            if (@typeInfo(flag.type) == .@"enum")
                try writer.print(" (default '{s}')", .{@tagName(tmp)})
            else
                try writer.print(" (default '{any}')", .{tmp});
        }
    }
    try writer.writeByte('\n');
}

pub fn formatAllCommandsDefault(
    comptime commands: []const Command,
    emit_ansi_codes: bool,
    writer: anytype,
) @TypeOf(writer).Error!void {
    try writer.print("{s}\n", .{ansi.ansiFormatter("COMMAND:", emit_ansi_codes, .green, .bold)});
    if (commands.len == 0) {
        try writer.print("    <no commands available>\n{s} {s}\n", .{
            ansi.ansiFormatter("Note:", emit_ansi_codes, .yellow, .bold),
            \\ Note: this project doesn't provide any valid values for 'COMMAND.' This may be a bug, and
            \\     as such you may want to report this issue to the author(s).
        });
        return;
    }
    const cmd_padding = comptime blk: {
        var max = 0;
        for (commands) |cmd| {
            max = @max(max, std.unicode.utf8CountCodepoints(cmd.cmd) catch unreachable);
        }
        break :blk max;
    };
    inline for (commands) |cmd| {
        try formatCommandDefault(
            cmd,
            .cyan,
            .bold,
            emit_ansi_codes,
            writer,
            .{ .cmd_padding = cmd_padding },
        );
    }
}

pub fn formatCommandDefault(
    comptime cmd: Command,
    color: ?Color,
    text_mod: ?TextModifier,
    emit_ansi_codes: bool,
    writer: anytype,
    /// Extra data
    extra: anytype,
) @TypeOf(writer).Error!void {
    try writer.writeAll(" " ** 4);
    const cmd_padding = extra.cmd_padding;
    try writer.print("{s}", .{ansi.ansiFormatter(cmd.cmd, emit_ansi_codes, color, text_mod)});
    if (cmd.help_msg) |help| {
        try writer.writeAll(" " ** (cmd_padding - (std.unicode.utf8CountCodepoints(cmd.cmd) catch unreachable) + 1) ++ help);
    }
    try writer.writeByte('\n');
}

pub fn formatPrologueDefault(
    comptime cmd_stack: []const Command,
    comptime current_mode: Mode,
    comptime current_flags: []const Flag,
    emit_ansi_codes: bool,
    program_name: []const u8,
    description: ?[]const u8,
    writer: anytype,
) @TypeOf(writer).Error!void {
    if (cmd_stack.len != 0) {
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
    try writer.print("{s} {s}{s}", .{
        ansi.ansiFormatter("Usage", emit_ansi_codes, .green, .bold),
        ansi.ansiFormatter(program_name, emit_ansi_codes, .blue, .bold),
        ansi.ansiFormatter(cmd_string, emit_ansi_codes, .blue, .bold),
    });
    if (current_flags.len != 0)
        try writer.print(" {s}", .{ansi.ansiFormatter("[FLAGS]", emit_ansi_codes, .cyan, .bold)});
    switch (current_mode) {
        .standard => |positionals| {
            comptime var num_optional_positionals = 0;
            inline for (positionals) |positional| {
                if (positional.type == argz.Trailing) break;
                try writer.writeByte(' ');
                if (@typeInfo(positional.type) == .optional) {
                    try writer.writeByte('[');
                    num_optional_positionals += 1;
                }
                try writer.print("{s}", .{ansi.ansiFormatter(positional.displayString(), emit_ansi_codes, .cyan, .bold)});
            }
            try writer.writeAll("]" ** num_optional_positionals);
            if (positionals.len != 0 and positionals[positionals.len - 1].type == argz.Trailing) {
                try writer.print(" {s}", .{ansi.ansiFormatter("[ -- " ++ positionals[positionals.len - 1].displayString() ++ "]", emit_ansi_codes, .cyan, .bold)});
            }
        },
        .commands => try writer.print(" {s}", .{ansi.ansiFormatter("COMMAND", emit_ansi_codes, .cyan, .bold)}),
    }
    try writer.writeByte('\n');
}
