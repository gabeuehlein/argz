const ansi = @import("ansi.zig");
const argz = @import("argz.zig");
const builtin = @import("builtin");
const std = @import("std");

pub const Formatter = std.fmt.Formatter;

pub const Color = ansi.TerminalColor;

pub const TextModifier = ansi.TextModifier;

const Flag = argz.Flag;

pub const WriteFlagsFn = fn (comptime []const Flag, bool, anytype) anyerror!void;

pub const FlagFormatFn = fn (
    comptime Flag,
    comptime []const Flag,
    ?Color,
    ?TextModifier,
    bool,
    anytype,
) anyerror!void;

pub fn writeFlagsDefault(
    comptime flags: []const Flag,
    emit_ansi_codes: bool,
    writer: anytype,
) @TypeOf(writer).Error!void {
    if (flags.len == 0) return;
    try writer.print("{s}\n", .{ansi.ansiFormatter("FLAGS:", emit_ansi_codes, .cyan, .bold)});
    inline for (flags) |flag| {
        try formatFlagDefault(flag, flags, .green, null, emit_ansi_codes, writer);
    }
}

pub fn formatFlagDefault(
    comptime flag: Flag,
    comptime all_flags: []const Flag,
    color: ?Color,
    text_mod: ?TextModifier,
    emit_ansi_codes: bool,
    writer: anytype,
) @TypeOf(writer).Error!void {
    comptime var bytes_written = 4;
    try writer.writeAll(" " ** 4);
    if (flag.long) |long| {
        try writer.print("{s}", .{ansi.ansiFormatter(long, emit_ansi_codes, color, text_mod)});
        bytes_written += comptime std.unicode.utf8CountCodepoints(long) catch unreachable;
        if (flag.short != null) {
            try writer.print(", ");
            bytes_written += 2;
        }
    }
    if (flag.short) |short| {
        try writer.print("-{u}", .{ansi.ansiFormatter(short, emit_ansi_codes, color, text_mod)});
        bytes_written += 2;
    }
    if (flag.help_msg) |help_msg| {
        const padding = comptime defaultGetFlagDescPadding(all_flags);
        try writer.writeAll(" " ** (padding - bytes_written + 1));
        try writer.writeAll(help_msg);
    }
    try writer.writeByte('\n');
}

/// This is a separate function so its result could potentially get interned in a future version of Zig.
fn defaultGetFlagDescPadding(comptime flags: []const Flag) usize {
    comptime var max: usize = 0;
    inline for (flags) |flag| {
        comptime var tmp: usize = 0;
        if (flag.long) |long| {
            tmp = 2 + comptime std.unicode.utf8CountCodepoints(long) catch unreachable;
            if (flag.short != null)
                tmp += 2;
        }
        if (flag.short != null)
            tmp += 2;
        max = @max(max, tmp);
    }
    return max;
}
