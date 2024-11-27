const std = @import("std");

const Formatter = std.fmt.Formatter;

const TerminalColor = enum {
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,

    pub fn getForegroundCode(self: @This()) u8 {
        return switch (self) {
            .red => 1,
            .green => 2,
            .yellow => 3,
            .blue => 4,
            .magenta => 5,
            .cyan => 6,
            .white => 7,
        };
    }
};

const TextModifier = enum {
    bold,
    italic,
    underline,

    pub fn getAnsiSetCode(self: @This()) u8 {
        return switch (self) {
            .bold => 1,
            .italic => 3,
            .underline => 4,
        };
    }

    pub fn getAnsiClearCode(self: @This()) u8 {
        return switch (self) {
            .bold => 22,
            .italic => 23,
            .underline => 24,
        };
    }
};

pub fn wrap(data: anytype, use_ansi: bool, color: ?TerminalColor, text_mod: ?TextModifier) Wrapper(@TypeOf(data)) {
    return .{
        .data = data,
        .use_ansi = use_ansi,
        .color = color,
        .text_mod = text_mod,
    };
}

pub fn Wrapper(comptime T: type) type {
    return struct {
        data: T,
        use_ansi: bool,
        color: ?TerminalColor,
        text_mod: ?TextModifier,
    };
}

/// Requires that `@TypeOf(data) == Wrapper(T)` or `@TypeOf(data)` is a superset of `Wrapper(T)` for any `T`.
fn fmtAnsi(data: anytype, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    if (data.use_ansi) {
        if (data.color) |col|
            try writer.print("\x1b[38;5;{d}m", .{col.getForegroundCode()});
        if (data.text_mod) |mod|
            try writer.print("\x1b[{d}m", .{mod.getAnsiSetCode()});
    }
    try std.fmt.format(writer, fmt, .{data.data});
    if (data.use_ansi) {
        if (data.text_mod) |mod|
            try writer.print("\x1b[{d}m", .{mod.getAnsiClearCode()});
        if (data.color != null)
            try writer.writeAll("\x1b[39;5;0m");
    }
}

pub fn ansiFormatter(
    value: anytype,
    emit_ansi_escape_codes: bool,
    comptime color: ?TerminalColor,
    comptime text_mod: ?TextModifier,
) Formatter(fmtAnsi) {
    return .{ .data = wrap(value, emit_ansi_escape_codes, color, text_mod) };
}
