const std = @import("std");

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

pub fn AnsiFormatter(comptime T: type, comptime fmt: []const u8, comptime color: ?TerminalColor, comptime text_mod: ?TextModifier) type {
    return struct {
        inner: T,
        enable: bool,

        pub fn format(f: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
            if (f.enable) {
                if (color) |col| {
                    try writer.print("\x1b[38;5;{d}m", .{col.getForegroundCode()});
                }
                if (text_mod) |mod| {
                    try writer.print("\x1b[{d}m", .{mod.getAnsiSetCode()});
                }
            }
            try writer.print(fmt, f.inner);
            if (f.enable) {
                if (text_mod) |mod| {
                    try writer.print("\x1b[{d}m", .{mod.getAnsiClearCode()});
                }
                if (color != null) {
                    try writer.writeAll("\x1b[39;5;0m");
                }
            }
        }
    };
}

test AnsiFormatter {
    const q = AnsiFormatter(u32, "{x}", .red, .bold){ .enable = true, .inner = 65535 };
    var buf = @as([128]u8, undefined);
    const b = std.fmt.bufPrint(&buf, "{}", .{q}) catch unreachable;
    try std.testing.expectEqualStrings("\x1b[38;5;1m\x1b[1mffff\x1b[22m\x1b[39;5;0m", b);
}
