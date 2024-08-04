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

fn TextFormatter(comptime Ty: type, comptime fmt: []const u8, comptime mod: TextModifier, comptime enable: bool) type {
    return struct {
        inner: Ty,

        pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            if (enable) {
                try writer.print("\x1b[{d}m", .{mod.getAnsiSetCode()});
            }
            try writer.print(fmt, self.inner);
            if (enable) {
                try writer.print("\x1b[{d}m", .{mod.getAnsiClearCode()});
            }
        }
    };
}

pub fn T(inner: anytype, comptime fmt: []const u8, comptime mod: TextModifier, comptime enable: bool) TextFormatter(@TypeOf(inner), fmt, mod, enable) {
    return .{ .inner = inner };
}

fn ColorFormatter(comptime Ty: type, comptime fmt: []const u8, comptime color: TerminalColor, comptime enable: bool) type {
    return struct {
        inner: Ty,

        pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            if (enable) {
                try writer.print("\x1b[38;5;{d}m", .{color.getForegroundCode()});
            }
            try writer.print(fmt, self.inner);
            if (enable) {
                try writer.writeAll("\x1b[39;5;0m");
            }
        }
    };
}

pub fn C(inner: anytype, comptime fmt: []const u8, comptime color: TerminalColor, comptime enable: bool) ColorFormatter(@TypeOf(inner), fmt, color, enable) {
    return .{ .inner = inner };
}
