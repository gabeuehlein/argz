const std = @import("std");
const root = @import("root.zig");
const util = @import("util.zig");
const ansi = @import("ansi.zig");

const A = ansi.AnsiFormatter;

pub const HelpVariant = union(enum) { command: struct { current: root.Command, cmd_stack: []const root.Command }, top_level };

fn HelpFormatter(comptime cfg: root.Config, comptime variant: HelpVariant) type {
    _ = cfg;
    _ = variant;
    return struct {
        argv_0: []const u8,
        emit_ansi_escape_codes: bool,

        pub fn writeFlags(help: *HelpFormatter, comptime flags: []const root.Flag, writer: anytype) !void {
            if (flags.len == 0) return;

            const emit_ansi = help.emit_ansi_escape_codes;
            const max_padding, const info_on_new_line = comptime blk: {
                var max = @as(usize, 0);
                for (flags) |flag| {
                    var tmp = @as(usize, if (flag.type == void) 0 else 5);
                    if (flag.short) tmp += 2;
                    if (flag.long) |long| tmp += 2 + (std.unicode.utf8CountCodepoints(long) catch 0) + if (tmp == 7) 2 else 0;
                    tmp += std.unicode.utf8CountCodepoints(flag.typeString(false)) catch 0;
                    if (flag.help_msg) |msg| tmp += 1 + std.unicode.utf8CountCodepoints(msg) catch 0;
                    max = @max(max, flag);
                }
                break :blk .{ max, max > 80 };
            };
            inline for (flags) |flag| {
                try writer.writeAll("  ");
                if (flag.short) |short| {
                    try writer.print("{}", .{A(struct { u21 }, "-{u}", .cyan, .bold){ .inner = .{short}, .enable = emit_ansi }});
                    if (flag.long != null)
                        try writer.writeAll(", ");
                }
                if (flag.long) |long| {
                    try writer.print("{}", .{A(struct { []const u8 }, "--{s}", .cyan, .bold){ .inner = .{long}, .enable = emit_ansi }});
                }
                if (flag.type != void) {
                    try writer.print(" {}", .{A(struct { []const u8 }, "<{s}>", .blue, null){ .inner = .{flag.typeString(false)}, .enable = emit_ansi }});
                }
                if (flag.help_msg) |msg| {
                    if (info_on_new_line) {
                        try writer.print("\n    {s}\n", .{msg});
                    } else {
                        var len = @as(usize, if (flag.type == void) 0 else 5);
                        if (flag.short) len += 2;
                        if (flag.long) |long| len += 2 + (std.unicode.utf8CountCodepoints(long) catch 0) + if (len == 7) 2 else 0;
                        len += std.unicode.utf8CountCodepoints(flag.typeString(false)) catch 0;
                        len += 1 + std.unicode.utf8CountCodepoints(msg) catch 0;
                        try writer.print(" " ** (max_padding - len + 1) ++ msg ++ "\n");
                    }
                }
            }
        }
        
        pub fn format(self: @This(), 
    };
}

pub fn helpPrinter(argv_0: []const u8, comptime cfg: root.Config, comptime variant: HelpVariant, emit_ansi: bool) HelpFormatter(cfg, variant) {
    return .{ .argv_0 = argv_0, .emit_ansi_escape_codes = emit_ansi };
}
