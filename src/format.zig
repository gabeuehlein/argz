const argz = @import("argz.zig");
const builtin = @import("builtin");
const std = @import("std");
const types = @import("types.zig");

const Parser = @import("Parser.zig");

pub const Formatter = std.fmt.Formatter;

pub const Color = std.io.tty.Color;

const Flag = argz.Flag;
const Command = argz.Command;
const Mode = argz.Mode;

pub const AllFlagsFormatFn = fn (
    std.io.tty.Config,
    comptime []const Flag, anytype) anyerror!void;

pub const AllCommandsFormatFn = fn (std.io.tty.Config, comptime []const Command, anytype) anyerror!void;

pub const ErrorFormatFn = fn(
    config: std.io.tty.Config,
    err: Parser.Error,
    parser: *Parser,
    writer: std.io.AnyWriter
) anyerror!void;

pub const PrologueFormatFn = fn (
    config: std.io.tty.Config,
    comptime main_cfg: argz.Config,
    comptime cmd_stack: []const Command,
    comptime current_mode: Mode,
    comptime current_flags: []const Flag,
    program_name: []const u8,
    description: ?[]const u8,
    writer: std.io.AnyWriter,
) anyerror!void;

pub const CommandFormatFn = fn (
    config: std.io.tty.Config,
    comptime cmd: Command,
    writer: std.io.AnyWriter,
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

pub const NoCommandFormatFn = fn(
    *Parser,
    found_token: bool,
    comptime top_level_config: argz.Config,
    comptime cmd_stack: []const Command,
    comptime current_mode: Mode,
    comptime current_flags: []const Flag,
) anyerror!void;

pub fn formatAllFlags(
    config: std.io.tty.Config,
    comptime flags: []const Flag,
    writer: std.io.AnyWriter,
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

            if (flagTypeString(flag)) |string| {
                max_flag_type_pad = @max(
                    max_flag_type_pad,
                    1 + (std.unicode.utf8CountCodepoints(string) catch unreachable) +
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
    writer: std.io.AnyWriter,
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
    if (flagTypeString(flag)) |string| {
        try writer.writeAll(" " ** (flag_padding - total_written + 1));
        try config.setColor(writer, .cyan);
        try config.setColor(writer, .bold);
        try writer.writeAll(if (@typeInfo(flag.type) == .optional)
            "[=" ++ string ++ "]"
        else
            "<" ++ string ++ ">");
        try config.setColor(writer, .reset);
        if (flag.help_msg) |help| {
            try writer.writeAll(" " ** (flag_type_padding - (2 + (std.unicode.utf8CountCodepoints(string) catch unreachable) + @intFromBool(@typeInfo(flag.type) == .optional))) ++ help);
        }
    } else if (flag.help_msg) |help| {
        try writer.writeAll(" " ** (flag_padding - total_written + 1) ++ " " ** flag_type_padding ++ help);
    }
    if (types.custom.isCustomType(flag.type, .flag) and types.custom.customTypeOverridesDefaultValueString(flag.type)) {
        if (flag.type.defaultValueString(.flag)) |string| {
            try writer.print(" (default {s})", .{string});
        }
    } else if (flag.type != void) {
        if (flag.defaultValue()) |default| {
            try writer.writeAll(" (default ");
            try formatValue(default, writer);
            try writer.writeByte(')');
        }
    }
    try writer.writeByte('\n');
}

fn formatValue(value: anytype, writer: std.io.AnyWriter) !void {
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
    writer: std.io.AnyWriter,
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
    writer: std.io.AnyWriter,
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
    writer: std.io.AnyWriter,
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
                if (positional.type == types.TrailingPositionals) break;
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
            if (positionals.len != 0 and positionals[positionals.len - 1].type == types.TrailingPositionals) {
                try writer.writeByte(' ');
                try config.setColor(writer, .cyan);
                try config.setColor(writer, .bold);
                try writer.writeByte('[');
                try config.setColor(writer, .reset);
                try writer.writeByte(' ');
                try config.setColor(writer, .cyan);
                try config.setColor(writer, .bold);
                try writer.writeAll("--");
                try config.setColor(writer, .reset);
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

pub fn formatErrorDefault(config: std.io.tty.Config, err: Parser.Error, parser: *Parser, writer: std.io.AnyWriter) anyerror!void {
    switch (err) {
        .expected_arg_for_flag => |data| {
            try emitErr(writer, config, "expected an argument for flag '{s}'", .{data.flag_string});
            if (data.arg_ty_string) |arg_ty_string|
                try emitErrNote(writer, config, "flag '{s}' requires an argument of type '{s}'", .{data.flag_string, arg_ty_string});
        },
        .unexpected_arg_for_flag => |data| {
            try emitErr(writer, config, "unexpected argument '{s}' found for flag '{s}'", .{data.arg_string, data.flag_string});
        },
        .invalid_arg_for_flag => |data| {
            try emitErr(writer, config, "invalid argument '{s}' for flag '{s}'", .{data.arg_string, data.flag_string});
            if (data.arg_ty_string) |arg_ty_string|
                try emitErrNote(writer, config, "flag '{s}' requires an argument of type '{s}'", .{data.flag_string, arg_ty_string});
        },
        .unknown_command => |data| {
            try emitErr(writer, config, "unknown command '{s}'", .{data.found});
            if (parser.make_suggestions) {
                var min_index: usize = 0;
                var dist: u8 = std.math.maxInt(u8);
                for (data.candidates, 0..) |candidate, i| {
                    const new_dist = dlDistance(data.found, candidate); 
                    if (new_dist < dist) {
                        min_index = i;
                        dist = new_dist;
                    }
                }
                if (dist <= data.candidates[min_index].len / 2 + 1) {
                    try emitErrNote(writer, config, "a command with a similar name exists: '{s}'", .{data.candidates[min_index]});
                }
            }
        },
        .unknown_long_flag => |data| {
            try emitErr(writer, config, "unknown flag '--{s}'", .{data.found});
            if (parser.make_suggestions) {
                var min_index: usize = 0;
                var dist: u8 = std.math.maxInt(u8);
                for (data.candidates, 0..) |candidate, i| {
                    const new_dist = dlDistance(data.found, candidate); 
                    if (new_dist < dist) {
                        min_index = i;
                        dist = new_dist;
                    }
                }
                if (dist <= data.candidates[min_index].len / 2) {
                    try emitErrNote(writer, config, "a flag with a similar name exists: '--{s}'", .{data.candidates[min_index]});
                }
            }
        },
        .unknown_short_flag => |data| {
            try emitErr(writer, config, "unknown flag '-{u}'", .{data.found});
        },
        .too_many_positionals => |data| {
            try emitErr(writer, config, "too many positionals provided", .{});
            try emitErrNote(writer, config, "first extra positional: '{s}'", .{data});
        },
        .invalid_positional => |data| {
            try emitErr(writer, config, "invalid value '{s}' provided for positional argument '{s}'", .{data.arg_string, data.positional_display_name});
            if (data.arg_ty_string) |arg_ty_string|
                try emitErrNote(writer, config, "positional '{s}' requires an argument of type '{s}'", .{data.arg_string, arg_ty_string});
        },
        .missing_positionals => |missing| {
            const max_missing_notes = 3;
            if (missing.len == 1)
                try emitErr(writer, config, "missing 1 positional argument", .{})
            else 
                try emitErr(writer, config, "missing {d} positional arguments", .{missing.len});
            for (0..@min(missing.len, max_missing_notes)) |i| {
                try emitErrNote(writer, config, "missing positional: {s}", .{missing[i]});
            }
            if (missing.len > max_missing_notes) {
                switch (missing.len - max_missing_notes) {
                    1 => try emitErrNote(writer, config, "missing 1 other positional", .{}),
                    else => |n| try emitErrNote(writer, config, "missing {d} other positionals", .{n}),
                }
            }
        },
        .no_command_provided => try emitErr(writer, config, "no command provided", .{}),
        .custom => |func| func(parser, writer, config),
    }
}

pub fn formatNoCommandDefault(
    p: *Parser,
    found_token: bool,
    comptime top_level_config: argz.Config,
    comptime cmd_stack: []const Command,
    comptime current_mode: Mode,
    comptime current_flags: []const Flag,
) anyerror!void {
    if (found_token)
        return p.fail(.no_command_provided);
    var stdout = std.io.getStdOut();
    const writer = stdout.writer();
    const config = p.stdout_config;
    try Parser.formatters.prologue(
        config,
        top_level_config,
        cmd_stack,
        current_mode,
        current_flags,
        p.program_name orelse p.lexer.args.get(0),
        p.program_description,
        writer,
    );
    switch (current_mode) {
        .positionals => {},
        .commands => |commands| {
            try Parser.formatters.commands(config, commands, writer);
        },
    }
    try Parser.formatters.flags(config, current_flags, writer);
    std.process.exit(0);
}

/// Only returns strings for long flag aliases. All command aliases are returned.
pub inline fn getAliasStrings(comptime cmd_or_flag: anytype) []const [:0]const u8 {
    switch (@TypeOf(cmd_or_flag)) {
        Command => return cmd_or_flag.aliases,
        Flag => {
            comptime var aliases: [cmd_or_flag.aliases.len][:0]const u8 = undefined;
            inline for (cmd_or_flag.aliases) |alias| {
                switch (alias) {
                    .long => |long| aliases = aliases ++ .{long},
                    .short => {},
                }
            }
            return &aliases;
        },
        else => @compileError("invalid value of type '" ++ @typeName(@TypeOf(cmd_or_flag)) ++ "' passed to getAliases'"),
    }
}

pub fn emitErr(writer: anytype, config: std.io.tty.Config, comptime fmt: []const u8, args: anytype) !void {
    try emitInfo(writer, config, "error", .red, fmt, .reset, args);
}

pub fn emitErrNote(writer: anytype, config: std.io.tty.Config, comptime fmt: []const u8, args: anytype) !void {
    try emitInfo(writer, config, "note", .cyan, fmt, .reset, args);
}

pub fn emitInfo(
    writer: anytype,
    cfg: std.io.tty.Config,
    comptime category: []const u8,
    comptime category_color: ?std.io.tty.Color,
    comptime fmt: []const u8,
    comptime fmt_color: ?std.io.tty.Color,
    args: anytype,
) !void {
    if (category_color) |col| {
        try cfg.setColor(writer, .bold);
        try cfg.setColor(writer, col);
    }
    try writer.writeAll(category ++ ":");
    try cfg.setColor(writer, .reset);
    try writer.writeByte(' ');
    if (fmt_color) |col|
        try cfg.setColor(writer, col);
    try writer.print(fmt, args);
    try cfg.setColor(writer, .reset);
    try writer.writeByte('\n');
}

pub inline fn flagTypeString(comptime flag: Flag) ?[:0]const u8 {
    comptime if (flag.alt_type_name) |alt| return alt;
    return comptime typeString(flag.type, .flag);
}

pub inline fn typeString(comptime T: type, comptime context: Parser.Context.Tag) ?[:0]const u8 {
    return types.typeName(T, context);
}

pub fn commandHelpDefaultCallback(
    comptime config: argz.Config,
    comptime _: Command,
    comptime command_stack: []const Command,
    comptime mode: Mode,
    comptime flags: []const Flag,
    parser: *const Parser,
    _: []const u8
) !void {
    var stdout = std.io.getStdOut();
    const writer = stdout.writer();
    try Parser.formatters.prologue(
        parser.stdout_config,
        config,
        command_stack,
        mode,
        flags,
        parser.program_name orelse parser.lexer.args.get(0),
        parser.program_description,
        writer.any(),
    );
    switch (mode) {
        .positionals => {},
        .commands => |commands| {
            try Parser.formatters.commands(parser.stdout_config, commands, writer.any());
        },
    }
    try Parser.formatters.flags(parser.stdout_config, flags, writer.any());
    std.process.exit(0);
}

/// Computes the Damerau-Levenshtein distance between two strings
/// `a` and `b`. Note that this only considers strings up to 256 bytes
/// long. If either string is longer than `maxInt(u8)` characters, this function will instantly
/// return `maxInt(u8)`.
///
/// This function may be useful to determine string similarity for making suggestions
/// based on unknown inputs' similarity to known commands or flags. It has time complexity
/// `O(mn)`, where `m = a.len` and `n = b.len` and uses `O(1)` space.
pub fn dlDistance(a: []const u8, b: []const u8) u8 {
    const max_u8 = std.math.maxInt(u8);
    var short: []const u8 = undefined; // shorter of a and b
    var long: []const u8 = undefined; // longer of a and b
    var current_row: [max_u8 + 1]u8 = undefined;
    var previous_row: [max_u8 + 1]u8 = undefined;

    if (a.len > max_u8 or b.len > max_u8)
        return max_u8;
    if (a.len == 0 or b.len == 0)
        return @truncate(a.len | b.len);

    if (a.len > b.len) {
        short = b;
        long = a;
    } else {
        short = a;
        long = b;
    }

    current_row = @splat(0);
    for (0..long.len + 1) |i| {
        previous_row[i] = @intCast(i);
    }
    
    for (0..short.len)|i| {
        for (0..long.len) |j| {
            const cost = @intFromBool(short[i] != long[j]);
            current_row[j + 1] = @min(
               previous_row[j+1] + 1,
               current_row[j] + 1,
               previous_row[j] + cost
            );
            if (i != 0 and j != 0 and short[i] == long[j-1] and short[i-1] == long[j-1])
                current_row[j+1] = @min(current_row[j+1], previous_row[j-1] + 1);
        }
        @memcpy(&previous_row, &current_row);
        @memset(current_row[0..long.len], 0);
    }
    return previous_row[long.len];
}
