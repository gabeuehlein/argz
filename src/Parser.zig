const std = @import("std");
const argz = @import("argz.zig");
const util = @import("util.zig");
const Lexer = @import("Lexer.zig");
const values = @import("values.zig");
const builtin = @import("builtin");

const assert = std.debug.assert;

const Flag = argz.Flag;
const Command = argz.Command;
const Mode = argz.Mode;
const Config = argz.Config;
const Allocator = std.mem.Allocator;
const Args = @import("args.zig").Args;
const ArgzType = util.ArgzType;

const TtyConfig = std.io.tty.Config;

const Type = std.builtin.Type;

const TrailingPositionals = argz.TrailingPositionals;

pub const Parser = struct {
    pub const Options = struct {
        /// Whether to emit ANSI escape sequences to enable support for colored output.
        ansi_mode: ColorMode = .detect,
        /// The name of the program that will be shown in descriptive help strings.
        program_name: ?[]const u8 = null,
        /// A brief description of how the program should be used.
        program_description: ?[]const u8 = null,
        allocator: ?Allocator = null,
    };

    pub const ColorMode = enum(u2) {
        /// Don't emit color even if stdout/stderr support them. Note that this will be
        /// forced if the NO_COLOR environment variable is set unless it is disabled in
        /// the module options.
        disable,
        /// Enable colored output if stdout/stderr support them.
        detect,
        /// Force the usage of ANSI escape sequences, regardless of whether stdout/stderr
        /// support them. Note that using this is discouraged; if you don't want `argz`
        /// to check for ANSI escape sequence support, prefer `disable` instead.
        force,
    };

    stdout_config: std.io.tty.Config,
    stderr_config: std.io.tty.Config,
    program_name: ?[]const u8,
    program_description: ?[]const u8,
    allocator: ?Allocator,
    lexer: Lexer,

    pub fn init(args: Args, options: Options) !Parser {
        const stdout_color, const stderr_color = switch (options.ansi_mode) {
            .disable => .{ .no_color, .no_color },
            .detect => blk: {
                const stdout = std.io.getStdOut();
                const stderr = std.io.getStdErr();
                break :blk .{ std.io.tty.detectConfig(stdout), std.io.tty.detectConfig(stderr) };
            },
            .force => .{ .escape_codes, .escape_codes },
        };
        return .{
            .stdout_config = stdout_color,
            .stderr_config = stderr_color,
            .program_name = options.program_name,
            .program_description = options.program_description,
            .allocator = options.allocator,
            .lexer = try Lexer.init(args),
        };
    }

    pub fn parse(p: *Parser, comptime config: Config) !ParseReturnType(config) {
        var result: ParseReturnType(config) = undefined;
        result.flags = .{};
        if (comptime config.mode == .positionals)
            result.positionals = .{};
        p.parseInner(config, config.mode, config.top_level_flags, &.{}, &result.flags, switch (comptime config.mode) {
            .positionals => &result.positionals,
            .commands => &result.command,
        }) catch |e| switch (e) {
            error.ArgzFlagHelp => std.process.exit(0),
            else => return e,
        };
        return result;
    }

    fn writeHelp(
        p: *const Parser,
        writer: anytype,
        writer_config: std.io.tty.Config,
        comptime config: Config,
        comptime flags: []const Flag,
        comptime command_stack: []const Command,
        comptime mode: Mode,
    ) !void {
        try config.formatters.prologue(
            writer_config,
            config,
            command_stack,
            mode,
            flags,
            p.program_name orelse p.lexer.args.get(0),
            p.program_description,
            writer,
        );
        switch (mode) {
            .positionals => {},
            .commands => |commands| {
                try config.formatters.commands(writer_config, commands, writer);
            },
        }
        try config.formatters.flags(writer_config, flags, writer);
    }

    inline fn handleFlag(
        p: *Parser,
        comptime config: Config,
        comptime flag: Flag,
        comptime flag_type: util.FlagType,
        comptime word_mode: Lexer.WordMode,
        comptime flags: []const Flag,
        comptime command_stack: []const Command,
        comptime mode: Mode,
        flags_data: anytype,
    ) !void {
        if (comptime flag.type == argz.FlagHelp) {
            var stdout = std.io.getStdOut();
            if (p.lexer.maybe(.flag_eq, word_mode) != null) {
                const category_topic = try p.lexer.argument(true);
                var split = std.mem.splitScalar(u8, category_topic, ':');
                const category = split.next() orelse return p.fail("extended help missing topic", .{});
                const topic = split.next().?;
                try config.formatters.expanded_help(p.stdout_config, mode, flags, category, topic, stdout.writer());
            } else {
                try p.writeHelp(stdout.writer(), p.stdout_config, config, flags, command_stack, mode);
            }
            return error.ArgzFlagHelp; // let resources get cleaned up before exiting
        }
        switch (@typeInfo(flag.type)) {
            .optional => |o| {
                if (p.lexer.maybe(.flag_eq, word_mode) != null) {
                    const arg = try p.lexer.argument(false);
                    @field(flags_data, flag.fieldName()) = if (comptime util.isSequence(flag.type))
                        try p.sequence(config, ArgzType.fromZigType(o.child), arg)
                    else blk: {
                        const ResolvedTy = ArgzType.fromZigType(o.child).Resolve(.parse_value, .flag);
                        if (comptime config.support_allocation)
                            break :blk try values.parseDynamicValue(ResolvedTy, p.allocator.?, arg)
                        else
                            break :blk try values.parseStaticValue(ResolvedTy, arg);
                    };
                } else @field(flags_data, flag.fieldName()) = null;
            },
            .void => @field(flags_data, flag.fieldName()) = true,
            else => {
                const arg = if (flag_type == .long) blk: {
                    const found_eq = p.lexer.maybe(.flag_eq, word_mode) != null;
                    break :blk try p.lexer.argument(found_eq);
                } else try p.lexer.argument(tySupportsLeadingDash(flag.type));
                const ResolvedTy = ArgzType.fromZigType(flag.type).Resolve(.parse_value, .flag);
                if (comptime util.isMulti(flag.type)) {
                    const val = if (comptime util.isSequence(flag.type.child))
                        try p.sequence(config, .fromZigType(flag.type.child), arg)
                    else if (comptime config.support_allocation)
                        try values.parseDynamicValue(ResolvedTy, p.allocator.?, arg)
                    else
                        try values.parseDynamicValue(ResolvedTy, p.allocator.?, arg);
                    if (comptime config.support_allocation)
                        try @field(flags_data, flag.fieldName()).append(p.allocator.?, val)
                    else
                        try @field(flags_data, flag.fieldName()).append(val);
                } else {
                    @field(flags_data, flag.fieldName()) = if (comptime util.isSequence(flag.type))
                        try p.sequence(config, ArgzType.fromZigType(flag.type), arg)
                    else blk: {
                        const ty_info = ArgzType.fromZigType(flag.type);
                        switch (comptime ty_info) {
                            .zig_primitive => {},
                            else => {}, //unreachable,
                        }
                        if (comptime config.support_allocation)
                            break :blk try values.parseDynamicValue(ResolvedTy, p.allocator.?, arg)
                        else
                            break :blk try values.parseStaticValue(ResolvedTy, arg);
                    };
                }
            },
        }
    }

    fn parseInner(
        p: *Parser,
        comptime config: Config,
        comptime mode: Mode,
        comptime flags: []const Flag,
        comptime command_stack: []const Command,
        flags_data: anytype,
        mode_data: anytype,
    ) !void {
        const required_flags = requiredFlagsBitSet(flags);
        var flags_found: @TypeOf(required_flags) = .initEmpty();
        var positionals_found: std.StaticBitSet(switch (mode) {
            .commands => 0,
            .positionals => |positionals| positionals.len,
        }) = .initEmpty();
        const word_mode: Lexer.WordMode = switch (mode) {
            .commands => .commands,
            .positionals => .positionals,
        };
        const has_variadic_positional, var variadic_positional_state = comptime if (config.support_allocation) switch (mode) {
            .commands => .{ false, {} },
            .positionals => |positionals| if (positionals.len == 0)
                .{ false, {} }
            else switch (@typeInfo(positionals[positionals.len - 1].type)) {
                .pointer => |ptr| if (ptr.is_const and ptr.child == u8)
                    .{ false, {} }
                else
                    .{ true, std.ArrayListUnmanaged(ArgzType.fromZigType(ptr.child).Resolve(.struct_field, .positional)).empty },
                else => .{ false, {} },
            },
        } else .{ false, {} };
        defer if (comptime has_variadic_positional) {
            variadic_positional_state.deinit(p.allocator.?);
        };
        var positional_index: usize = 0;
        var found_token = false;
        var found_force_stop = false;

        errdefer {
            var it = flags_found.iterator(.{});
            while (it.next()) |bit_idx| {
                switch (bit_idx) {
                    inline 0...flags.len => |i| {
                        if (comptime i == flags.len) unreachable;
                        if (ArgzType.fromZigType(flags[i].type).requiresAllocator()) {
                            values.freeExt(p.allocator.?, flags[i].type, &@field(flags_data, flags[i].fieldName()));
                        }
                    },
                    else => unreachable,
                }
            }

            switch (mode) {
                .positionals => |positionals| for (0..positional_index) |positional_idx| {
                    switch (positional_idx) {
                        inline 0...mode.positionals.len => |i| {
                            if (comptime i == positionals.len) unreachable;
                            if (ArgzType.fromZigType(positionals[i].type).requiresAllocator()) {
                                values.freeExt(p.allocator.?, positionals[i].type, @field(mode_data, positionals[i].fieldName()));
                            }
                        },
                        else => unreachable,
                    }
                },
                .commands => {}, // inner parseInner already cleaned up the command's data
            }
        }

        top: while (p.lexer.nextToken(word_mode)) |tok| {
            found_token = true;
            switch (tok) {
                .long_flag => |flag_text| {
                    inline for (flags, 0..) |flag, i| {
                        if (comptime flag.long) |long| {
                            if (std.mem.eql(u8, long, flag_text)) {
                                if (flags_found.isSet(i)) {
                                    if (comptime !(util.isCounter(flag.type) or util.isMulti(flag.type)))
                                        return p.fail("flag '--" ++ flag.long.? ++ "' was found multiple times", .{});
                                }
                                try p.handleFlag(config, flag, .long, word_mode, flags, command_stack, mode, flags_data);
                                flags_found.set(i);
                                break;
                            }
                        }
                    } else return p.fail("unknown flag '--{s}'", .{flag_text});
                },
                .short_flag, .short_chain => |flag_char| {
                    inline for (flags, 0..) |flag, i| {
                        if ((comptime flag.short != null) and flag_char == flag.short) {
                            if (flags_found.isSet(i)) {
                                if (comptime !(util.isCounter(flag.type) or util.isMulti(flag.type)))
                                    return p.fail("flag '-" ++ (comptime std.fmt.comptimePrint("{u}", .{flag.short.?})) ++ "' was found multiple times", .{});
                            }
                            try p.handleFlag(config, flag, .long, word_mode, flags, command_stack, mode, flags_data);
                            flags_found.set(i);
                            break;
                        }
                    } else return p.fail("unknown flag: '-{u}'", .{flag_char});
                },
                .command => |cmd_text| {
                    if (comptime mode != .commands) unreachable;
                    const commands = mode.commands;
                    inline for (commands, 0..) |cmd, i| {
                        if (std.mem.eql(u8, cmd.cmd, cmd_text)) {
                            const target = commands[i];
                            var cmd_flags_data: util.StructFromFlags(target.flags) = .{};
                            var cmd_mode_data: cmd.mode.ToType() = undefined;
                            if (comptime mode == .positionals)
                                cmd_mode_data = .{};
                            try p.parseInner(config, cmd.mode, cmd.flags, command_stack ++ .{target}, &cmd_flags_data, &cmd_mode_data);
                            mode_data.* = @unionInit(@TypeOf(mode_data.*), cmd.fieldName(), switch (cmd.mode) {
                                .commands => .{ .flags = cmd_flags_data, .cmd = cmd_mode_data },
                                .positionals => .{ .flags = cmd_flags_data, .positionals = cmd_mode_data },
                            });
                            break;
                        }
                    } else return p.fail("unknown command '{s}'", .{cmd_text});
                },
                .positional => |positional_text| {
                    if (comptime mode != .positionals) unreachable;
                    const positionals = mode.positionals;
                    if (positional_index == positionals.len)
                        return p.fail("found extra positional '{s}'", .{positional_text});
                    switch (positional_index) {
                        inline 0...positionals.len => |idx| {
                            if (comptime idx == positionals.len) // checked above
                                unreachable;
                            if (comptime positionals[idx].type == argz.Trailing) {
                                if (!found_force_stop)
                                    return p.fail("positional '{s}' must be placed after a force stop (\"--\") sequence", .{positional_text});
                                @field(mode_data, positionals[idx].fieldName()) = TrailingPositionals.init(p.lexer.args, p.lexer.argi - 1);
                                break :top;
                            }
                            const ResolvedTy = ArgzType.fromZigType(positionals[idx].type).Resolve(.parse_value, .positional);
                            if (comptime config.support_allocation) {
                                const val = try values.parseDynamicValue(ResolvedTy, p.allocator.?, positional_text);
                                if (comptime has_variadic_positional and idx == positionals.len - 1) {
                                    try variadic_positional_state.append(p.allocator.?, val);
                                } else {
                                    @field(mode_data, positionals[idx].fieldName()) = val;
                                    positional_index += 1;
                                }
                            } else {
                                const val = try values.parseStaticValue(ResolvedTy, positional_text);
                                @field(mode_data, positionals[idx].fieldName()) = val;
                                positional_index += 1;
                            }
                            positionals_found.set(idx);
                        },
                        else => unreachable,
                    }
                },
                .force_stop => {
                    if (comptime mode != .positionals) unreachable;
                    const positionals = mode.positionals;
                    if (comptime positionals.len == 0) continue;
                    blk: switch (positional_index) {
                        inline 0...positionals.len - 1 => |i| if (comptime positionals.len == 0) break :blk else {
                            if (positionals[positionals.len - 1].type == argz.Trailing)
                                if (i != positionals.len - 1)
                                    return p.fail("missing required positional '{s}'", .{positionals[i].displayString()});
                        },
                        positionals.len => {},
                        else => unreachable,
                    }
                    found_force_stop = true;
                },
                .flag_eq => return p.fail("wasn't expecting an argument in '{s}'", .{p.lexer.args.get(p.lexer.argi)}),
                inline else => |_, tag| @panic("TODO: " ++ @tagName(tag)),
            }
        }
        if (!found_token) {
            if (!required_flags.eql(.initEmpty()) or mode == .commands or (mode.positionals.len != 0 and switch (@typeInfo(mode.positionals[0].type)) {
                .optional => false,
                .pointer => |info| info.is_const and info.child == u8,
                else => true,
            })) {
                var stdout = std.io.getStdOut();
                p.writeHelp(stdout.writer(), p.stdout_config, config, flags, command_stack, mode) catch {};
                return error.ArgzFlagHelp;
            }
        }
        if (comptime mode == .positionals) {
            const positionals = mode.positionals;
            if (positional_index != positionals.len) {
                switch (positional_index) {
                    inline 0...positionals.len => |i| if (comptime i == positionals.len) unreachable else blk: {
                        switch (@typeInfo(positionals[i].type)) {
                            .optional => break :blk,
                            .pointer => |info| if (!(info.child == u8 and info.is_const)) break :blk,
                            else => {},
                        }
                        if (positionals[i].type == argz.Trailing)
                            break :blk;
                        return p.fail("missing required positional '{s}'", .{positionals[i].displayString()});
                    },
                    else => unreachable,
                }
            }
        }
        inline for (flags, 0..flags.len) |flag, i| {
            if (!flags_found.isSet(i)) {
                if (required_flags.isSet(i)) {
                    if (comptime flag.defaultValue()) |dv|
                        @field(flags_data, flag.fieldName()) = dv
                    else
                        return p.fail("missing required flag: {s}", .{flag.flagString(if (flag.long != null) .long else .short)});
                }
            }
        }
        if (comptime has_variadic_positional)
            @field(mode_data, mode.positionals[mode.positionals.len - 1].fieldName()) = try variadic_positional_state.toOwnedSlice(p.allocator.?);
    }

    fn fail(p: *const Parser, comptime fmt: []const u8, args: anytype) error{ParseError} {
        var stderr = std.io.getStdErr();
        if (!(stderr.tryLock(.exclusive) catch return error.ParseError))
            return error.ParseError;
        defer stderr.unlock();
        emitInfo(stderr.writer(), p.stderr_config, "error", .red, fmt, .blue, args) catch {};
        return error.ParseError;
    }

    pub fn fatal(p: *const Parser, comptime fmt: []const u8, args: anytype) noreturn {
        p.fail(fmt, args) catch {};
        std.process.exit(1);
    }

    pub fn emitInfo(writer: anytype, cfg: std.io.tty.Config, comptime category: []const u8, comptime category_color: ?std.io.tty.Color, comptime fmt: []const u8, comptime fmt_color: ?std.io.tty.Color, args: anytype,) !void {
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

    fn sequence(p: *Parser, comptime config: Config, comptime ty_info: ArgzType, first_arg: ?[]const u8) !ty_info.Resolve(.struct_field, .flag) {
        comptime assert(ty_info == .sequence);
        if (ty_info.sequence.len == 0)
            return .{};
        const tys = ty_info.sequence;
        var arg: []const u8 = first_arg orelse try p.lexer.argument(tySupportsLeadingDash(tys[0]));
        var result: ty_info.Resolve(.struct_field, .flag) = undefined;
        var initialized: std.StaticBitSet(tys.len) = .initEmpty();
        errdefer {
            inline for (0..tys.len) |i| {
                if (initialized.isSet(i))
                    if (ArgzType.fromZigType(tys[i]).requiresAllocator())
                        values.freeExt(p.allocator.?, result[i]);
            }
        }
        inline for (0..tys.len) |i| {
            const ToParse = ArgzType.fromZigType(tys[i]).Resolve(.parse_value, .flag);
            result[i] = if (config.support_allocation)
                try values.parseDynamicValue(ToParse, p.allocator.?, arg)
            else
                try values.parseStaticValue(ToParse, arg);
            if (comptime i + 1 == tys.len)
                break; // don't treat the next argument as a positional
            arg = p.lexer.argument(tySupportsLeadingDash(tys[i + 1])) catch |e| switch (e) {
                error.MissingArgument => switch (@typeInfo(tys[i])) {
                    .optional => {
                        inline for (i..tys.len) |j| {
                            result[j] = null;
                        }
                        break;
                    },
                    else => return p.fail("expected another argument", .{}),
                },
                else => return e,
            };
        }
        return result;
    }
};

inline fn requiredFlagsBitSet(comptime flags: []const Flag) std.StaticBitSet(flags.len) {
    var result: std.StaticBitSet(flags.len) = .initEmpty();
    inline for (flags, 0..) |flag, i| {
        if (!(flag.type == argz.FlagHelp or util.isCounter(flag.type) or util.isMulti(flag.type) or flag.type == void or flag.default_value_ptr != null))
            result.set(i);
    }
    return result;
}

pub fn ParseReturnType(comptime config: Config) type {
    return switch (config.mode) {
        .positionals => struct {
            flags: util.StructFromFlags(config.top_level_flags),
            positionals: config.mode.ToType(),
        },
        .commands => struct {
            flags: util.StructFromFlags(config.top_level_flags),
            command: config.mode.ToType(),
        },
    };
}

inline fn tySupportsLeadingDash(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .int => |i| i.signedness == .signed,
        .float => true,
        .pointer => |p| (p.is_const and p.child == u8) or tySupportsLeadingDash(p.child),
        else => false,
    };
}
