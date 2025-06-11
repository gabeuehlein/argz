const std = @import("std");
const argz = @import("argz.zig");
const Lexer = @import("Lexer.zig");
const builtin = @import("builtin");
const values = @import("Parser/values.zig");

const assert = std.debug.assert;

const Flag = argz.Flag;
const Command = argz.Command;
const Mode = argz.Mode;
const Config = argz.Config;
const Allocator = std.mem.Allocator;
const Args = @import("args.zig").Args;
const format = @import("format.zig");
const types=  @import("types.zig");

const TtyConfig = std.io.tty.Config;

const Type = std.builtin.Type;

pub const Parser = @This();

pub const Options = struct {
    /// Whether to emit ANSI escape sequences to enable support for colored output.
    color_mode: ColorMode = .detect,
    /// The name of the program that will be shown in descriptive help strings.
    program_name: ?[]const u8 = null,
    /// A brief description of how the program should be used.
    program_description: ?[]const u8 = null,
    allocator: ?Allocator = null,
    /// Gives a hint to error handlers that they should attempt to
    /// make suggestions to users to correct invalid inputs.
    make_suggestions: bool = true,
};

pub const Formatters = struct {
    flags: format.AllFlagsFormatFn = format.formatAllFlagsDefault,
    commands: format.AllCommandsFormatFn = format.formatAllCommandsDefault,
    prologue: format.PrologueFormatFn = format.formatPrologueDefault,
    errors: format.ErrorFormatFn = format.formatErrorDefault,
    no_command_provided: format.NoCommandFormatFn = format.formatNoCommandDefault,
};

pub const formatters: Formatters = blk: {
    // TODO check module options so default formatters can be overwritten by user code
    var result: Formatters = .{};
    _ = &result;
    break :blk result;
};

pub const Context = union(enum) {
    pub const Tag = std.meta.Tag(@This());

    flag: struct {
        flag_string: []const u8,
        flag_ty_string: ?[:0]const u8,
    },
    positional: struct {
        positional_display: []const u8,
        positional_ty_string: ?[:0]const u8,
    },
};

/// Describes the state of the environment in which the parser is currently at.
/// This type should always be able to be resolved at `comptime` - failure to do
/// so is a bug and should be reported as such.
pub const Environment = struct {
    context: Context,
    flags: []const Flag,
    command_stack: []const Command,
    current_mode: Mode,
    top_level_config: Config,
};

pub const ColorMode = enum(u2) {
    /// Don't emit color even if stdout/stderr support them. 
    disable,
    /// Enable colored output if stdout/stderr support them.
    detect,
    /// Force the use of color in terminal output, regardless of whether stdout/stderr
    /// support them. Note that using this is discouraged; if you don't want `argz`
    /// to check for ANSI escape sequence support, prefer `disable` instead.
    force,
};

stdout_config: std.io.tty.Config,
stderr_config: std.io.tty.Config,
program_name: ?[]const u8,
program_description: ?[]const u8,
make_suggestions: bool = true,
allocator: ?Allocator,
lexer: Lexer,

pub const Error = union(enum) {
    unexpected_arg_for_flag: struct {
        flag_string: []const u8,
        arg_string: []const u8,
    },
    expected_arg_for_flag: struct {
        flag_string: []const u8,
        arg_ty_string: ?[]const u8,
    },
    invalid_arg_for_flag: struct {
        flag_string: []const u8,
        arg_string: []const u8,
        arg_ty_string: ?[]const u8,
    },
    unknown_command: struct {
        found: []const u8,
        candidates: []const []const u8,
    },
    unknown_short_flag: struct {
        found: u21,
        candidates: []const u21,
    },
    unknown_long_flag: struct {
        found: []const u8,
        candidates: []const []const u8,
    },
    invalid_positional: struct {
        arg_string: []const u8,
        arg_ty_string: ?[]const u8,
        positional_display_name: []const u8,
    },
    /// Payload is the argument of the first extra positional found.
    too_many_positionals: []const u8,
    /// Payload is the display names of the required positionals.
    missing_positionals: []const []const u8,
    no_command_provided,
    /// Payload is the handler.
    custom: *const fn(*Parser, std.io.AnyWriter, TtyConfig) void,

    var error_buffer: [4096]u8 = undefined;
    var error_format_string: []u8 = undefined;
    // This is atomic in the case that a poorly implemented custom type
    // returns parser errors from multiple threads at the same time.
    var error_buffer_initialized: std.atomic.Value(bool) = .init(false);

    pub fn fmt(comptime format_string: []const u8, args: anytype) Error {
        assert(error_buffer_initialized.cmpxchgStrong(false, true, .seq_cst, .seq_cst) == null);

        const func = struct {
            fn func(_: *Parser, writer: std.io.AnyWriter, config: TtyConfig) void {
                config.setColor(writer, .bold) catch {};
                config.setColor(writer, .red) catch {};
                writer.writeAll("error:") catch {};
                config.setColor(writer, .reset) catch {};
                writer.writeByte(' ') catch {};
                writer.writeAll(error_format_string) catch {};
                writer.writeByte('\n') catch {};
            }
        }.func;

        error_format_string = std.fmt.bufPrint(&error_buffer, format_string, args) catch blk: {
            const truncated_msg = "<truncated>...";
            @memcpy(error_buffer[error_buffer.len-truncated_msg.len..], truncated_msg);
            break :blk &error_buffer;
        };

        return .{ .custom = &func };
    }
};

pub fn init(args: Args, options: Options) !Parser {
    const stdout_color, const stderr_color = switch (options.color_mode) {
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
    if (config.support_allocation and p.allocator == null)
        @panic("the current configuration requires a memory allocator, but the application did not provide one.");
    return p.parseInner(config.mode, config.top_level_flags, &.{}, config);
}

pub fn parseInner(p: *Parser, comptime mode: Mode, comptime flags: []const Flag, comptime command_stack: []const Command, comptime config: Config) !types.WrapModeAndFlags(mode, flags) {
    var result: types.WrapModeAndFlags(mode, flags) = undefined;
    var last_positional_index: ?usize = null;
    var positional_index: usize = 0;
    var set_flags: std.StaticBitSet(flags.len) = .initEmpty(); 
    var found_command = false;
    // includes both primary flag strings and their aliases
    const all_long_flags, const all_short_flags = comptime blk: {
        var longs: []const [:0]const u8 = &.{};
        var shorts: []const u21 = &.{};
        for (flags) |flag| {
            longs = longs ++ (if (flag.long) |long| .{ long } else .{}) ++ format.getAliasStrings(flag);
            shorts = shorts ++ if (flag.short) |char| .{ char } else .{};
        }
        break :blk .{ longs, shorts };
    };
    const start_index = p.lexer.argi;
    errdefer {
        flags: {
            if (comptime flags.len == 0)
                break :flags;
            var it = set_flags.iterator(.{});
            while (it.next()) |index| {
                switch (index) {
                    inline 0...flags.len - 1 => |i| {
                        const flag = flags[i];
                        if (types.custom.isCustomType(flag.type, .flag)) {
                            flag.type.deinitWithContext(.flag, p, &@field(result.flags, flag.fieldName()));
                        } else if (types.requiresAllocator(flag.type)) {
                            p.allocator.?.free(@field(result.flags, flag.fieldName()));
                        }
                    },
                    else => unreachable,
                }
            }
        }
        mode: {
            switch (mode) {
                .commands => |commands| {
                    if (comptime commands.len == 0)
                        break :mode;
                    if (!found_command)
                        break :mode;
                    top: inline for (commands) |cmd| {
                        const tag = std.meta.stringToEnum(std.meta.FieldEnum(@TypeOf(result.command)), cmd.cmd).?;
                        if (tag == result.command) {
                            p.freeFlags(cmd.flags, &@field(result.command, cmd.fieldName()).flags);
                            p.freeMode(cmd.mode, &@field(result.command, cmd.fieldName()));
                            break :top;
                        }
                    }
                },
                .positionals => |positionals| {
                    if (comptime positionals.len == 0)
                        break :mode;
                    switch (positional_index) {
                        inline 0...positionals.len - 1 => |i| {
                            inline for (0..i) |j| {
                                const positional = positionals[j];
                                if (types.custom.isCustomType(positional.type, .positional)) {
                                    positional.type.deinitWithContext(.positional, p, &@field(result.positionals, positional.fieldName()));
                                } else if (types.requiresAllocator(positional.type)) {
                                    p.allocator.?.free(@field(result.positionals, positional.fieldName()));
                                }
                            }
                        },
                        else => unreachable,
                    }
                },
            }
        }
    }
    top: while (p.lexer.nextToken()) |tok| {
        switch (tok) {
            .long_flag => |lf| {
                inline for (flags, 0..) |flag, i| {
                    if (flag.long) |long| {
                        if (std.mem.eql(u8, lf, long)) {
                            const env: Environment = comptime .{
                                .context = .{ .flag = .{
                                    .flag_string = flag.flagString(.long),
                                    .flag_ty_string = format.typeString(flag.type, .flag),
                                } },
                                .command_stack = command_stack,
                                .current_mode = mode,
                                .flags = flags,
                                .top_level_config = config,
                            };
                            try p.handleFlag(&result.flags, &set_flags, env, flag, .long, long, i);
                            continue :top;
                        }
                        inline for (flag.aliases) |alias| {
                            const repr = alias.flagString();
                            switch (alias) {
                                .long => |long_alias| {
                                    if (std.mem.eql(u8, lf, long)) {
                                        const env: Environment = comptime .{
                                            .context = .{ .flag = .{
                                                .flag_string = repr,
                                                .flag_ty_string = format.typeString(flag.type, .flag),
                                            } },
                                            .command_stack = command_stack,
                                            .current_mode = mode,
                                            .flags = flags,
                                            .top_level_config = config,
                                        };
                                        try p.handleFlag(&result.flags, &set_flags, env, flag, .long, long_alias, i);
                                        continue :top;
                                    }
                                },
                                else => {},
                            }
                        }
                         
                    }
                }
                return p.fail(.{ .unknown_long_flag = .{
                    .found = lf,
                    .candidates = all_long_flags,
                } });
            },
            .short_flag => |sf| {
                inline for (flags, 0..) |flag, i| {
                    if (flag.short) |short| {
                        const short_as_string = comptime std.fmt.comptimePrint("{u}", .{short});
                        if (sf == short) {
                            const env: Environment = comptime .{
                                .context = .{ .flag = .{
                                    .flag_string = "-" ++ short_as_string,
                                    .flag_ty_string = format.typeString(flag.type, .flag),
                                } },
                                .command_stack = command_stack,
                                .current_mode = mode,
                                .flags = flags,
                                .top_level_config = config,
                            };
                            try p.handleFlag(&result.flags, &set_flags, env, flag, .short, short_as_string, i);
                            continue :top;
                        }
                        inline for (flag.aliases) |alias| {
                            switch (alias) {
                                .short => |short_alias| {
                                    if (sf == short_alias) {
                                        const short_alias_as_string = comptime std.fmt.comptimePrint("{u}", .{short_alias});
                                        const env: Environment = comptime .{
                                            .context = .{ .flag = .{
                                                .flag_string = "-" ++ short_alias_as_string,
                                                .flag_ty_string = format.typeString(flag.type, .flag),
                                            } },
                                            .command_stack = command_stack,
                                            .current_mode = mode,
                                            .flags = flags,
                                            .top_level_config = config,
                                        };
                                        try p.handleFlag(&result.flags, &set_flags, env, flag, .short, short_as_string, i);
                                        continue :top;
                                    }
                                },
                                else => {},
                            }
                        }
                         
                    }
                }
                return p.fail(.{ .unknown_short_flag = .{
                    .found = sf,
                    .candidates = all_short_flags,
                } });
            },
            .word => |word| switch (mode) {
                .commands => |commands| {
                    inline for (commands) |command| {
                        if (std.mem.eql(u8, word, command.cmd)) {
                            found_command = true;
                            try command.callback(config, command, command_stack, mode, flags, p, command.cmd);
                            result.command = @unionInit(@TypeOf(result.command), command.fieldName(), try p.parseInner(command.mode, command.flags, command_stack ++ .{command}, config)); 
                            break :top;
                        }
                        inline for (command.aliases) |alias| {
                            if(true)@panic("TOOO");
                            if (std.mem.eql(u8, word, alias)) {
                                found_command = true;
                                try command.callback(config, command, command_stack, mode, flags, p, alias);
                                result.command = @unionInit(@TypeOf(result.command), command.fieldName(), try p.parseInner(command.mode, command.flags, command_stack ++ .{command}, config)); 
                                break :top;
                            }
                        }
                    }
                    const all_commands: []const [:0]const u8 = comptime blk: {
                        var all: []const [:0]const u8 = &.{};
                        for (commands) |command| {
                            all = all ++ .{ command.cmd } ++ command.aliases;
                        }
                        break :blk all;
                    };
                    return p.fail(.{ .unknown_command = .{
                        .found = word,
                        .candidates = all_commands,
                    } });
                },
                .positionals => |positionals| {
                    if (std.mem.eql(u8, word, "--")) {
                        p.lexer.found_force_stop = true;
                        continue :top;
                    }
                    if (positional_index >= positionals.len) {
                        return p.fail(.{ .too_many_positionals = word });
                    }
                    if (comptime positionals.len == 0)
                        unreachable;
                    defer last_positional_index = positional_index;
                    switch (positional_index) {
                        inline 0...positionals.len => |pi| {
                            if (comptime pi == positionals.len)
                                unreachable;
                            const positional = positionals[pi];
                            const arg = blk: {
                                if (types.custom.isCustomType(positional.type, .positional)) {
                                    switch (types.custom.customTypeArgumentMode(positional.type)) {
                                        .mandatory, .optional => break :blk word,
                                        .none => @compileError("positionals must not take no argument"),
                                    }
                                } else break :blk word;
                            };
                            const env: Environment = comptime .{
                                .context = .{ .positional = .{
                                    .positional_display = positional.displayString(),
                                    .positional_ty_string = format.typeString(positional.type, .positional),
                                } },
                                .command_stack = &.{},
                                .current_mode = mode,
                                .flags = flags,
                                .top_level_config = config,
                            };
                            if (types.custom.isCustomType(positional.type, .positional)) {
                                if (types.custom.customTypeIsRepeatable(positional.type)) {
                                    if (last_positional_index == null or positional_index > last_positional_index.?) {
                                        @field(result.positionals, positional.fieldName()) = comptime positional.type.defaultValue(.positional) orelse @compileError("default value required for positional type '" ++ @typeName(positional.type) ++ "'");
                                    }
                                } else {
                                    positional_index += 1;
                                }
                            } else {
                                positional_index += 1;
                            }
                            try values.parseValueAuto(positional.type, &@field(result.positionals, positional.fieldName()), p, arg, env, 0);
                        },
                        else => unreachable,
                    }
                },
            },
            .flag_eq => @panic("token 'flag_eq' found unexpectedly. This is a bug."),
            .err => @panic("TODO"),
        }
    }

    switch (mode) {
        .positionals => |positionals| top: {
            if (comptime positionals.len == 0)
                break :top;
            while (positional_index != positionals.len) : (positional_index += 1) {
                switch (positional_index) {
                    inline 0...positionals.len - 1 => |pi| {
                        const positional_displays_from_here: []const []const u8 = comptime blk: {
                            var displays: [positionals.len - pi][]const u8 = undefined;
                            for (pi..positionals.len) |i| {
                                displays[i - pi] = positionals[i].displayString();
                            }
                            const as_const = displays;
                            break :blk &as_const;
                        };
                        const positional = positionals[pi];
                        if (types.custom.isCustomType(positional.type, .positional)) {
                            if (last_positional_index == positional_index)
                                break :top;
                            if (types.custom.customTypeIsRepeatable(positional.type)) {
                                @field(result.positionals, positional.fieldName()) = comptime positional.type.defaultValue(.positional) orelse @compileError("default value required for positional type '" ++ @typeName(positional.type) ++ "'");
                            }
                        } else if (@typeInfo(positional.type) == .optional) {
                            inline for (pi..positionals.len) |i| {
                                @field(result.positionals, positionals[i].fieldName()) = null;
                            }
                        } 
                        return p.fail(.{ .missing_positionals = positional_displays_from_here });
                    },
                    else => unreachable,
                }
            }
        },
        .commands => if (!found_command) {
            const found_token = p.lexer.argi > start_index;
            try formatters.no_command_provided(p, found_token, command_stack, mode, flags);
            return p.fail(.no_command_provided);
        },
    }

    // TODO I'm guessing this doesn't work.
    try @import("Parser/dependency.zig").ensureFlagDependenciesSatisfied(p, flags, &result.flags, &set_flags);

    {
        const required_flags = requiredFlagsBitSet(flags);
        inline for (0..flags.len) |i| {
            const flag = flags[i];
            const context: Context = comptime .{ .flag = .{
                .flag_string = flag.flagString(.auto),
                .flag_ty_string = format.typeString(flag.type, .flag),
            } };
            if (required_flags.isSet(i)) {
                if (!set_flags.isSet(i)) {
                    if (flags[i].defaultValue()) |dv| {
                        @field(result.flags, flags[i].fieldName()) = dv;
                    } else {
                        return p.fail(.fmt(context, "missing required flag '{s}'", .{flag.flagString(.auto)}));
                    }
                }
            } else if (!set_flags.isSet(i)) {
                @field(result.flags, flags[i].fieldName()) = comptime flag.defaultValue() orelse @compileError("non-mandatory flag must have a default value");
            }
        }

    }
    return result;
}

pub fn deinit(p: *Parser, comptime config: Config, data: *ParseReturnType(config)) void {
    p.freeFlags(config.top_level_flags, &data.flags);
    p.freeMode(config.mode, switch (config.mode) {
        .commands => &data.command,
        .positionals => &data.positionals,
    });
}

fn freeCommandUnion(p: *Parser, comptime cmd: Command, data: anytype) void {
    switch (data) {
        inline else => |active_data| {
            p.freeFlags(cmd.flags, &active_data.flags);
            switch (cmd.mode) {
                .commands => p.freeMode(cmd.mode, &active_data.command),
                .positionals => p.freeMode(cmd.mode, &active_data.positionals),
            }
        },
    }
}

fn freeFlags(p: *Parser, comptime flags: []const Flag, flag_data: anytype) void {
    inline for (flags) |flag| {
        if (types.custom.isCustomType(flag.type, .flag)) {
            flag.type.deinitWithContext(.flag, p, &@field(flag_data, flag.fieldName()));
        } else if (types.requiresAllocator(flag.type)) {
            p.allocator.?.free(@field(flag_data, flag.fieldName()));
        }
    }
}

fn freeMode(p: *Parser, comptime mode: Mode, mode_data: anytype) void {
    switch (mode) {
        .commands => |commands| {
            top: inline for (commands) |cmd| {
                const tag = std.meta.stringToEnum(std.meta.FieldEnum(@TypeOf(mode_data)), cmd.cmd).?;
                if (tag == mode_data) {
                    p.freeCommandUnion(cmd, &@field(mode_data, cmd.fieldName()));
                    break :top;
                }
            }
        },
        .positionals => |positionals| {
            inline for (positionals) |positional| {
                if (types.custom.isCustomType(positional.type, .positional)) {
                    positional.type.deinitWithContext(.positional, p, &@field(mode_data, positional.fieldName()));
                } else if (types.requiresAllocator(positional.type)) {
                    p.allocator.?.free(@field(mode_data, positional.fieldName()));
                }
            }
        },
    }
}

inline fn requiredFlagsBitSet(comptime flags: []const Flag) std.StaticBitSet(flags.len) {
    var result: std.StaticBitSet(flags.len) = .initEmpty();
    inline for (flags, 0..) |flag, i| {
        if (flag.defaultValue() == null)
            result.set(i);
    }
    return result;
}

inline fn handleFlag(
    p: *Parser,
    flag_data: anytype,
    set_flags: anytype,
    comptime env: Environment,
    comptime flag: Flag,
    comptime repr: enum { long, short },
    comptime flag_string: []const u8,
    comptime index: usize,
) !void {
    if (types.custom.isCustomType(flag.type, .flag)) {
        if (types.custom.customTypeIsRepeatable(flag.type)) {
            if (!set_flags.isSet(index))
                @field(flag_data, flag.fieldName()) = comptime flag.defaultValue() orelse @compileError("default value required for flag type '" ++ @typeName(flag.type) ++ "'");
        } else if (set_flags.isSet(index)) {
            return p.fail(.fmt(env.context, "flag '{s}' found multiple times", .{ env.context.flag.flag_string }));
        }

        set_flags.set(index);
        var first_arg: ?[]const u8 = null;
        const allows_leading_dash = types.custom.customTypeAllowsLeadingDash(flag.type);
        switch (types.custom.customTypeArgumentMode(flag.type)) {
            .mandatory => {
                switch (repr) {
                    .long => {
                        _ = p.lexer.maybe(&.{ .flag_eq });
                        first_arg = try p.lexer.argument(allows_leading_dash);
                    },
                    .short => first_arg = try p.lexer.argument(allows_leading_dash),
                }
            },
            .optional => {
                if (p.lexer.maybe(&.{ .flag_eq })) |_| {
                    first_arg = try p.lexer.argument(allows_leading_dash);
                }
            },
            .none => if (repr == .long and p.lexer.maybe(&.{ .flag_eq }) != null) {
                return p.fail(.{ .unexpected_arg_for_flag = .{ .flag_string = flag_string, .arg_string = p.lexer.argument(allows_leading_dash) catch unreachable } });
            },
        }
        try flag.type.parseWithContext(env, first_arg, p, &@field(flag_data, flag.fieldName()), 0);
    } else if (set_flags.isSet(index)) {
        return p.fail(.fmt(.{.flag = .{ .flag_string = flag_string, .flag_ty_string = format.typeString(flag.type, .flag) } }, "flag '{s}' found multiple times", .{ env.context.flag.flag_string }));
    } else {
        set_flags.set(index);
        if (flag.type == void) {
            @field(flag_data, flag.fieldName()) = true;
        } else {
            if (repr == .long)
                _ = p.lexer.maybe(&.{ .flag_eq });
            const arg = p.lexer.argument(types.typeSupportsLeadingDash(flag.type, .flag)) catch return p.fail(.{ .expected_arg_for_flag = .{
                .flag_string = flag_string,
                .arg_ty_string = format.typeString(flag.type, .flag),
            } });
            try values.parseValueAuto(flag.type, &@field(flag_data, flag.fieldName()), p, arg, env, 0);
        }
    }
}

fn ParseReturnType(comptime config: Config) type {
    return types.WrapModeAndFlags(config.mode, config.top_level_flags);
}


pub fn fail(p: *Parser, err: Error) error{ParseError} {
    var stderr = std.io.getStdErr();
    stderr.lock(.exclusive) catch return error.ParseError;
    formatters.errors(p.stderr_config, err, p, stderr.writer().any()) catch return error.ParseError;
    return error.ParseError;
}

pub fn fatal(p: *const Parser, comptime fmt: []const u8, args: anytype) noreturn {
    format.emitErr(std.io.getStdErr().writer().any(), p.stderr_config, fmt, args) catch {};
    std.process.exit(1);
}

