const std = @import("std");
const argz = @import("argz.zig");
const util = @import("util.zig");
const Lexer = @import("Lexer.zig");
const HelpPrinter = @import("help.zig").HelpPrinter;
const values = @import("values.zig");
const ansi = @import("ansi.zig");

const assert = std.debug.assert;

const a = ansi.ansiFormatter;
const Flag = argz.Flag;
const Command = argz.Command;
const Mode = argz.Mode;
const Config = argz.Config;
const Allocator = std.mem.Allocator;
const Args = @import("args.zig").Args;

const Type = std.builtin.Type;

const TrailingPositionals = argz.TrailingPositionals;

/// Doesn't check the validity of the type beforehand.
pub fn ResolveType(comptime T: type) type {
    assert(@inComptime());
    @setEvalBranchQuota(5000000);

    const argzType = util.ArgzType.fromZigType;

    comptime return switch (argzType(T)) {
        .pair => |p| struct { p.lhs_type, p.rhs_type },
        .multi => |m| ResolveType(m.child),
        .counter => @compileError("internal argz error: type '" ++ @typeName(T) ++ "' shouldn't have been resolved"),
        .zig_primitive => |ty| if (ty == void) bool else ty,
        .trailing => TrailingPositionals,
        .flag_help => void,
    };
}

fn validateFlags(comptime flags: []const Flag, comptime support_allocation: bool) void {
    assert(@inComptime());
    @setEvalBranchQuota(5000000);
    const argzType = util.ArgzType.fromZigType;

    inline for (0.., flags) |i, flag| {
        inline for (0.., flags) |j, other_flag| {
            if (i == j) continue;
            if (flag.short != null and flag.short == other_flag.short)
                @compileError(std.fmt.comptimePrint("flag at index {d} has the same short representation as the flag at index {d} ('{u}')", .{ i, j, flag.short.? }));
            if (flag.long) |long|
                if (other_flag.long) |other_long|
                    if (std.mem.eql(u8, long, other_long))
                        @compileError(std.fmt.comptimePrint("flag at index {d} has the same long representation as the flag at index {d} ('{s}')", .{ i, j, long }));
        }
        util.validateType(flag.type, .flag, false, false, support_allocation);
        const info = argzType(flag.type);
        if (!support_allocation and info.requiresAllocator())
            @compileError("flag type '" ++ @typeName(flag.type) ++ "' is not supported without an allocator");
    }
}

fn TypeFromFlags(comptime flags: []const Flag, comptime support_allocation: bool) type {
    assert(@inComptime());
    @setEvalBranchQuota(5000000);

    validateFlags(flags, support_allocation);
    comptime var fields = @as([flags.len]Type.StructField, undefined);
    inline for (0.., flags) |i, flag| {
        const FlagType = ResolveType(flag.type);
        fields[i] = Type.StructField{
            .name = flag.fieldName(),
            .type = FlagType,
            .alignment = 0,
            .is_comptime = false,
            .default_value_ptr = null,
        };
    }
    return @Type(.{ .@"struct" = .{ .fields = &fields, .layout = .auto, .decls = &.{}, .is_tuple = false } });
}

inline fn requiredFlagsBitSet(comptime flags: []const Flag) std.StaticBitSet(flags.len) {
    comptime {
        var result: std.StaticBitSet(flags.len) = .initEmpty();
        for (0.., flags) |i, flag| {
            if (!(flag.type == void or flag.type == argz.FlagHelp or util.isCounter(flag.type) or util.isMulti(flag.type) or flag.default_value_ptr != null)) {
                result.set(i);
            }
        }
        return result;
    }
}

fn TypeFromMode(comptime mode: Mode, comptime support_allocation: bool) type {
    assert(@inComptime());
    @setEvalBranchQuota(5000000);

    comptime return switch (mode) {
        .commands => |commands| {
            var enum_fields = @as([commands.len]Type.EnumField, undefined);
            var union_fields = @as([commands.len]Type.UnionField, undefined);

            for (0.., commands) |i, cmd| {
                enum_fields[i] = Type.EnumField{
                    .name = cmd.fieldName(),
                    .value = i,
                };
                union_fields[i] = Type.UnionField{ .name = cmd.fieldName(), .type = switch (cmd.mode) {
                    .positionals => struct { positionals: TypeFromMode(cmd.mode, support_allocation), flags: TypeFromFlags(cmd.flags, support_allocation) },
                    .commands => struct { command: TypeFromMode(cmd.mode, support_allocation), flags: TypeFromFlags(cmd.flags, support_allocation) },
                }, .alignment = 0 };
            }

            const EnumType = @Type(.{ .@"enum" = .{
                .fields = &enum_fields,
                .tag_type = std.math.IntFittingRange(0, commands.len),
                .is_exhaustive = true,
                .decls = &.{},
            } });
            return @Type(.{ .@"union" = .{
                .fields = &union_fields,
                .layout = .auto,
                .tag_type = EnumType,
                .decls = &.{},
            } });
        },
        .positionals => |positionals| {
            var struct_fields = @as([positionals.len]Type.StructField, undefined);
            if (positionals.len != 0) {
                const last = positionals[positionals.len - 1];
                const last_info = util.ArgzType.fromZigType(last.type);
                if (!support_allocation and last_info.requiresAllocator())
                    @compileError("final positional argument type '" ++ @typeName(last.type) ++ "' requires an allocator to parse");
            }

            var found_optional = false;

            for (0.., positionals) |i, positional| {
                if (!support_allocation and i != positionals.len and util.ArgzType.fromZigType(positional.type).requiresAllocator())
                    @compileError("positional argument type '" ++ @typeName(positional.type) ++ "' requires an allocator to parse");
                if (@typeInfo(positional.type) == .optional) {
                    if (!found_optional)
                        found_optional = true;
                } else if (found_optional and positional.type != argz.Trailing)
                    @compileError("optional positionals must be at the end of a positional list");
                util.validateType(positional.type, .positional, false, false, support_allocation);
                struct_fields[i] = Type.StructField{
                    .name = positional.fieldName(),
                    .type = ResolveType(positional.type),
                    .default_value_ptr = null,
                    .is_comptime = false,
                    .alignment = 0,
                };
            }

            return @Type(.{ .@"struct" = .{
                .fields = &struct_fields,
                .layout = .auto,
                .decls = &.{},
                .is_tuple = false,
            } });
        },
    };
}

fn ArgParser(comptime cfg: argz.Config) type {
    @setEvalBranchQuota(5000000);
    return struct {
        const Self = @This();

        pub const ParseError = anyerror; //error{ArgParseError} || util.ParseValueError || Allocator.Error || error{Overflow};

        const Options = ParseInnerReturnType(cfg.mode, cfg.top_level_flags);

        lexer: Lexer,
        positional_index: usize = 0,
        stdout_supports_ansi: bool,
        stderr_supports_ansi: bool,
        allocator: Allocator,

        const FlagType = util.FlagType;

        inline fn parseValue(self: *const Self, comptime T: type, string: []const u8) !ResolveType(T) {
            const R = ResolveType(T);
            if (cfg.support_allocation)
                return try values.parseDynamicValue(R, self.allocator, string)
            else
                return try values.parseStaticValue(R, string);
        }

        inline fn handleFlag(
            self: *Self,
            comptime flags: []const Flag,
            comptime index: usize,
            comptime command_stack: []const Command,
            comptime variant: FlagType,
            comptime mode: Mode,
            val: ?[]const u8,
            flag_data: *TypeFromFlags(flags, cfg.support_allocation),
            flags_set: *std.StaticBitSet(flags.len),
        ) !void {
            const flag = flags[index];
            switch (variant) {
                .long => if (flag.long == null) unreachable,
                .short => if (flag.short == null) unreachable,
            }
            var ok = true;
            defer {
                if (ok)
                    flags_set.set(index);
            }
            errdefer ok = false;
            switch (util.ArgzType.fromZigType(flag.type)) {
                .counter => if (val) |v| {
                    return self.fail("wasn't expecting value '{s}' for flag '{s}'", .{ v, flag.flagString(variant) });
                } else {
                    @field(flag_data, flag.fieldName()) +|= 1;
                },
                .pair => {
                    if (flags_set.isSet(index))
                        return self.fail("flag '{s}' found multiple times", .{flag.flagString(variant)});
                    const v = val orelse return self.fail("expected value for flag '{s}'", .{flag.flagString(variant)});
                    @field(flag_data, flag.fieldName()) = try self.parseValue(flag.type, v);
                },
                .multi => |m| {
                    if (@typeInfo(m.child) == .optional) {
                        if (val) |v| {
                            try @field(flag_data, flag.fieldName()).append(try self.parseValue(@typeInfo(m.child).optional.child, v));
                        } else {
                            try @field(flag_data, flag.fieldName()).append(null);
                        }
                    } else {
                        const v = val orelse return self.fail("expected value for flag '{s}'", .{flag.flagString(variant)});
                        try @field(flag_data, flag.fieldName()).append(try self.parseValue(m.child, v));
                    }
                },
                .zig_primitive => |prim| {
                    if (flags_set.isSet(index))
                        return self.fail("flag '{s}' found multiple times", .{flag.flagString(variant)});
                    if (prim == void) {
                        if (val) |v|
                            return self.fail("unexpected value '{s}' found for flag '{s}'", .{ v, flag.flagString(variant) });
                        @field(flag_data, flag.fieldName()) = true;
                    } else if (@typeInfo(prim) == .optional) {
                        if (val) |v| {
                            @field(flag_data, flag.fieldName()) = try self.parseValue(@typeInfo(prim).optional.child, v);
                        } else {
                            @field(flag_data, flag.fieldName()) = null;
                        }
                    } else if (@typeInfo(prim) == .array and @typeInfo(@typeInfo(prim).array.child) == .optional) {
                        if (val) |v| {
                            @field(flag_data, flag.fieldName()) = try self.parseValue(prim, v);
                        } else {
                            @field(flag_data, flag.fieldName()) = @splat(null);
                        }
                    } else {
                        const v = val orelse return self.fail("expected value for flag '{s}'", .{flag.flagString(variant)});
                        @field(flag_data, flag.fieldName()) = try self.parseValue(prim, v);
                    }
                },
                .flag_help => {
                    var stdout = std.io.getStdOut();
                    const w = stdout.writer();
                    if (val) |v| {
                        const colon_idx = std.mem.indexOfScalar(u8, v, ':') orelse return self.fail("colon separator (':') missing in help flag argument '{s}'", .{v});
                        const category = v[0..colon_idx];
                        const topic = v[colon_idx + 1 ..];
                        cfg.formatters.expanded_help(mode, flags, self.stdout_supports_ansi, category, topic, w) catch |e| switch (e) {
                            error.UnknownHelpTopic => return self.fail("unknown help topic for category '{s}': {s}", .{ category, topic }),
                            error.UnknownHelpCategory => return self.fail("unknown help category '{s}'", .{category}),
                            error.NoHelpAvailable => return self.fail("no help available for '{s}:{s}'", .{ category, topic }),
                            error.DifferentModeActive => return self.fail("category '{s}' is unavailable", .{category}),
                            else => return e,
                        };
                    } else {
                        try self.writeHelpFull(w, self.stdout_supports_ansi, flags, command_stack, mode);
                    }
                    std.process.exit(0);
                },
                .trailing => unreachable,
            }
        }

        fn handleErr(self: *@This(), err: Lexer.Token.Error, comptime flags: []const Flag, comptime commands: []const Command) ParseError {
            const flagStringWithRuntimeIndex = struct {
                fn func(comptime flags1: []const Flag, comptime variant: FlagType, idx: Lexer.FlagIndex) [:0]const u8 {
                    return switch (@intFromEnum(idx)) {
                        inline 0...@max(1, flags.len) - 1 => |i| {
                            if (i >= flags1.len)
                                unreachable;
                            return flags[i].flagString(variant);
                        },
                        else => unreachable,
                    };
                }
            }.func;
            _ = commands;
            const args = self.lexer.args;
            return switch (err) {
                .unexpected_value_for_long_flag => |data| self.fail("unexpected value '{s}' found for flag '{s}'", .{ args.getSpanText(data.value_span), flagStringWithRuntimeIndex(flags, .long, data.index) }),
                .unexpected_value_for_short_flag => |data| self.fail("unexpected value '{s}' found for flag '{s}'", .{ args.getSpanText(data.value_span), flagStringWithRuntimeIndex(flags, .short, data.index) }),
                .missing_value_for_long_flag => |data| self.fail("expected value after the equals sign for flag '{s}'", .{flagStringWithRuntimeIndex(flags, .long, data.index)}),
                .missing_value_for_short_flag => |data| self.fail("expected value after the equals sign for flag '{s}'", .{flagStringWithRuntimeIndex(flags, .short, data.index)}),
                .expected_value_for_long_flag => |data| self.fail("expected value for flag '{s}'", .{flagStringWithRuntimeIndex(flags, .long, data.index)}),
                .expected_value_for_short_flag => |data| self.fail("expected value for flag '{s}'", .{flagStringWithRuntimeIndex(flags, .short, data.index)}),
                .unknown_command => |data| self.fail("unknown command '{s}'", .{data.argv_index.get(args)}),
                .unknown_long_flag => |data| self.fail("unknown long flag '{s}'", .{data.span.getText(args)}),
                .unknown_short_flag => |data| self.fail("unknown short flag '{s}'", .{data.span.getText(args)}),
                .unexpected_force_stop => self.fail("unexpected force stop found", .{}),
                .empty_argument => self.fail("stray empty argument found", .{}),
            };
        }

        fn emitInfo(self: *const @This(), comptime class_color: ansi.TerminalColor, comptime fmt_color: ansi.TerminalColor, comptime class: []const u8, comptime fmt: []const u8, args: anytype) void {
            const stderr = std.io.getStdErr();
            stderr.lock(.exclusive) catch return;
            defer stderr.unlock();
            var stderrw = stderr.writer();

            stderrw.print("{s} ", .{a(class ++ ":", self.stderr_supports_ansi, class_color, .bold)}) catch {};
            comptime var p = std.fmt.Parser{
                .pos = 0,
                .iter = .{ .i = 0, .bytes = fmt },
            };
            comptime var i = 0;
            inline while (true) : (i += 1) {
                const str = comptime p.until('{');
                stderrw.print("{s}", .{a(str, self.stderr_supports_ansi, fmt_color, null)}) catch {};
                if (comptime p.maybe('{')) {
                    if (comptime p.maybe('{')) {
                        stderrw.writeByte('}') catch {};
                    } else {
                        const fmt_spec = comptime p.until('}');
                        if (!comptime p.maybe('}')) unreachable;
                        std.fmt.format(stderrw, "{" ++ fmt_spec ++ "}", .{a(args[i], self.stderr_supports_ansi, .blue, null)}) catch {};
                    }
                } else break;
            }
            stderrw.writeByte('\n') catch {};
            comptime assert(i == @typeInfo(@TypeOf(args)).@"struct".fields.len);
        }

        /// Internal function to emit a formatted error
        fn fail(self: *const @This(), comptime fmt: []const u8, args: anytype) ParseError {
            self.emitInfo(.red, .blue, "error", fmt, args);
            return error.ArgParseError;
        }

        pub fn fatal(self: *const @This(), comptime fmt: []const u8, args: anytype) noreturn {
            self.fail(fmt, args) catch {};
            std.process.exit(1);
        }

        pub fn warn(self: *const @This(), comptime fmt: []const u8, args: anytype) void {
            self.emitInfo(.yellow, .blue, "warning", fmt, args);
        }

        /// Prints a message depending on the `ParseError` provided, and returns `error.ArgParseError`
        fn handleInternalError(self: *const @This(), comptime flag_data: ?struct { Flag, FlagType }, comptime positional: ?argz.Positional, val: ?[]const u8, err: ParseError) ParseError {
            const args2 = if (flag_data) |data| .{
                "flag",
                data[0].flagString(data[1]),
            } else .{
                "positional",
                positional.?.display,
            };
            const args3 = if (flag_data) |data| .{
                val orelse "",
                "flag",
                data[0].flagString(data[1]),
            } else .{
                val orelse "",
                "positional",
                positional.?.display,
            };
            return switch (err) {
                error.ArgParseError => error.ArgParseError,
                error.OutOfMemory => self.fail("ran out of memory when parsing arguments", .{}),
                error.InvalidBool => self.fail("invalid boolean value '{s}' provided to {s} '{s}'", args3),
                error.TooManyArguments => self.fail("too many array elements provided for {s} '{s}'", args2),
                error.NotEnoughArguments => self.fail("too few array elements provided for {s} '{s}'", args2),
                error.Overflow => self.fail("too many unique values provided for variadic {s} '{s}'", args2),
                error.InvalidCharacter => self.fail("invalid character found when parsing value '{s}' for {s} '{s}'", args3),
                error.InvalidEnumField => self.fail("invalid variant '{s}' found for {s} '{s}'", args3),
                error.IntegerOverflow => self.fail("integer '{s}' out of range for {s} '{s}'", args3),
                else => |e| self.fail("TODO: handle internal error '{s}'", .{@errorName(e)}),
            };
        }

        fn ParseInnerReturnType(comptime mode: Mode, comptime flags: []const Flag) type {
            const ModeData = TypeFromMode(mode, cfg.support_allocation);
            const Flags = TypeFromFlags(flags, cfg.support_allocation);
            return switch (mode) {
                .commands => struct { command: ModeData, flags: Flags },
                .positionals => struct { positionals: ModeData, flags: Flags },
            };
        }

        pub fn parseInner(self: *Self, comptime mode_or_cmd: anytype, comptime flags: []const Flag, comptime cmd_stack: []const Command) ParseError!ParseInnerReturnType(mode_or_cmd, flags) {
            const mode = if (@TypeOf(mode_or_cmd) == Command) mode_or_cmd.mode else mode_or_cmd;
            const has_trailing_positionals = comptime switch (mode) {
                .positionals => |positionals| positionals.len != 0 and positionals[positionals.len - 1].type == argz.Trailing,
                .commands => false,
            };
            const has_variadic_positional = comptime switch (mode) {
                .positionals => |positionals| positionals.len > @intFromBool(has_trailing_positionals) and positionals[positionals.len - 1 - @intFromBool(has_trailing_positionals)].type == argz.Trailing,
                .commands => false,
            };
            const required_flags = requiredFlagsBitSet(flags);
            var found_trailing_positionals = false;
            var result = @as(ParseInnerReturnType(mode_or_cmd, flags), undefined);
            var flags_set = std.StaticBitSet(flags.len).initEmpty();
            var found_command = false;
            var variadic_positional_state = comptime switch (mode) {
                .positionals => |positionals| if (positionals.len == 0 or !util.ArgzType.fromZigType(positionals[positionals.len - 1 - @intFromBool(has_trailing_positionals)].type).requiresAllocator())
                    std.ArrayListUnmanaged(void).empty
                else
                    std.ArrayListUnmanaged(@typeInfo(positionals[positionals.len - 1 - @intFromBool(has_trailing_positionals)].type).pointer.child).empty,
                .commands => std.ArrayListUnmanaged(void).empty,
            };
            var found_token = false;
            errdefer if (cfg.support_allocation) {
                inline for (0.., flags) |i, flag| {
                    if (flags_set.isSet(i) and util.ArgzType.fromZigType(flag.type).requiresAllocator()) {
                        values.freeExt(self.allocator, @field(result.flags, flag.fieldName()));
                    }
                }
                if (@TypeOf(variadic_positional_state) != std.ArrayListUnmanaged(void)) {
                    if (util.ArgzType.fromZigType(@TypeOf(variadic_positional_state.pop())).requiresAllocator()) {
                        for (variadic_positional_state.items) |itm|
                            values.freeExt(self.allocator, itm);
                    }
                    variadic_positional_state.deinit(self.allocator);
                }
            };
            top: while (self.lexer.nextToken(flags, switch (mode) {
                .positionals => .positionals,
                .commands => |cmds| .{ .commands = .{ .commands = cmds, .default = null } },
            })) |tok| {
                found_token = true;
                switch (tok) {
                    .err => |e| return self.handleErr(e, flags, cmd_stack),
                    inline .long_flag, .short_flag => |data, tag| if (comptime flags.len == 0) unreachable else switch (@intFromEnum(data.index)) {
                        inline 0...flags.len - 1 => |idx| self.handleFlag(flags, idx, cmd_stack, if (tag == .long_flag) .long else .short, mode, null, &result.flags, &flags_set) catch |e| {
                            return self.handleInternalError(.{ flags[idx], if (tag == .long_flag) .long else .short }, null, null, e);
                        },
                        else => unreachable,
                    },
                    inline .long_flag_with_value, .short_flag_with_value => |data, tag| if (comptime flags.len == 0) unreachable else switch (@intFromEnum(data.index)) {
                        inline 0...flags.len - 1 => |idx| self.handleFlag(flags, idx, cmd_stack, if (tag == .long_flag_with_value) .long else .short, mode, self.lexer.args.getSpanText(data.value_span), &result.flags, &flags_set) catch |e| return self.handleInternalError(.{ flags[idx], if (tag == .long_flag_with_value) .long else .short }, null, self.lexer.args.getSpanText(data.value_span), e),
                        else => unreachable,
                    },
                    .force_stop => if (has_trailing_positionals) {
                        if (mode.positionals.len == 0)
                            return self.fail("unexpected force stop found", .{});
                        switch (self.positional_index) {
                            inline 0...@max(1, mode.positionals.len) - 1 => |idx| {
                                const pos = mode.positionals[idx];
                                if (pos.type != argz.Trailing) {
                                    switch (@typeInfo(pos.type)) {
                                        .optional, .pointer => {
                                            self.positional_index = mode.positionals.len - 1;
                                        },
                                        else => return self.fail("unexpected force stop found", .{}),
                                    }
                                }
                            },
                            else => unreachable,
                        }
                    },
                    .positional => |index| {
                        if (mode != .positionals)
                            unreachable;
                        const arg = index.argv_index.get(self.lexer.args);
                        const positionals = mode.positionals;
                        if (positionals.len == 0)
                            return self.fail("unexpected positional argument found: '{s}'", .{arg});
                        switch (self.positional_index) {
                            inline 0...@max(1, positionals.len) - 1 => |idx| {
                                const pos = positionals[idx];
                                if (pos.type == argz.Trailing) {
                                    if (!self.lexer.found_force_stop)
                                        return self.fail("extra positional argument found: '{s}'", .{arg});

                                    @field(result.positionals, pos.fieldName()) = TrailingPositionals.init(self.lexer.args, self.lexer.argi - 1);
                                    found_trailing_positionals = true;
                                    // "eat" the rest of the arguments
                                    self.lexer.argi = self.lexer.args.len;
                                    break :top;
                                }
                                if (pos.type == []const u8 or @typeInfo(pos.type) != .pointer) {
                                    self.positional_index += 1;
                                    @field(result.positionals, pos.fieldName()) = self.parseValue(pos.type, arg) catch |e| return self.handleInternalError(null, pos, arg, e);
                                } else {
                                    comptime assert(idx == positionals.len - 1 - @intFromBool(has_trailing_positionals) - @intFromBool(has_variadic_positional));
                                    if (has_trailing_positionals)
                                        assert(!self.lexer.found_force_stop);
                                    try variadic_positional_state.append(self.allocator, self.parseValue(@typeInfo(pos.type).pointer.child, arg) catch |e| return self.handleInternalError(null, pos, arg, e));
                                }
                            },
                            else => return self.fail("extra positional argument found: '{s}'", .{arg}),
                        }
                    },
                    .command => |index| {
                        if (comptime mode == .positionals)
                            unreachable;
                        switch (mode) {
                            .positionals => unreachable,
                            .commands => |cmds| switch (@intFromEnum(index.index)) {
                                inline 0...@max(1, cmds.len) - 1 => |idx| {
                                    if (comptime cmds[idx].is_help) {
                                        comptime assert(cmds[idx].mode == .positionals and cmds[idx].mode.positionals.len == 0);
                                        var stdout = std.io.getStdOut();
                                        const stdoutw = stdout.writer();
                                        try self.writeHelpFull(stdoutw, self.stdout_supports_ansi, flags, cmd_stack, mode);
                                        std.process.exit(0); // TODO maybe free memory/run destructors before doing this?
                                        unreachable;
                                    }
                                    // TODO `data` and `result.command.<cmd.fieldName()>` should really be the same type so
                                    // this hack isn't needed.
                                    const data = try self.parseInner(cmds[idx].mode, cmds[idx].flags, cmd_stack ++ .{cmds[idx]});
                                    result.command = @unionInit(@TypeOf(result.command), cmds[idx].fieldName(), switch (cmds[idx].mode) {
                                        .positionals => .{ .positionals = data.positionals, .flags = data.flags },
                                        .commands => .{ .command = data.command, .flags = data.flags },
                                    });
                                    found_command = true;
                                },
                                else => unreachable,
                            },
                        }
                    },
                }
            }
            if (!found_token) {
                if (required_flags.count() != 0) {
                    var stdout = std.io.getStdOut();
                    const stdoutw = stdout.writer();
                    var buf_writer = std.io.bufferedWriter(stdoutw);
                    const real_writer = buf_writer.writer();
                    self.writeHelpFull(real_writer, self.stdout_supports_ansi, flags, cmd_stack, mode) catch {};
                    buf_writer.flush() catch {};
                    std.process.exit(0);
                }
            } else {
                switch (mode) {
                    .positionals => |positionals| {
                        if (comptime positionals.len != 0) {
                            const last_idx = positionals.len - 1 - @intFromBool(has_variadic_positional) - @intFromBool(has_trailing_positionals);
                            if (self.positional_index < last_idx)
                                return self.fail("too few positional arguments found", .{});
                            const maybe_variadic_pos = positionals[positionals.len - 1 - @intFromBool(has_trailing_positionals)];
                            if (comptime maybe_variadic_pos.type != []const u8 and @typeInfo(maybe_variadic_pos.type) == .pointer) {
                                @field(result.positionals, maybe_variadic_pos.fieldName()) = try variadic_positional_state.toOwnedSlice(self.allocator);
                            }
                            const maybe_trailing_pos = positionals[positionals.len - 1];
                            if (!found_trailing_positionals and maybe_trailing_pos.type == argz.Trailing)
                                @field(result.positionals, maybe_trailing_pos.fieldName()) = TrailingPositionals.init(.empty, 0);
                        }
                    },
                    .commands => if (!found_command) return self.fail("no command provided", .{}),
                }
            }
            inline for (0..flags.len) |i| {
                if (!flags_set.isSet(i)) {
                    if (flags[i].type == void) {
                        @field(result.flags, flags[i].fieldName()) = false;
                    } else if (comptime !(flags[i].type == argz.FlagHelp or util.isCounter(flags[i].type) or util.isMulti(flags[i].type))) {
                        if (flags[i].defaultValue()) |default| {
                            @field(result.flags, flags[i].fieldName()) = default;
                        } else {
                            return self.fail("missing required flag: '{s}'", .{flags[i].flagString(if (flags[i].long != null) .long else .short)});
                        }
                    }
                }
            }
            return result;
        }

        pub fn deinitOptions(self: *@This(), opts: Options) void {
            if (!cfg.support_allocation) return;
            inline for (cfg.top_level_flags) |flag| {
                if (flag.hasDynamicValue())
                    values.freeExt(self.allocator, @field(opts, flag.fieldName()));
            }
            freeMode(self.allocator, cfg.mode, switch (cfg.mode) {
                .commands => opts.command,
                .positionals => opts.positionals,
            });
        }

        fn freeMode(allocator: Allocator, mode: Mode, val: anytype) void {
            switch (mode) {
                .commands => |cmds| {
                    const Tag = std.meta.Tag(val);
                    inline for (cmds) |cmd| {
                        const tag = @field(Tag, cmd.fieldName());
                        if (tag == @as(Tag, val)) {
                            const data = @field(val, cmd.fieldName());
                            inline for (cmd.flags) |flag| {
                                if (flag.hasDynamicValue())
                                    values.freeExt(allocator, @field(data.flags, flag.fieldName()));
                            }
                            freeMode(allocator, data.mode, switch (data.mode) {
                                .commands => data.command,
                                .positionals => data.positionals,
                            });
                        }
                    }
                },
                .positionals => |positionals| {
                    inline for (positionals) |positional| {
                        if (util.ArgzType.fromZigType(positional.type).requiresAllocator()) {
                            values.freeExt(allocator, @field(val, positional.fieldName()));
                        }
                    }
                },
            }
        }

        pub fn parse(self: *@This()) !Options {
            return self.parseInner(cfg.mode, cfg.top_level_flags, &.{});
        }

        fn writeHelpFull(
            self: *const @This(),
            writer: anytype,
            use_ansi: bool,
            comptime flags: []const Flag,
            comptime command_stack: []const Command,
            comptime mode: Mode,
        ) !void {
            try cfg.formatters.prologue(
                command_stack,
                mode,
                flags,
                use_ansi,
                cfg.program_name orelse self.lexer.args.get(0),
                cfg.program_description,
                writer,
            );
            switch (mode) {
                .positionals => {},
                .commands => |commands| {
                    try cfg.formatters.commands(commands, use_ansi, writer);
                },
            }
            try cfg.formatters.flags(flags, use_ansi, writer);
        }
    };
}

pub fn argParser(comptime cfg: Config, args: Args, allocator: ?Allocator) !ArgParser(cfg) {
    const stdout_supports_ansi = switch (cfg.ansi_mode) {
        .force => true,
        .detect => std.io.getStdOut().supportsAnsiEscapeCodes(),
        .disable => false,
    };
    const stderr_supports_ansi = switch (cfg.ansi_mode) {
        .force => true,
        .detect => std.io.getStdErr().supportsAnsiEscapeCodes(),
        .disable => false,
    };
    if (cfg.support_allocation)
        if (allocator == null)
            if (@inComptime())
                @compileError("no allocator provided when one is required by the configuration")
            else
                return error.NoAllocatorProvided
        else
            return .{
                .lexer = Lexer{
                    .args = args,
                },
                .stdout_supports_ansi = stdout_supports_ansi,
                .stderr_supports_ansi = stderr_supports_ansi,
                .allocator = allocator.?,
            }
    else
        return .{
            .lexer = Lexer{
                .args = args,
            },
            .stdout_supports_ansi = stdout_supports_ansi,
            .stderr_supports_ansi = stderr_supports_ansi,
            .allocator = undefined,
        };
}
