const std = @import("std");
const root = @import("argz.zig");
const util = @import("util.zig");
const Lexer = @import("Lexer.zig");
const HelpPrinter = @import("help.zig").HelpPrinter;
const values = @import("values.zig");
const ansi = @import("ansi.zig");

const assert = std.debug.assert;

const A = ansi.AnsiFormatter;
const Flag = root.Flag;
const Command = root.Command;
const Mode = root.Mode;
const Config = root.Config;
const Allocator = std.mem.Allocator;
const Args = @import("args.zig").Args;

// TODO:
//  1. Unify command-level and top-level Mode Parsing
//  2. Implement fix suggestions
//  3. Implement optional type parsing
//  4. Implement forceful argument ending
//  5. Implement changes to default initialization (to support strings without having to special-case it)

const Type = std.builtin.Type;

/// Returns a length-zero slice to be used as a dummy when creating `ArrayListUnmanaged` instances.
fn nullSlice(comptime T: type) []T {
    var s = @as([0]T, undefined);
    return &s;
}

pub fn ResolveType(comptime T: type) type {
    comptime return if (util.isBoundedMulti(T))
        ResolveType(@as(T, .{}).__argz_bmulti_backing_type)
    else if (util.isDynamicMulti(T))
        ResolveType(@as(T, .{}).__argz_dmulti_backing_type)
    else if (util.isCounter(T))
        @as(T, .{}).__argz_counter_type
    else if (T == void)
        bool
    else if (util.isPair(T)) blk: {
        const v = @as(T, .{}).__argz_pair_result;
        break :blk struct { v[0], v[1] };
    } else T;
}

fn TypeFromFlags(comptime flags: []const Flag) type {
    comptime var fields = @as([flags.len]Type.StructField, undefined);
    inline for (0.., flags) |i, flag| {
        const FlagType = ResolveType(flag.type);
        fields[i] = Type.StructField{
            .name = flag.fieldName(),
            .type = FlagType,
            .alignment = 0,
            .is_comptime = false,
            .default_value = null,
        };
    }
    return @Type(.{ .@"struct" = .{ .fields = &fields, .layout = .auto, .decls = &.{}, .is_tuple = false } });
}

fn TypeFromMode(comptime mode: Mode) type {
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
                    .standard => struct { positionals: TypeFromMode(cmd.mode), flags: TypeFromFlags(cmd.flags) },
                    .commands => struct { command: TypeFromMode(cmd.mode), flags: TypeFromFlags(cmd.flags) },
                }, .alignment = 0 };
            }

            const EnumType = @Type(.{ .@"enum" = .{
                .fields = &enum_fields,
                .tag_type = std.math.IntFittingRange(0, commands.len),
                .is_exhaustive = true,
                .decls = &.{},
            } });
            return @Type(.{ .@"union" = .{ .fields = &union_fields, .layout = .auto, .tag_type = EnumType, .decls = &.{} } });
        },
        .standard => |positionals| {
            var struct_fields = @as([positionals.len]Type.StructField, undefined);

            for (0.., positionals) |i, pos| {
                struct_fields[i] = Type.StructField{ .name = pos.fieldName(), .type = ResolveType(pos.type), .default_value = null, .is_comptime = false, .alignment = 0 };
            }

            return @Type(.{ .@"struct" = .{ .fields = &struct_fields, .layout = .auto, .decls = &.{}, .is_tuple = false } });
        },
    };
}

fn ArgParser(comptime cfg: root.Config) type {
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

        fn parseValue(self: *const @This(), comptime T: type, string: []const u8) !ResolveType(T) {
            if (cfg.support_allocation)
                return try values.parseDynamicValue(T, self.allocator, string, false)
            else
                return try values.parseStaticValue(T, string);
        }

        // zig fmt: off
        fn handleFlag(
            self: *@This(),
            comptime flags: []const Flag,
            comptime index: usize,
            comptime command_stack: []const Command,
            comptime variant: FlagType,
            comptime mode: Mode,
            val: ?[]const u8,
            flag_data: *TypeFromFlags(flags),
            flags_set: *std.StaticBitSet(flags.len)
        ) !void {
            // zig fmt: on
            const flag = flags[index];
            if (comptime util.isCounter(flag.type)) {
                @field(flag_data, flag.fieldName()) +|= 1;
                return;
            } else if (comptime flag.type == root.FlagHelp) {
                var stdout = std.io.getStdOut();
                const stdoutw = stdout.writer();
                try self.writeHelpFull(stdoutw, self.stdout_supports_ansi, flags, command_stack, mode);
                std.process.exit(0); // TODO maybe free memory/run destructors before doing this?
                return;
            } else if (flags_set.isSet(index) and comptime !(util.isBoundedMulti(flag.type) or util.isDynamicMulti(flag.type))) {
                return self.fail("flag '{s}' was found multiple times", .{flag.flagString(variant)});
            }

            if (comptime util.isBoundedMulti(flag.type)) {
                const Child = @as(flag.type, .{}).__argz_bmulti_child;
                if (val) |v|
                    try @field(flag_data, flag.fieldName()).append(try self.parseValue(Child, v))
                else
                    return self.fail("expected value for flag '{s}'", .{flag.flagString(variant)});
                flags_set.set(index);
                return;
            } else if (comptime util.isDynamicMulti(flag.type)) {
                const Child = @as(flag.type, .{}).__argz_dmulti_child;
                if (val) |v|
                    try @field(flag_data, flag.fieldName()).append(self.allocator, try self.parseValue(Child, v))
                else
                    return self.fail("expected value for flag '{s}'", .{flag.flagString(variant)});
                flags_set.set(index);
                return;
            } else if (flag.type == void) {
                if (val != null) return self.fail("wasn't expecting value '{s}' for flag '{s}'", .{ val.?, flag.flagString(variant) });
                @field(flag_data, flag.fieldName()) = true;
                flags_set.set(index);
                return;
            } else if (comptime util.isPair(flag.type)) {
                if (val) |v| {
                    @field(flag_data, flag.fieldName()) = try self.parseValue(flag.type, v);
                    flags_set.set(index);
                    return;
                } else {
                    return self.fail("expected value for flag '{s}'", .{flag.flagString(variant)});
                }
            } else if (@typeInfo(flag.type) == .optional) {
                @field(flag_data, flag.fieldName()) = if (val) |v| try self.parseValue(flag.type, v) else null;
                flags_set.set(index);
                return;
            } else if (val == null) {
                return self.fail("was expecting a value of type '{s}' for flag '{s}'", .{ flag.typeString(false), flag.flagString(variant) });
            } else {
                @field(flag_data, flag.fieldName()) = try self.parseValue(flag.type, val.?);
                flags_set.set(index);
                return;
            }
        }

        fn handleErr(self: *@This(), err: Lexer.Token.Error, comptime flags: []const Flag, comptime commands: []const Command) ParseError {
            const flagStringWithRuntimeIndex = struct {
                fn func(comptime flags1: []const Flag, comptime variant: FlagType, idx: usize) [:0]const u8 {
                    return switch (idx) {
                        inline 0...flags.len - 1 => |i| {
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
                .unknown_command => |data| self.fail("unknown command '{s}'", .{args.get(data.span.argv_index)}),
                .unknown_long_flag => |data| self.fail("unknown long flag '{s}'", .{args.getSpanText(data.span)}),
                .unknown_short_flag => |data| self.fail("unknown short flag '{s}'", .{args.getSpanText(data.span)}),
                .unexpected_force_stop => self.fail("unexpected force stop found", .{}),
                .empty_argument => self.fail("stray empty argument found", .{}),
            };
        }

        fn fail(self: *const @This(), comptime fmt: []const u8, args: anytype) ParseError {
            const stderr = std.io.getStdErr();
            stderr.lock(.exclusive) catch return error.ArgParseError;
            defer stderr.unlock();
            var stderrw = stderr.writer();
            stderrw.print("{} {}\n", .{
                A(struct { []const u8 }, "{s}", .red, .bold){ .inner = .{"error:"}, .enable = self.stderr_supports_ansi },
                A(@TypeOf(args), fmt, .blue, null){ .inner = args, .enable = self.stderr_supports_ansi },
            }) catch {};
            return error.ArgParseError;
        }

        /// Prints a message depending on the `ParseError` provided, and returns `error.ArgParseError`
        fn handleInternalError(self: *const @This(), comptime flag_data: ?struct { Flag, FlagType }, comptime positional: ?root.Positional, val: ?[]const u8, err: ParseError) ParseError {
            const args2 = if (flag_data) |data| .{ "flag", data[0].flagString(data[1]) } else .{ "positional", positional.?.display };
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
            const ModeData = TypeFromMode(mode);
            const Flags = TypeFromFlags(flags);
            return switch (mode) {
                .commands => struct { command: ModeData, flags: Flags },
                .standard => struct { positionals: ModeData, flags: Flags },
            };
        }

        pub fn parseInner(self: *Self, comptime mode_or_cmd: anytype, comptime flags: []const Flag, comptime cmd_stack: []const Command) ParseError!ParseInnerReturnType(mode_or_cmd, flags) {
            const mode = if (@TypeOf(mode_or_cmd) == Command) mode_or_cmd.mode else mode_or_cmd;
            var result = @as(ParseInnerReturnType(mode_or_cmd, flags), undefined);
            var flags_set = std.StaticBitSet(flags.len).initEmpty();
            var variadic_positional_state = comptime switch (mode) {
                .standard => |positionals| if (positionals.len == 0 and !util.typeHasDynamicValue(positionals[positionals.len - 1].type))
                    std.ArrayListUnmanaged(void).empty
                else
                    std.ArrayListUnmanaged(@typeInfo(positionals[positionals.len - 1].type).pointer.child).empty,
                .commands => std.ArrayListUnmanaged(void).empty,
            };
            errdefer if (cfg.support_allocation) inline for (0.., flags) |i, flag| {
                if (flags_set.isSet(i) and comptime util.typeHasDynamicValue(flag.type)) {
                    values.freeDynamicValue(flag.type, &@field(result.flags, flag.fieldName()), self.allocator, false);
                }
                if (mode == .standard and mode.standard.len != 0) {
                    if (comptime util.typeHasDynamicValue(mode.standard[mode.standard.len - 1].type)) {
                        if (variadic_positional_state.items.len != 0) {
                            values.freeDynamicValue(@TypeOf(variadic_positional_state.items), variadic_positional_state.items, self.allocator, false);
                        }
                    }
                }
            };
            while (self.lexer.nextToken(flags, switch (mode) {
                .standard => .positionals,
                .commands => |cmds| .{ .commands = cmds },
            })) |tok| {
                switch (tok) {
                    .err => |e| return self.handleErr(e, flags, cmd_stack),
                    inline .long_flag, .short_flag => |data, tag| switch (data.index) {
                        inline 0...flags.len - 1 => |idx| self.handleFlag(flags, idx, cmd_stack, if (tag == .long_flag) .long else .short, mode, null, &result.flags, &flags_set) catch |e| return self.handleInternalError(.{ flags[idx], if (tag == .long_flag) .long else .short }, null, null, e),
                        else => unreachable,
                    },
                    inline .long_flag_with_value, .short_flag_with_value => |data, tag| switch (data.index) {
                        inline 0...flags.len - 1 => |idx| self.handleFlag(flags, idx, cmd_stack, if (tag == .long_flag_with_value) .long else .short, mode, self.lexer.args.getSpanText(data.value_span), &result.flags, &flags_set) catch |e| return self.handleInternalError(.{ flags[idx], if (tag == .long_flag) .long else .short }, null, self.lexer.args.getSpanText(data.value_span), e),
                        else => unreachable,
                    },
                    .force_stop => {},
                    .positional => |index| {
                        if (mode != .standard or mode.standard.len == 0)
                            unreachable;
                        const arg = self.lexer.args.get(index.span.argv_index);
                        const positionals = mode.standard;
                        switch (self.positional_index) {
                            inline 0...@max(1, positionals.len) - 1 => |idx| {
                                const pos = positionals[idx];
                                if (pos.type == []const u8 or @typeInfo(pos.type) != .pointer) {
                                    self.positional_index += 1;
                                    @field(result.positionals, pos.fieldName()) = try self.parseValue(pos.type, self.lexer.args.getSpanText(index.span));
                                } else {
                                    comptime assert(idx == positionals.len - 1);
                                    try variadic_positional_state.append(self.allocator, try self.parseValue(@typeInfo(pos.type).pointer.child, arg));
                                }
                            },
                            else => return self.fail("extra positional argument found: '{s}'", .{arg}),
                        }
                    },
                    .command => |index| {
                        if (comptime mode == .standard)
                            unreachable;
                        switch (mode) {
                            .standard => unreachable,
                            .commands => |cmds| switch (index.index) {
                                inline 0...cmds.len - 1 => |idx| {
                                    const data = try self.parseInner(cmds[idx].mode, cmds[idx].flags, cmd_stack ++ .{cmds[idx]});
                                    result.command = @unionInit(@TypeOf(result.command), cmds[idx].fieldName(), switch (cmds[idx].mode) {
                                        .standard => .{ .positionals = data.positionals, .flags = data.flags },
                                        .commands => .{ .command = data.command, .flags = data.flags },
                                    });
                                },
                                else => unreachable,
                            },
                        }
                    },
                }
            }
            switch (mode) {
                .standard => |positionals| {
                    if (comptime positionals.len != 0) {
                        const pos = positionals[positionals.len - 1];
                        if (comptime pos.type != u8 and @typeInfo(pos.type) == .pointer) {
                            @field(result.positionals, positionals[positionals.len - 1].fieldName()) = try variadic_positional_state.toOwnedSlice(self.allocator);
                        }
                    }
                },
                .commands => {},
            }
            // TODO: check all required flags were found and do default initialization
            return result;
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
        ) @TypeOf(writer).Error!void {
            if (command_stack.len != 0)
                if (cfg.program_description) |desc|
                    try writer.writeAll(desc ++ "\n\n");

            try writer.print("Usage: {}{}", .{ A(struct { []const u8 }, "{s}", .blue, .bold){ .inner = .{cfg.program_name orelse self.lexer.args.get(0)}, .enable = use_ansi }, blk: {
                comptime var extra = @as([]const u8, "");
                inline for (command_stack) |cmd| {
                    extra = extra ++ " " ++ cmd.cmd;
                }
                break :blk A(struct { []const u8 }, "{s}", .blue, .bold){ .inner = .{extra}, .enable = use_ansi };
            } });
            switch (mode) {
                .standard => |positionals| {
                    comptime var nb_opt_args = 0;
                    inline for (positionals) |positional| {
                        try writer.writeByte(' ');
                        if (@typeInfo(positional.type) == .optional) {
                            nb_opt_args += 1;
                            try writer.print("{}", .{A(struct { []const u8 }, "{s}", .cyan, .bold){ .inner = .{"["}, .enable = use_ansi }});
                        }
                        try writer.print("{}", .{A(struct { []const u8 }, "{s}", .cyan, .bold){ .inner = .{positional.displayString()}, .enable = use_ansi }});
                    }
                    if (nb_opt_args != 0)
                        try writer.print("{}", .{A(struct { []const u8 }, "{s}", .cyan, .bold){ .inner = .{"]" ** nb_opt_args}, .enable = use_ansi }});
                    try writer.writeByte('\n');
                },
                .commands => |commands| {
                    try writer.print("{}\n", .{A(struct { []const u8 }, "{s}", .cyan, .bold){ .inner = .{"COMMAND"}, .enable = use_ansi }});
                    if (commands.len == 0) {
                        try writer.print("{}\n", .{A(struct { []const u8 }, "{s}", .yellow, .bold){ .inner = .{"Note: this project doesn't provide any valid values for 'COMMAND.' You may want to report this issue to the author(s)."}, .enable = use_ansi }});
                    } else {
                        const cmd_padding = comptime blk: {
                            var max_pad = 0;
                            for (commands) |cmd| {
                                max_pad = @max(max_pad, std.unicode.utf8CountCodepoints(cmd.cmd) catch unreachable);
                            }
                            break :blk max_pad;
                        };
                        try writer.print("{}\n", .{A(struct { []const u8 }, "{s}", .green, .bold){ .inner = .{"COMMAND:"}, .enable = use_ansi }});
                        inline for (commands) |cmd| {
                            try writer.writeAll(" " ** 4);
                            try writer.print("{}", .{A(struct { []const u8 }, "{s}", .cyan, .bold){ .inner = .{cmd.cmd}, .enable = use_ansi }});
                            if (cmd.help_msg) |help| {
                                const cp_len = comptime std.unicode.utf8CountCodepoints(cmd.cmd) catch unreachable;
                                try writer.writeAll(" " ** (cmd_padding - cp_len) ++ help);
                            }
                        }
                    }
                },
            }
            const flag_padding, const flag_type_padding = comptime blk: {
                var max_flag_pad = 0;
                var max_flag_type_pad = 0;
                for (flags) |flag| {
                    var tmp = 0;
                    if (flag.short != null) tmp += 2;
                    if (flag.long) |long| tmp += 2 + (std.unicode.utf8CountCodepoints(long) catch unreachable);
                    if (flag.short != null and flag.long != null) tmp += 2;
                    max_flag_pad = @max(max_flag_pad, tmp);

                    max_flag_type_pad = @max(max_flag_type_pad, if (flag.type != void and flag.type != root.FlagHelp)
                        (std.unicode.utf8CountCodepoints(flag.typeString(false)) catch unreachable) + if (@typeInfo(flag.type) == .optional) 1 else 0
                    else
                        0);
                }
                break :blk .{ max_flag_pad, max_flag_type_pad };
            };
            inline for (flags) |flag| {
                comptime var total_written = 0;
                try writer.writeAll(" " ** 4);
                if (flag.short) |short| {
                    try writer.print("{}", .{A(struct { u21 }, "-{u}", .cyan, .bold){ .inner = .{short}, .enable = use_ansi }});
                    total_written += 2;
                    if (flag.long != null) {
                        try writer.writeAll(", ");
                        total_written += 2;
                    }
                }
                if (flag.long) |long| {
                    try writer.print("{}", .{A(struct { []const u8 }, "--{s}", .cyan, .bold){ .inner = .{long}, .enable = use_ansi }});
                    total_written += comptime std.unicode.utf8CountCodepoints(flag.flagString(.long)) catch unreachable;
                }
                if (flag.type != void and flag.type != root.FlagHelp) {
                    try writer.writeAll(" " ** (flag_padding - total_written + 1));
                    try writer.print("{}", .{A(struct { []const u8 }, if (@typeInfo(flag.type) == .optional)
                        "[={s}]"
                    else
                        "<{s}>", .cyan, .bold){ .inner = .{flag.typeString(false)}, .enable = use_ansi }});
                    if (flag.help_msg) |help| {
                        try writer.writeAll(" " ** (flag_type_padding - (std.unicode.utf8CountCodepoints(flag.typeString(false)) catch unreachable) + if (@typeInfo(flag.type) == .optional) 0 else 1) ++ help);
                    }
                } else if (flag.help_msg) |help| {
                    try writer.writeAll(" " ** (flag_padding - total_written + 4) ++ " " ** flag_type_padding ++ help);
                }
                try writer.writeByte('\n');
            }
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
            return .{ .lexer = Lexer{
                .args = args,
            }, .stdout_supports_ansi = stdout_supports_ansi, .stderr_supports_ansi = stderr_supports_ansi, .allocator = allocator.? }
    else
        return .{ .lexer = Lexer{
            .args = args,
        }, .stdout_supports_ansi = stdout_supports_ansi, .stderr_supports_ansi = stderr_supports_ansi, .allocator = undefined };
}
