const std = @import("std");
const root = @import("root.zig");
const util = @import("util.zig");
const tokenizer = @import("Tokenizer.zig");
const HelpPrinter = @import("help.zig").HelpPrinter;
const ansi = @import("ansi.zig");

const T = ansi.T;
const C = ansi.C;

// TODO:
//  1. Unify command-level and top-level Mode Parsing
//  2. Implement fix suggestions
//  3. Implement optional type parsing
//  4. Implement forceful argument ending
//  5. Implement changes to default initialization (to support strings without having to special-case it)

const Type = std.builtin.Type;

/// Returns a length-zero slice to be used as a dummy when creating
fn nullSlice(comptime Ty: type) []Ty {
    var s = @as([0]Ty, undefined);
    return &s;
}

fn checkFlags(comptime flags: []const root.Flag, comptime support_allocation: bool, comptime help: root.Help) void {
    inline for (0.., flags) |i, flag| {
        util.checkValidFlagType(flag.type, support_allocation);

        if (help.short)
            if (flag.short == 'h')
                @compileError("short form of help flag (`-h`) enabled, but it clashes with custom `-h` flag");

        if (help.long)
            if (flag.long) |long|
                if (std.mem.eql(u8, long, "help"))
                    @compileError("long form of help flag (`--help`) enabled, but it clashes with custom `--help` flag");

        inline for (0.., flags) |j, other_flag| {
            if (i == j) continue;
            if (flag.short) |short0|
                if (other_flag.short) |short1|
                    if (short0 == short1)
                        @compileError("duplicate short flag `" ++ std.unicode.utf8EncodeComptime(short0) ++ "`");
            if (flag.long) |long0|
                if (other_flag.long) |long1|
                    if (std.mem.eql(u8, long0, long1))
                        @compileError("duplicate long flag `" ++ long0 ++ "`");

            if (std.mem.eql(u8, flag.field_name, other_flag.field_name))
                @compileError("duplicate field name for flag: `" ++ flag.field_name ++ "`");
        }
    }
}

fn checkMode(comptime mode: root.Mode, cfg: root.Config) void {
    switch (mode) {
        .commands => |commands| {
            inline for (commands) |cmd| {
                if (cfg.force_utf8)
                    if (!std.unicode.utf8ValidateSlice(cmd.cmd))
                        @compileError("command is not valid UTF-8");
                if (cmd.help.command) {
                    if (std.mem.eql(u8, cmd.cmd, "help"))
                        @compileError("command form of `help` clashes with custom command identifiable by the string `help`");
                    if (cmd.mode == .standard)
                        @compileError("command form of `help` is not supported with `standard` configuration");
                }
                checkMode(cmd.mode, cfg);
                checkFlags(cmd.flags, cfg.support_allocation, cmd.help);
            }
        },
        .standard => |positionals| {
            comptime var found_optional = false;
            inline for (0.., positionals) |i, positional| {
                util.checkValidPositionalType(positional.type, i + 1 == positionals.len);
                if (found_optional)
                    @compileError("stray optional type found in positional list; optional positionals may only be followed by optional positionals");
                if (@typeInfo(positional.type) == .Optional) {
                    found_optional = true;
                } else if (cfg.force_utf8) {
                    if (!std.unicode.utf8ValidateSlice(positional.display))
                        @compileError("positional name is not valid UTF-8");
                }
                inline for (0.., positionals) |j, other_positional| {
                    if (i == j) continue;
                    if (std.mem.eql(u8, positional.field_name, other_positional.field_name))
                        @compileError("duplicate field name found when checking validity of positionals: `" ++ positional.field_name ++ "`");
                }
            }
        },
    }
}

/// Returns a `struct` representing every element in `flags`.
/// Assumes the flags have already been verified to be of proper form.
pub fn typeFromFlags(comptime flags: []const root.Flag) type {
    comptime var struct_fields = @as([flags.len]Type.StructField, undefined);
    inline for (0.., flags) |i, flag| {
        const Ty, const default_value = if (comptime util.isCounter(flag.type)) blk: {
            const CounterType = util.getCounterType(flag.type);
            break :blk .{ CounterType, &@as(CounterType, 0) };
        } else if (comptime util.isMulti(flag.type)) blk: {
            const MultiType = util.getMultiType(flag.type);
            const ChildType = util.getMultiChildType(flag.type);
            break :blk .{ MultiType, if (util.getMultiStackLen(flag.type)) |size| &@unionInit(MultiType, "stack", std.BoundedArray(ChildType, size).init(0) catch unreachable) else &MultiType.initBuffer(nullSlice(ChildType)) };
        } else if (flag.type == bool)
            .{ bool, null }
        else
            .{ flag.type, null };
        struct_fields[i] = Type.StructField{
            .alignment = 0,
            .name = flag.field_name,
            .type = Ty,
            .default_value = default_value,
            .is_comptime = false,
        };
    }
    return @Type(.{ .Struct = Type.Struct{ .layout = .auto, .decls = &.{}, .fields = &struct_fields, .is_tuple = false } });
}

pub fn typeFromMode(mode: root.Mode) type {
    comptime return switch (mode) {
        .commands => |cmds| blk: {
            var union_fields = @as([cmds.len]Type.UnionField, undefined);
            var enum_fields = @as([cmds.len]Type.EnumField, undefined);

            for (0.., cmds) |i, cmd| {
                union_fields[i] = Type.UnionField{
                    .name = cmd.field_name orelse cmd.cmd,
                    .type = typeFromCommand(cmd),
                    .alignment = 0,
                };
                enum_fields[i] = Type.EnumField{
                    .name = cmd.field_name orelse cmd.cmd,
                    .value = i,
                };
            }
            const EnumTy = @Type(.{ .Enum = Type.Enum{ .decls = &.{}, .fields = &enum_fields, .tag_type = std.math.IntFittingRange(0, cmds.len), .is_exhaustive = true } });
            break :blk @Type(.{ .Union = Type.Union{
                .decls = &.{},
                .layout = .auto,
                .fields = &union_fields,
                .tag_type = EnumTy,
            } });
        },
        .standard => |positionals| blk: {
            var struct_fields = @as([positionals.len]Type.StructField, undefined);
            for (0.., positionals) |i, positional| {
                struct_fields[i] = Type.StructField{ .name = positional.field_name, .type = positional.type, .alignment = 0, .is_comptime = false, .default_value = null };
            }
            break :blk @Type(.{ .Struct = Type.Struct{
                .decls = &.{},
                .layout = .auto,
                .fields = &struct_fields,
                .is_tuple = false,
            } });
        },
    };
}

pub fn typeFromCommand(cmd: root.Command) type {
    const FlagsType = typeFromFlags(cmd.flags);
    const ModeType = typeFromMode(cmd.mode);
    return switch (cmd.mode) {
        .commands => struct { flags: FlagsType, command: ModeType },
        .standard => struct { flags: FlagsType, positionals: ModeType },
    };
}

fn ArgParser(comptime cfg: root.Config) type {
    const TopLevelFlags = typeFromFlags(cfg.top_level_flags);
    const TopLevelMode = typeFromMode(cfg.mode);

    const Options = switch (cfg.mode) {
        .commands => struct { flags: TopLevelFlags, command: TopLevelMode = undefined },
        .standard => struct { flags: TopLevelFlags, positionals: TopLevelMode = undefined },
    };

    return struct {
        const Self = @This();

        /// The number of errors encountered during parsing.
        qty_errors: u32 = 0,
        tokenizer: root.Tokenizer(cfg),
        /// The state that is manipulated as the parser chews through the arguments provided. If
        /// an error is returned during parsing, this is left in an indeterminate state.
        state: Options = undefined,
        top_level_flags_set: std.StaticBitSet(cfg.top_level_flags.len) = std.StaticBitSet(cfg.top_level_flags.len).initEmpty(),
        stdout_supports_ansi: bool,
        stderr_supports_ansi: bool,
        allocator: std.mem.Allocator,

        fn parseType(self: *@This(), comptime Ty: type, comptime for_short_flag: bool) !Ty {
            return blk: {
                switch (@typeInfo(Ty)) {
                    .Optional => |opt| {
                        if (!self.eatMaybe(&.{.equals}))
                            return null;
                        const tok = self.tokenizer.restOfCurrentToken() orelse return error.ValueExpected;
                        defer _ = self.tokenizer.nextArg(true);
                        break :blk util.parseStaticValue(opt.child, tok) catch return self.fail("invalid value for type `{s}`: {s}", .{ @typeName(Ty), tok });
                    },
                    else => {
                        if (for_short_flag) {
                            self.tokenizer.in_short_chain = false;
                            _ = self.eatMaybe(&.{
                                .equals,
                                .new_arg,
                            });
                        } else _ = try self.expect(&.{ .equals, .new_arg });
                        // TODO implement parsing with dynamic allocation
                        if (for_short_flag) {
                            const tok = try self.expect(&.{.other});
                            break :blk util.parseStaticValue(Ty, tok.other) catch return self.fail("invalid value for type `{s}`: {s}", .{ @typeName(Ty), tok.other });
                        } else {
                            const tok = try self.expect(&.{.other});
                            break :blk util.parseStaticValue(Ty, tok.other) catch return self.fail("invalid value for type `{s}`: {s}", .{ @typeName(Ty), tok.other });
                        }
                    },
                }
            };
        }

        fn longFlag(self: *@This(), comptime flags: []const root.Flag, flag_repr: []const u8, flags_data: anytype, flags_set_data: anytype) !void {
            inline for (flags, 0..) |flag, i| {
                if (flag.long) |long| {
                    if (std.mem.eql(u8, flag_repr, long)) {
                        if (comptime util.isMulti(flag.type)) {
                            if (cfg.support_allocation) {
                                try @field(flags_data, flag.field_name).append(self.allocator, try self.parseType(comptime util.getMultiChildType(flag.type), false));
                            } else {
                                try @field(flags_data, flag.field_name).appendNoAlloc(try self.parseType(comptime util.getMultiChildType(flag.type), false));
                            }
                            flags_set_data.set(i);
                            return;
                        } else if (comptime util.isCounter(flag.type)) {
                            @field(flags_data, flag.field_name) +|= 1;
                            return;
                        } else if (flags_set_data.isSet(i)) {
                            return self.fail("multiple instances of long flag '--{s}'", .{flag_repr});
                        } else {
                            @field(flags_data, flag.field_name) = try self.parseType(flag.type, false);
                            flags_set_data.set(i);
                            return;
                        }
                    }
                }
            }
            return self.fail("unknown long flag: '--{s}'", .{flag_repr});
        }

        fn shortFlag(self: *@This(), comptime flags: []const root.Flag, flag_repr: u21, flags_data: anytype, flags_set_data: anytype) !void {
            inline for (flags, 0..) |flag, i| {
                if (flag.short) |short| {
                    if (flag_repr == short) {
                        if (comptime util.isMulti(flag.type)) {
                            if (cfg.support_allocation) {
                                try @field(flags_data, flag.field_name).append(self.allocator, try self.parseType(comptime util.getMultiChildType(flag.type), true));
                            } else {
                                try @field(flags_data, flag.field_name).appendNoAlloc(try self.parseType(comptime util.getMultiChildType(flag.type), true));
                            }
                            flags_set_data.set(i);
                            return;
                        } else if (comptime util.isCounter(flag.type)) {
                            @field(flags_data, flag.field_name) +|= 1;
                            return;
                        } else if (flags_set_data.isSet(i)) {
                            return self.fail("multiple instances of short flag '-{u}'", .{flag_repr});
                        } else {
                            @field(flags_data, flag.field_name) = try self.parseType(flag.type, true);
                            flags_set_data.set(i);
                            return;
                        }
                    }
                }
            }
            return self.fail("unknown short flag '-{u}'", .{flag_repr});
        }

        fn positional(_: *@This(), state: anytype, comptime positionals: []const root.Positional, comptime flag_index: usize, string: []const u8) !void {
            // TODO variadic positionals
            @field(state, positionals[flag_index].field_name) = try util.parseStaticValue(positionals[flag_index].type, string);
        }

        pub fn subcommand(self: *@This(), comptime cmd: root.Command, comptime cmd_stack: []const root.Command, cmd_flag_data: anytype) !typeFromCommand(cmd) {
            var retval = @as(typeFromCommand(cmd), undefined);
            var positional_index = @as(usize, 0);
            _ = &positional_index;
            _ = &retval;
            top: while (self.tokenizer.nextToken()) |tok| {
                switch (tok) {
                    .long => |long| if (cmd.help.long and std.mem.eql(u8, long, "help"))
                        self.printHelpForCommandAndExit(cmd, cmd_stack)
                    else
                        try self.longFlag(cmd.flags, long, &retval.flags, cmd_flag_data),
                    .short => |short| if (cmd.help.short and short == 'h')
                        self.printHelpForCommandAndExit(cmd, cmd_stack)
                    else
                        try self.shortFlag(cmd.flags, short, &retval.flags, cmd_flag_data),
                    .other => |other| switch (cmd.mode) {
                        .commands => |cmds| {
                            inline for (cmds) |cmd0| {
                                if (std.mem.eql(u8, other, cmd0.cmd)) {
                                    const FlagSet = std.StaticBitSet(cmd0.flags.len);
                                    var flags = FlagSet.initEmpty();
                                    retval.command = @unionInit(@TypeOf(retval.command), cmd0.field_name orelse cmd0.cmd, try self.subcommand(cmd0, cmd_stack ++ [1]root.Command{cmd}, &flags));
                                    continue :top;
                                }
                            }
                            return self.fail("unknown command '{s}'", .{other});
                        },
                        .standard => |positionals| if (positional_index >= positionals.len)
                            return self.fail("too many positional arguments provided", .{})
                        else inline for (0..positionals.len) |i| {
                            if (i == positional_index) {
                                // TODO variadic positionals
                                try self.positional(&retval.positionals, cmd.mode.standard, i, other);
                                positional_index += 1;
                            }
                        },
                    },
                    .new_arg => {},
                    .force_end => @panic("TODO: forceful stopping of flag interpretation"),
                    else => |tk| return self.fail("expected flag, word, or force stop, found '{s}'", .{@as(tokenizer.TokenTag, tk).stringRepr()}),
                }
            }
            if (cmd.mode == .standard and cmd.mode.standard.len != positional_index)
                return self.fail("not enough positionals provided", .{});

            inline for (cmd.flags, 0..) |flag, i| {
                if (!cmd_flag_data.isSet(i)) {
                    if (!(util.isCounter(flag.type) or util.isMulti(flag.type))) {
                        if (flag.default_value) |default| {
                            if (flag.type == []const u8) {
                                const ptr = @as([*:0]const u8, @ptrCast(default));
                                comptime var len = 0;
                                inline while (ptr[len] != 0) : (len += 1) {}
                                @field(retval.flags, flag.field_name) = ptr[0..len];
                            } else @field(retval.flags, flag.field_name) = @as(*const flag.type, @ptrCast(@alignCast(default))).*;
                        } else {
                            return self.fail("missing required flag '{s}'", .{if (flag.long) |long| "--" ++ long else std.fmt.comptimePrint("-{u}", .{flag.short.?})});
                        }
                    }
                }
            }
            return retval;
        }

        pub fn parse(self: *@This()) !Options {
            var positional_index = @as(usize, 0);
            top: while (self.tokenizer.nextToken()) |tok| {
                switch (tok) {
                    .long => |long| if (cfg.help.long and std.mem.eql(u8, long, "help"))
                        self.printHelpAndExit()
                    else
                        try self.longFlag(cfg.top_level_flags, long, &self.state.flags, &self.top_level_flags_set),
                    .short => |short| if (cfg.help.short and short == 'h')
                        self.printHelpAndExit()
                    else
                        try self.shortFlag(cfg.top_level_flags, short, &self.state.flags, &self.top_level_flags_set),
                    .other => |other| switch (cfg.mode) {
                        .commands => |cmds| {
                            inline for (cmds) |cmd| {
                                if (std.mem.eql(u8, other, cmd.cmd)) {
                                    const FlagSet = std.StaticBitSet(cmd.flags.len);
                                    var flags = FlagSet.initEmpty();
                                    self.state.command = @unionInit(@TypeOf(self.state.command), cmd.field_name orelse cmd.cmd, try self.subcommand(cmd, &.{}, &flags));
                                    continue :top;
                                }
                            }
                            return self.fail("unknown command '{s}'", .{other});
                        },
                        .standard => |positionals| if (positional_index >= positionals.len)
                            return self.fail("too many positional arguments provided", .{})
                        else inline for (0..positionals.len) |i| {
                            if (i == positional_index) {
                                // TODO variadic positionals
                                try self.positional(&self.state.positionals, cfg.mode.standard, i, other);
                                positional_index += 1;
                            }
                        },
                    },
                    .new_arg => {},
                    .force_end => @panic("TODO: forceful stopping of flag interpretation"),
                    else => |tk| return self.fail("expected flag, word, or force stop, found '{s}'", .{@as(tokenizer.TokenTag, tk).stringRepr()}),
                }
            }
            if (cfg.mode == .standard and cfg.mode.standard.len != positional_index)
                return self.fail("not enough positionals provided", .{});

            inline for (cfg.top_level_flags, 0..) |flag, i| {
                if (!self.top_level_flags_set.isSet(i)) {
                    if (!(util.isCounter(flag.type) or util.isMulti(flag.type))) {
                        if (flag.default_value) |default| {
                            if (flag.type == []const u8) {
                                const ptr = @as([*:0]const u8, @ptrCast(default));
                                comptime var len = 0;
                                inline while (ptr[len] != 0) : (len += 1) {}
                                @field(self.state.flags, flag.field_name) = ptr[0..len];
                            } else @field(self.state.flags, flag.field_name) = @as(*const flag.type, @ptrCast(@alignCast(default))).*;
                        } else {
                            return self.fail("missing required flag '{s}'", .{if (flag.long) |long| "--" ++ long else std.fmt.comptimePrint("-{u}", .{flag.short.?})});
                        }
                    }
                }
            }
            return self.state;
        }

        pub fn expect(self: *@This(), comptime possible_types: []const tokenizer.TokenTag) error{UnexpectedToken}!tokenizer.TokenKind {
            const err_fmt = blk: {
                comptime var fmt = @as([]const u8, "expected");
                if (possible_types.len == 1) {
                    fmt = fmt ++ " " ++ (comptime possible_types[0].stringRepr()) ++ ",";
                } else if (possible_types.len == 2) {
                    fmt = fmt ++ " " ++ (comptime possible_types[0].stringRepr()) ++ " or " ++ (comptime possible_types[1].stringRepr()) ++ ",";
                } else inline for (possible_types, 0..) |ty, i| {
                    fmt = fmt ++ " " ++ (comptime ty.stringRepr()) ++ ",";
                    if (i == possible_types.len - 1)
                        fmt = fmt ++ " or";
                }
                break :blk fmt ++ " but found '{s}'\n";
            };
            const arg = self.tokenizer.nextToken() orelse return self.fail(err_fmt, .{"end of arguments"});
            for (possible_types) |p| {
                if (arg == p) {
                    return arg;
                }
            }
            return self.fail(err_fmt, .{@as(tokenizer.TokenTag, arg).stringRepr()});
        }

        fn fail(self: *const @This(), comptime fmt: []const u8, args: anytype) error{UnexpectedToken} {
            const stderr = std.io.getStdErr();
            stderr.lock(.exclusive) catch return error.UnexpectedToken;
            defer stderr.unlock();
            if (self.stderr_supports_ansi)
                stderr.writer().print("{} {}\n", .{ T(.{C(.{"error:"}, "{s}", .red, true)}, "{}", .bold, true), C(args, fmt, .blue, true) }) catch return error.UnexpectedToken
            else
                stderr.writer().print("{} {}\n", .{ T(.{C(.{"error:"}, "{s}", .red, false)}, "{}", .bold, false), C(args, fmt, .blue, false) }) catch return error.UnexpectedToken;
            return error.UnexpectedToken;
        }

        pub fn eatMaybe(self: *@This(), comptime tokens: []const tokenizer.TokenTag) bool {
            inline for (tokens) |tag| {
                if (self.tokenizer.peek()) |tok| {
                    if (tok == tag) {
                        self.tokenizer.current_token = null;
                        return true;
                    }
                } else break;
            }
            return false;
        }

        pub fn printHelpAndExit(self: *const @This()) noreturn {
            const stdout = std.io.getStdOut();
            var stdoutw = stdout.writer();
            const supports_ansi = switch (cfg.ansi_mode) {
                .force => true,
                .detect => stdout.supportsAnsiEscapeCodes(),
                .disable => false,
            };
            if (supports_ansi)
                stdoutw.print("{}", .{HelpPrinter(self.tokenizer.program_name, cfg, .top_level, true)}) catch {}
            else
                stdoutw.print("{}", .{HelpPrinter(self.tokenizer.program_name, cfg, .top_level, false)}) catch {};
            std.process.exit(0);
        }

        pub fn printHelpAndExitErr(self: *const @This()) noreturn {
            const stderr = std.io.getStdOut();
            var stderrw = stderr.writer();
            const supports_ansi = switch (cfg.ansi_mode) {
                .force => true,
                .detect => stderr.supportsAnsiEscapeCodes(),
                .disable => false,
            };
            if (supports_ansi)
                stderrw.print("{}", .{HelpPrinter(self.tokenizer.program_name, cfg, .top_level, true)}) catch {}
            else
                stderrw.print("{}", .{HelpPrinter(self.tokenizer.program_name, cfg, .top_level, false)}) catch {};
            std.process.exit(1);
        }

        pub fn printHelpForCommandAndExit(self: *const @This(), comptime cmd: root.Command, comptime cmd_stack: []const root.Command) noreturn {
            const stdout = std.io.getStdOut();
            var stdoutw = stdout.writer();
            const supports_ansi = switch (cfg.ansi_mode) {
                .force => true,
                .detect => stdout.supportsAnsiEscapeCodes(),
                .disable => false,
            };
            if (supports_ansi)
                stdoutw.print("{}", .{HelpPrinter(self.tokenizer.program_name, cfg, .{ .command = .{ .current = cmd, .cmd_stack = cmd_stack } }, true)}) catch {}
            else
                stdoutw.print("{}", .{HelpPrinter(self.tokenizer.program_name, cfg, .{ .command = .{ .current = cmd, .cmd_stack = cmd_stack } }, false)}) catch {};
            std.process.exit(0);
        }

        pub fn printHelpForCommandAndExitErr(self: *const @This(), comptime cmd: root.Command, comptime cmd_stack: []const root.Command) noreturn {
            const stderr = std.io.getStdOut();
            var stderrw = stderr.writer();
            const supports_ansi = switch (cfg.ansi_mode) {
                .force => true,
                .detect => stderr.supportsAnsiEscapeCodes(),
                .disable => false,
            };
            if (supports_ansi)
                stderrw.print("{}", .{HelpPrinter(self.tokenizer.program_name, cfg, .{ .command = .{ .current = cmd, .cmd_stack = cmd_stack } }, true)}) catch {}
            else
                stderrw.print("{}", .{HelpPrinter(self.tokenizer.program_name, cfg, .{ .command = .{ .current = cmd, .cmd_stack = cmd_stack } }, false)}) catch {};
            std.process.exit(1);
        }
    };
}

pub fn argParser(comptime cfg: root.Config, args: tokenizer.Args, allocator: ?std.mem.Allocator) !ArgParser(cfg) {
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
            return error.NoAllocatorProvided
        else
            return .{ .tokenizer = try tokenizer.Tokenizer(cfg).init(args), .stdout_supports_ansi = stdout_supports_ansi, .stderr_supports_ansi = stderr_supports_ansi, .allocator = allocator.? }
    else
        return .{ .tokenizer = try tokenizer.Tokenizer(cfg).init(args), .stdout_supports_ansi = stdout_supports_ansi, .stderr_supports_ansi = stderr_supports_ansi, .allocator = undefined };
}
