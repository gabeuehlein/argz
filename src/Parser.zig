const std = @import("std");
const root = @import("root.zig");
const util = @import("util.zig");
const Lexer = @import("Lexer.zig");
const HelpPrinter = @import("help.zig").HelpPrinter;
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

fn checkValidFlags(comptime flags: []const Flag, comptime support_allocation: bool) void {
    comptime for (0.., flags) |i, flag| {
        if (flag.hasDynamicValue(support_allocation) and !support_allocation) {
            @compileError("type '" ++ @typeName(flag.type) ++ "' is not supported without support for dynamic memory allocation");
        } else check: {
            if (flag.field_name == null and flag.long == null) {
                @compileError("flag must be representable with a long flag or have an explicitly set field name; info has been logged using @compileLog");
            }
            if (flag.type == root.FlagHelp)
                break :check;
            if (util.isBoundedMulti(flag.type) or util.isDynamicMulti(flag.type)) {
                const Child = if (util.isBoundedMulti(flag.type))
                    @as(flag.type, .{}).__argz_bmulti_child
                else
                    @as(flag.type, .{}).__argz_dmulti_child;
                switch (@typeInfo(Child)) {
                    .Int, .Float, .Bool, .Array, .Pointer => break :check,
                    else => @compileError("invalid type for flag: '" ++ @typeName(flag.type) ++ "'"),
                }
            } else {
                var optional = false;
                var array_or_slice = false;
                flag_check: switch (@typeInfo(flag.type)) {
                    .Void, .Int, .Float, .Bool => {},
                    .Optional => |opt| if (optional)
                        @compileError("argz does not support parsing nested optionals")
                    else {
                        optional = true;
                        continue :flag_check opt.child;
                    },
                    .pointer => |ptr| if (ptr.size != .Slice)
                        @compileError("argz does not support parsing flags of type '" ++ @typeName(@Type(.{ .pointer = ptr })) ++ "'")
                    else if (ptr.child == []const u8)
                        break :flag_check
                    else if (array_or_slice)
                        @compileError("argz does not support parsing slices of '" ++ @typeName(@Type(.{ .pointer = ptr })) ++ "'")
                    else {
                        array_or_slice = true;
                        optional = false;
                        continue :flag_check ptr.child;
                    },
                }
            }
            const slice_or_array, const optional, const Child = switch (@typeInfo(flag.type)) {
                .Void => break :check,
                .Optional => |opt| .{ false, true, opt.child },
                .Pointer => |ptr| if (ptr.size != .Slice)
                    @compileError("argz does not support parsing non-slice pointer types")
                else if (flag.type == []const u8)
                    break :check
                else
                    .{ true, false, ptr.child },
                .Array => |arr| if (arr.len == 0)
                    @compileError("argz does not support parsing zero-sized arrays")
                else
                    .{ true, false, arr.child },
                .Int, .Float, .Bool => .{ false, false, flag.type },
                else => @compileError("invalid type for flag '" ++ @typeName(flag.type) ++ "' found"),
            };
            switch (@typeInfo(Child)) {
                .Int, .Float, .Bool => {},
                .Optional,
                => if (optional)
                    @compileError("argz does not support parsing nested optionals"),
                .Pointer, .Array => if (Child != []const u8 and slice_or_array)
                    @compileError("argz does not support parsing nested arrays or slices"),
                else => @compileError("invalid type for flag: '" ++ @typeName(flag.type) ++ "'"),
            }
        }
        for (0.., flags) |j, other_flag| {
            if (i == j) continue;
            if (flag.short != null and other_flag.short == flag.short) {
                @compileError("duplicate short flag '-" ++ std.fmt.comptimePrint("{u}", .{flag.short.?} ++ "' found"));
            }

            if (flag.long) |long| {
                if (other_flag.long != null and std.mem.eql(u8, long, other_flag.long.?)) {
                    @compileError("duplicate long flag '--" ++ long ++ "' found");
                }
            }
        }
    };
}

fn checkValidMode(comptime mode: Mode, comptime cfg: Config) void {
    comptime switch (mode) {
        .commands => |cmds| {
            for (cmds) |cmd| {
                if (cmd.cmd.len == 0) @compileError("command text cannot be empty");
                checkValidFlags(cmd.flags, cfg.support_allocation);
                checkValidMode(cmd.mode, cfg.support_allocation);
            }
        },
        .standard => |positionals| {
            var optional = false;
            var array_or_slice = false;
            for (0.., positionals) |i, positional| {
                if (positional.display.len == 0) @compileError("positional display text cannot be empty");
                check: switch (@typeInfo(positional.type)) {
                    .Void => @compileError("positional type cannot be void"),
                    .Int, .Float, .Bool, .Array => {},
                    .pointer => |ptr| if (ptr.size != .Slice)
                        @compileError("argz does not support parsing positionals of type '" ++ @typeName(@Type(.{ .pointer = ptr })) ++ "'")
                    else if (i + 1 != positionals.len)
                        @compileError("variadic positionals must be at the end of the positional list")
                    else if (ptr.child == []const u8)
                        break :check
                    else if (array_or_slice)
                        @compileError("argz does not support parsing slices of type '" ++ @typeName(@Type(.{ .pointer = ptr })) ++ "'")
                    else {
                        array_or_slice = true;
                        optional = false; // ?[]?T is allowed
                        continue :check ptr.child;
                    },
                    .Optional => |opt| if (optional)
                        @compileError("argz does not support parsing nested optional values")
                    else {
                        optional = true;
                        continue :check opt.child;
                    },
                }
            }
        },
    };
}

/// Gets the "real" type of a flag or positional, i.e. the backing
/// type of a *Multi or a Counter. Assumes the type has already been
/// checked, and therefore is valid.
fn ResolveType(comptime T: type) type {
    comptime return if (util.isBoundedMulti(T))
        @as(T, .{}).__argz_bmulti_backing_type
    else if (util.isDynamicMulti(T))
        @as(T, .{}).__argz_dmulti_backing_type
    else if (util.isCounter(T))
        @as(T, .{}).__argz_counter_type
    else if (T == void)
        bool
    else
        T;
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
    return @Type(.{ .Struct = .{ .fields = &fields, .layout = .auto, .decls = &.{}, .is_tuple = false } });
}

/// Assume the mode has already been verified to be correct.
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
                union_fields[i] = Type.UnionField{ .name = cmd.fieldName(), .type = TypeFromMode(cmd.mode), .alignment = 0 };
            }

            const EnumType = @Type(.{ .Enum = .{ .fields = enum_fields, .tag_type = std.math.IntFittingRange(0, commands.len), .is_exhaustive = true } });
            return @Type(.{ .Union = .{ .fields = union_fields, .layout = .auto, .tag_type = EnumType } });
        },
        .standard => |positionals| {
            var struct_fields = @as([positionals.len]Type.StructField, undefined);

            for (0.., positionals) |i, pos| {
                struct_fields[i] = Type.StructField{ .name = pos.fieldName(), .type = ResolveType(pos.type), .default_value = null, .is_comptime = false, .alignment = 0 };
            }

            return @Type(.{ .Struct = .{ .fields = &struct_fields, .layout = .auto, .decls = &.{}, .is_tuple = false } });
        },
    };
}

fn ArgParser(comptime cfg: root.Config) type {
    return struct {
        const Self = @This();

        pub const ParseError = error{ArgParseError} || util.ParseValueError || Allocator.Error || error{Overflow};

        const Options = ParseInnerReturnType(cfg.mode, cfg.top_level_flags);

        lexer: Lexer,
        positional_index: usize = 0,
        stdout_supports_ansi: bool,
        stderr_supports_ansi: bool,
        allocator: Allocator,

        const FlagType = util.FlagType;

        // zig fmt: off
        fn handleFlag(
            self: *@This(),
            comptime flags: []const Flag,
            comptime index: usize,
            comptime command_stack: []const Command,
            comptime variant: FlagType,
            val: ?[]const u8,
            flag_data: *TypeFromFlags(flags),
            flags_set: *std.StaticBitSet(flags.len)
        ) !void {
            _ = command_stack;
            // zig fmt: on
            const flag = flags[index];
            if (comptime util.isCounter(flag.type)) {
                @field(flag_data, flag.fieldName()) +|= 1;
                return;
            } else if (comptime flag.type == root.FlagHelp) {
                @compileError("TODO: printing autogenerated help");
            } else if (!(util.isBoundedMulti(flag.type) or util.isDynamicMulti(flag.type)) and flags_set.isSet(index)) {
                return self.fail("flag '{s}' was found multiple times", .{flag.flagString(variant)});
            }

            if (comptime util.isBoundedMulti(flag.type)) {
                const Child = @as(flag.type, .{}).__argz_bmulti_child;
                if (val) |v|
                    if (comptime flag.hasDynamicValue(cfg.support_allocation))
                        try @field(flag_data, flag.fieldName()).append(try util.parseDynamicValue(Child, self.allocator, v))
                    else
                        try @field(flag_data, flag.fieldName()).append(try util.parseStaticValue(Child, v))
                else if (@typeInfo(Child) == .Optional)
                    try @field(flag_data, flag.fieldName()).append(null);
                flags_set.set(index);
                return;
            } else if (comptime util.isDynamicMulti(flag.type)) {
                const Child = @as(flag.type, .{}).__argz_dmulti_child;
                if (val) |v|
                    if (comptime util.typeHasDynamicValue(Child, .flag, cfg.support_allocation))
                        try @field(flag_data, flag.fieldName()).append(self.allocator, try util.parseDynamicValue(Child, self.allocator, v))
                    else
                        try @field(flag_data, flag.fieldName()).append(self.allocator, try util.parseStaticValue(Child, v))
                else if (@typeInfo(Child) == .Optional)
                    try @field(flag_data, flag.fieldName()).append(self.allocator, null);
                flags_set.set(index);
                return;
            } else if (flag.type == void) {
                if (val != null) return self.fail("wasn't expecting a value for flag '{s}'", .{switch (variant) {
                    .long => "--" ++ flag.long.?,
                    .short => std.fmt.comptimePrint("-{u}", .{flag.short.?}),
                }});
                @field(flag_data, flag.fieldName()) = true;
                flags_set.set(index);
                return;
            } else if (@typeInfo(flag.type) != .Optional and val == null) {
                return self.fail("was expecting a value of type '{s}' for flag '{s}'", .{ flag.typeString(false), switch (variant) {
                    .long => "--" ++ flag.long.?,
                    .short => std.fmt.comptimePrint("-{u}", .{flag.short.?}),
                } });
            } else switch (@typeInfo(flag.type)) {
                .Int, .Float, .Bool => {
                    const v = val.?;
                    @field(flag_data, flag.fieldName()) = try util.parseStaticValue(flag.type, v);
                    flags_set.set(index);
                },
                .Array => |arr| {
                    const v = val.?;
                    @field(flag_data, flag.fieldName()) = if (cfg.support_allocation and arr.child == []const u8)
                        try util.parseSlice(flag.type, self.allocator, v)
                    else
                        try util.parseStaticValue(flag.type, v);
                    flags_set.set(index);
                },
                .Optional => |opt| {
                    if (val) |v|
                        @field(flag_data, flag.fieldName()) = if (comptime util.typeHasDynamicValue(opt.child, .flag, cfg.support_allocation))
                            try util.parseSlice(opt.child, self.allocator, v)
                        else
                            try util.parseStaticValue(opt.child, v)
                    else
                        @field(flag_data, flag.fieldName()) = null;
                    flags_set.set(index);
                },
                else => @compileError("TODO: handle type '" ++ @typeName(flag.type) ++ "'"),
            }
        }

        fn handleErr(self: *@This(), err: Lexer.Token.Error) ParseError {
            const args = self.lexer.args;
            return switch (err) {
                .value_for_flag_with_no_arg => |data| self.fail("unexpected value '{s}' found for flag '{s}'", .{ args.getSpanText(data.value_span), args.getSpanText(data.flag_span) }),
                .expected_value_for_flag => |span| self.fail("expected value for flag '{s}'", .{args.getSpanText(span)}),
                .unexpected_positional => |idx| self.fail("found unexpected positional '{s}'", .{args.get(idx)}),
                .unknown_command => |idx| self.fail("unknown command '{s}'", .{args.get(idx)}),
                .unknown_long_flag => |span| self.fail("unknown long flag '{s}'", .{args.getSpanText(span)}),
                .unknown_short_flag => |span| self.fail("unknown short flag '{s}'", .{args.getSpanText(span)}),
                .unexpected_force_stop => self.fail("unexpected force stop found", .{}),
                .empty_argument => self.fail("stray empty argument found", .{}),
                .short_flag_invalid_utf8 => self.fail("short flag contains invalid UTF-8", .{}),
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
            };
        }

        /// Assumes that the flags and the mode have already been checked for validity.
        fn ParseInnerReturnType(comptime mode_or_cmd: anytype, comptime flags: []const Flag) type {
            const ModeData = TypeFromMode(switch (@TypeOf(mode_or_cmd)) {
                Command => mode_or_cmd.mode,
                Mode => mode_or_cmd,
                else => comptime unreachable,
            });
            const Flags = TypeFromFlags(flags);
            return switch (@TypeOf(mode_or_cmd)) {
                Command => struct { command: ModeData, flags: Flags },
                Mode => struct { positionals: ModeData, flags: Flags },
                else => comptime unreachable,
            };
        }

        pub fn parseInner(self: *Self, comptime mode_or_cmd: anytype, comptime flags: []const Flag, comptime cmd_stack: []const Command) ParseError!ParseInnerReturnType(mode_or_cmd, flags) {
            const mode = if (@TypeOf(mode_or_cmd) == Command) mode_or_cmd.mode else mode_or_cmd;
            var result = @as(ParseInnerReturnType(mode_or_cmd, flags), undefined);
            var flags_set = std.StaticBitSet(flags.len).initEmpty();
            errdefer if (cfg.support_allocation) inline for (0.., flags) |i, flag| fail: {
                if (flags_set.isSet(i) and comptime flag.hasDynamicValue(true)) {
                    if (comptime util.isDynamicMulti(flag.type)) {
                        const Child = @as(flag.type, .{}).__argz_dmulti_child;
                        if (@typeInfo(Child) == .Pointer) {
                            const ChildChild = @typeInfo(Child).Pointer.child;
                            for (@field(result.flags, flag.fieldName()).items) |elem| {
                                if (ChildChild == []const u8) {
                                    for (elem) |subelem| {
                                        self.allocator.free(subelem);
                                    }
                                }
                                self.allocator.free(elem);
                            }
                        }
                        @field(result.flags, flag.fieldName()).deinit(self.allocator);
                        break :fail;
                    } else if (comptime util.isBoundedMulti(flag.type)) {
                        if (cfg.support_allocation and @typeInfo(@as(flag.type, .{}).__argz_dmulti_child) == .Pointer) {
                            for (@field(result.flags, flag.fieldName()).items) |elem| {
                                self.allocator.free(elem);
                            }
                        }
                    }
                    switch (comptime @typeInfo(flag.type)) {
                        .Array => |arr| {
                            comptime assert(arr.child == []const u8);
                            for (@field(result.flags, flag.fieldName())) |elem| {
                                self.allocator.free(elem);
                            }
                        },
                        .Pointer => |ptr| {
                            comptime assert(ptr.child == []const u8);
                            for (@field(result.flags, flag.fieldName())) |elem| {
                                self.allocator.free(elem);
                            }
                            self.allocator.free(@field(result.flags, flag.fieldName()));
                        },
                        else => unreachable,
                    }
                }
            };
            while (self.lexer.next(flags, mode, cfg.support_allocation)) |tok| {
                switch (tok) {
                    .err => |e| return self.handleErr(e),
                    inline .long_flag, .short_flag => |index, tag| switch (index) {
                        inline 0...flags.len - 1 => |idx| self.handleFlag(flags, idx, cmd_stack, if (tag == .long_flag) .long else .short, null, &result.flags, &flags_set) catch |e| return self.handleInternalError(.{ flags[idx], if (tag == .long_flag) .long else .short }, null, null, e),
                        else => unreachable,
                    },
                    inline .long_flag_with_value, .short_flag_with_value => |data, tag| switch (data.index) {
                        inline 0...flags.len - 1 => |idx| self.handleFlag(flags, idx, cmd_stack, if (tag == .long_flag_with_value) .long else .short, self.lexer.args.getSpanText(data.value_span), &result.flags, &flags_set) catch |e| return self.handleInternalError(.{ flags[idx], if (tag == .long_flag) .long else .short }, null, self.lexer.args.getSpanText(data.value_span), e),
                        else => unreachable,
                    },
                    .force_stop => {},
                    .positional => |index| {
                        const arg = self.lexer.args.get(index);
                        const positionals = mode.standard;
                        switch (self.positional_index) {
                            inline 0...@max(1, positionals.len) - 1 => |idx| {
                                const pos = positionals[idx];
                                if (@typeInfo(pos.type) != .Pointer) {
                                    self.positional_index += 1;
                                }
                            },
                            else => return self.fail("extra positional argument foud: '{s}'", .{arg}),
                        }
                    },
                    inline else => |_, tag| @panic("TODO: " ++ @tagName(tag)),
                }
            }
            // TODO: check all required flags were found and do default initialization
            return result;
        }

        pub fn parse(self: *@This()) !Options {
            return self.parseInner(cfg.mode, cfg.top_level_flags, &.{});
        }
    };
}

pub fn argParser(comptime cfg: Config, args: Args, allocator: ?Allocator) !ArgParser(cfg) {
    comptime checkValidFlags(cfg.top_level_flags, cfg.support_allocation);
    comptime checkValidMode(cfg.mode, cfg);

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
