const std = @import("std");
const builtin = @import("builtin");
const util = @import("util.zig");
const args = @import("args.zig");

const Type = std.builtin.Type;

pub const Args = args.Args;
pub const OwnedArgs = args.OwnedArgs;
pub const SystemArgs = args.SystemArgs;
pub const Parser = @import("Parser.zig").Parser;
pub const Lexer = @import("Lexer.zig");
pub const fmt = @import("format.zig");

/// Dummy type indicating that positional should capture all arguments found after
/// a force stop ('--'); this must be the last positional in a positional list.
pub const Trailing = struct {};

/// Dummy type indicating that a flag should serve to print a message showing usage information and exit.
pub const FlagHelp = struct {};

pub const Mode = union(enum(u1)) {
    /// Specifies the usage of a set of commands and subcommands (and sub-subcommands, and so on).
    /// Each command has its own set of commands *or* positional arguments and a dedicated set of flags pertaining
    /// to the command's function.
    commands: []const Command,
    /// A "standard" interface. Only allows positional arguments on top of any flags specified in
    /// its respective configuration environment. This is allowed in both the root [Config](argz.Config)
    /// and [Commands](argz.Command) nested at any level.
    positionals: []const Positional,

    pub inline fn ToType(comptime mode: Mode) type {
        switch (mode) {
            .positionals => |positionals| comptime {
                var struct_fields: [positionals.len]Type.StructField = undefined;
                for (positionals, 0..) |positional, i| {
                    const Resolved = util.ArgzType.fromZigType(positional.type).Resolve(.struct_field, .positional);
                    struct_fields[i] = Type.StructField{
                        .name = positional.fieldName(),
                        .type = Resolved,
                        .alignment = 0,
                        .is_comptime = false,
                        .default_value_ptr = if (@typeInfo(Resolved) == .optional) &@as(Resolved, null) else if (@typeInfo(Resolved) == .pointer) @as(*const anyopaque, @ptrCast(&@as(Resolved, &.{}))) else &@as(Resolved, undefined),
                    };
                }
                return @Type(.{ .@"struct" = .{
                    .fields = &struct_fields,
                    .layout = .auto,
                    .decls = &.{},
                    .is_tuple = false,
                } });
            },
            .commands => |cmds| comptime {
                var union_fields: [cmds.len]Type.UnionField = undefined;
                var enum_fields: [cmds.len]Type.EnumField = undefined;
                for (cmds, 0..) |cmd, i| {
                    union_fields[i] = Type.UnionField{
                        .type = cmd.ToType(),
                        .name = cmd.fieldName(),
                        .alignment = 0,
                    };
                    enum_fields[i] = Type.EnumField{
                        .name = cmd.fieldName(),
                        .value = i,
                    };
                }
                return @Type(.{ .@"union" = Type.Union{
                    .fields = &union_fields,
                    .decls = &.{},
                    .layout = .auto,
                    .tag_type = @Type(.{
                        .@"enum" = .{
                            .fields = &enum_fields,
                            .decls = &.{},
                            .tag_type = std.math.IntFittingRange(0, cmds.len),
                            .is_exhaustive = true,
                        },
                    }),
                } });
            },
        }
    }
};

pub const Config = struct {
    /// Top-level flags for the CLI.
    top_level_flags: []const Flag = &.{},
    /// The top-level [Mode](argz.Mode) for the CLI. This determines whether a project
    /// is command-based or flag-based.
    mode: Mode,
    /// Whether to support dynamic memory allocation. If `true`, variable length slices and non-zero sentinel terminated
    /// strings become legal as option types. It is the programmer's responsibility to free them when no longer needed.
    ///
    /// Note that slices are to be passed as comma-separated strings containing the data to be parsed. There is currently
    /// no way to escape commas in slices of strings. Each application must implement their own way of doing so themselves.
    support_allocation: bool = false,
    formatters: struct {
        flags: fmt.AllFlagsFormatFn = fmt.formatAllFlagsDefault,
        commands: fmt.AllCommandsFormatFn = fmt.formatAllCommandsDefault,
        prologue: fmt.PrologueFormatFn = fmt.formatPrologueDefault,
        expanded_help: fmt.ExpandedHelpFormatFn = fmt.formatExpandedHelpDefault,
    } = .{},
};

pub const Command = struct {
    /// The textual representation of the command.
    cmd: [:0]const u8,
    /// Custom flags for the command. Any prior flags will *not* be parsed.
    flags: []const Flag,
    /// The parsing [Mode](argz.Mode) for this command. `.positionals` should be used for bottom-level
    /// commands, i.e. those which should perform a particular action. `.commands` should be used to
    /// to group clusters of related actions.
    mode: Mode,
    /// A brief help message describing the command's usage. Use `info` to provide
    /// a more in-depth help message.
    help_msg: ?[]const u8,
    /// An override for the default field name of the command.
    field_name: ?[:0]const u8,
    /// Whether to designate this flag as a help command. Note that multiple such help commands can
    /// exist in one set of commands, and it is not checked that there is at most one help command.
    is_help: bool,
    /// A detailed string documenting the command's purpose. For use within instances of a `--help=cmd:<command>` flag;
    info: ?[]const u8,
    // TODO: implement command aliases
    // aliases: []const [:0]const u8 = &.{},

    pub const Extra = struct {
        field_name: ?[:0]const u8 = null,
        is_help: bool = false,
        info: ?[]const u8 = null,
    };

    pub const help: Command = .init("help", &.{}, .{ .positionals = &.{} }, "show this help", .{
        .is_help = true,
        .info =
        \\ The 'help' command displays help regarding program usage
        \\ and exits the program gracefully.
        ,
    });

    pub inline fn init(cmd: [:0]const u8, flags: []const Flag, mode: Mode, help_msg: ?[]const u8, extra: Extra) Command {
        if (extra.is_help) {
            if (flags.len != 0)
                @compileError("help command must not have any flags");
            switch (mode) {
                .positionals => |positionals| if (positionals.len != 0)
                    @compileError("help command must not have any positional arguments"),
                .commands => @compileError("help command must have a Mode of '.positionals'"),
            }
        }
        return comptime .{
            .cmd = cmd,
            .flags = flags,
            .mode = mode,
            .help_msg = help_msg,
            .field_name = extra.field_name,
            .is_help = extra.is_help,
            .info = extra.info,
        };
    }

    pub fn fieldName(cmd: Command) [:0]const u8 {
        return cmd.field_name orelse cmd.cmd;
    }

    pub inline fn ToType(comptime cmd: Command) type {
        const Flags = util.StructFromFlags(cmd.flags);
        return switch (cmd.mode) {
            .positionals => struct { flags: Flags, positionals: cmd.mode.ToType() },
            .commands => struct { flags: Flags, cmd: cmd.mode.ToType() },
        };
    }
};

pub fn Pair(comptime First: type, comptime Second: type, comptime _separator: u21) type {
    return struct {
        pub const argz_pair_tag = {};

        pub const lhs_ty = First;
        pub const rhs_ty = Second;
        pub const separator = _separator;
    };
}

/// A sequence of values represented as different arguments. Optional
/// arguments may be placed at the end if they are not absolutely required
/// for parsing.
pub fn Sequence(comptime tys: []const type) type {
    return struct {
        pub const argz_sequence_tag = {};

        pub const items: []const type = tys;
    };
}

pub const Flag = struct {
    /// The short form of the flag. If equal to `null`, `long` must have a valid representation.
    short: ?u21,
    /// The long form of the flag. If equal to `null`, `short` must have a valid representation.
    long: ?[:0]const u8,
    /// The type of the flag. If equal to `void`, then the corresponding `struct` field will be a
    /// boolean indicating whether this flag was found in the argument list.
    type: type,
    /// A default value for the flag.
    default_value_ptr: ?*const anyopaque,
    /// A brief description of the flag's purpose and usage.
    help_msg: ?[]const u8,
    /// A detailed string documenting the flag's purpose. For use within instances of a `--help=flag:<flag>` flag;
    info: ?[]const u8,
    /// The name of the field representing the flag in the resulting flag `struct`. If `null`, the
    /// field name will be equal to the flag's long form, or the short form if no long form was provided.
    field_name: ?[:0]const u8,
    /// An alternative type name to display in place of a flag's type. For example, one might specify
    /// this to be `"PATH"` if a string argument should represent a filesystem path.
    alt_type_name: ?[:0]const u8,
    // A list of potential aliases for this flag. No alias may be the same as another alias or the flag's
    // primary long or short form.
    // TODO implement flag aliases
    // aliases: []const Alias,

    pub const Extra = struct {
        info: ?[]const u8 = null,
        field_name: ?[:0]const u8 = null,
        alt_type_name: ?[:0]const u8 = null,
        // aliases: []const Alias = &.{},
    };

    const Alias = union(enum) {
        long: ?[:0]const u8,
        short: ?u21,
    };

    pub inline fn init(comptime T: type, short: ?u21, long: ?[:0]const u8, default_value: ?Resolve(T), help_msg: ?[]const u8, extra: Extra) Flag {
        return comptime .{
            .short = short,
            .long = long,
            .type = T,
            .default_value_ptr = if (default_value) |dv| @ptrCast(&@as(Resolve(T), dv)) else null,
            .help_msg = help_msg,
            .info = extra.info,
            .field_name = extra.field_name,
            .alt_type_name = extra.alt_type_name,
            //   .aliases = extra.aliases,
        };
    }

    pub inline fn fieldName(comptime flag: Flag) [:0]const u8 {
        return flag.field_name orelse (flag.long orelse std.fmt.comptimePrint("{u}", .{flag.short.?}));
    }

    pub inline fn defaultValue(comptime flag: Flag) ?Resolve(flag.type) {
        return @as(*const Resolve(flag.type), @ptrCast(@alignCast(flag.default_value_ptr orelse return null))).*;
    }

    pub inline fn hasDynamicValue(comptime flag: Flag) bool {
        return util.ArgzType.fromZigType(flag.type).requiresAllocator();
    }

    inline fn Resolve(comptime T: type) type {
        return util.ArgzType.fromZigType(T).Resolve(.struct_field, .flag);
    }

    /// Returns a type string representing the flag's type. Will *not* append suffixes
    /// to an alternate type name if found, and will cause a compile error if `flag.type` is
    /// equal to `void`.
    pub inline fn typeString(comptime flag: Flag, comptime ignore_alternate: bool) [:0]const u8 {
        return if (!ignore_alternate and flag.alt_type_name != null) flag.alt_type_name.? ++ flag.typeStringSuffix() else {
            const check = struct {
                fn func(comptime T: type) [:0]const u8 {
                    return comptime switch (util.ArgzType.fromZigType(T)) {
                        .counter => "COUNTER",
                        .pair => |p| std.fmt.comptimePrint("{s}{u}{s}", .{ func(p.Lhs), p.separator, func(p.Rhs) }),
                        .multi => |m| func(m.child),
                        .zig_primitive => |prim| switch (@typeInfo(prim)) {
                            .void,
                            => unreachable,
                            .int => "INTEGER",
                            .float => "NUMBER",
                            .bool => "BOOLEAN",
                            .pointer => |ptr| if (ptr.child == u8 and ptr.is_const)
                                if (ptr.sentinel()) |sentinel|
                                    if (sentinel != 0)
                                        "STRING \\ " ++ if (sentinel >= 32 and sentinel <= 127) .{sentinel} else std.fmt.comptimePrint("{d}", .{sentinel})
                                    else
                                        "STRING"
                                else
                                    "STRING"
                            else
                                func(ptr.child),
                            .array => |arr| func(arr.child),
                            .@"enum" => |info| blk: {
                                var string: [:0]const u8 = "{" ++ info.fields[0].name;
                                for (info.fields[1..]) |field| {
                                    string = string ++ "|" ++ field.name;
                                }
                                string = string ++ "}";
                                break :blk if (string.len < 60)
                                    string
                                else
                                    // string will likely contribute to a line longer than the terminal width
                                    @typeName(T);
                            },
                            .optional => |info| func(info.child),
                            else => unreachable,
                        },
                        .trailing => unreachable,
                        .flag_help => unreachable,
                        .sequence => |tys| blk: {
                            var result: [:0]const u8 = func(tys[0]);
                            for (tys[1..]) |Ty| {
                                result = result ++ " " ++ func(Ty);
                            }
                            break :blk result;
                        },
                    };
                }
            }.func;
            return check(flag.type) ++ flag.typeStringSuffix();
        };
    }

    inline fn typeStringSuffix(comptime flag: Flag) [:0]const u8 {
        return switch (@typeInfo(flag.type)) {
            .pointer => |ptr| if (ptr.child == u8 and ptr.is_const) "" else "...",
            .array => |arr| "[" ++ (if (@typeInfo(arr.child) == .optional) "≤" else "") ++ std.fmt.comptimePrint("{d}", .{arr.len}) ++ "]",
            else => "",
        };
    }

    pub inline fn flagString(comptime flag: Flag, variant: util.FlagType) [:0]const u8 {
        return switch (variant) {
            .long => "--" ++ (flag.long orelse unreachable),
            .short => std.fmt.comptimePrint("-{u}", .{flag.short orelse unreachable}),
        };
    }

    pub const help: Flag = .init(FlagHelp, 'h', "help", null, "display this help", .{ .info = 
        \\ The '--help' flag displays help regarding program usage.
        \\ An occurance of the '--help' flag will exit the program.
    });
};

pub const Positional = struct {
    /// The positional's type. Can *not* be `void`. May be an optional value if and only if all successive positionals are optional.
    ///
    /// Optionally, the very last positional in a positional list may have the type [Trailing], which will collect every string after
    /// a force-stop sequence (`"--"`). The actual type of the positional will be [TrailingPositionals], which will reference
    /// the strings in the arguments passed to the parser.
    type: type,
    /// The string that will be displayed in parentheses or braces in the CLI's help message. This should
    /// be brief yet descriptive, such as `"PATH"` or `"ITERATIONS"`.
    display: [:0]const u8,
    /// A help string describing the positional argument's use.
    help_msg: ?[]const u8,
    /// The string that will identify the positional's field in the resulting struct. If equal to `null`,
    /// [fieldName] will return `display` instead.
    field_name: ?[:0]const u8,
    /// A detailed string documenting the positional's purpose. For use within instances of a `--help=pos:<positional>` flag;
    info: ?[]const u8 = null,

    pub const Extra = struct {
        field_name: ?[:0]const u8 = null,
        info: ?[:0]const u8 = null,
    };

    pub inline fn init(comptime T: type, display: [:0]const u8, help_msg: ?[:0]const u8, extra: Extra) Positional {
        return .{
            .type = T,
            .display = display,
            .help_msg = help_msg,
            .field_name = extra.field_name,
            .info = extra.info,
        };
    }

    pub inline fn fieldName(comptime pos: Positional) [:0]const u8 {
        return pos.field_name orelse pos.display;
    }

    pub inline fn displayString(comptime pos: Positional) [:0]const u8 {
        return pos.display ++ pos.suffix();
    }

    pub inline fn suffix(comptime pos: Positional) [:0]const u8 {
        return switch (@typeInfo(pos.type)) {
            .array => |arr| std.fmt.comptimePrint("[{d}]", .{arr.len}),
            .pointer => if (pos.type == []const u8) "" else "...",
            else => "",
        };
    }
};

pub inline fn Counter(comptime T: type) type {
    return struct {
        pub const argz_counter_int = T;
    };
}

pub const MultiStorage = union(enum) {
    /// Value is the maximum number of elements that can be parsed.
    bounded: usize,
    dynamic,
};

pub inline fn Multi(comptime T: type, comptime _storage: MultiStorage) type {
    return struct {
        pub const argz_multi_tag = {};

        pub const child = T;
        pub const storage = _storage;
    };
}

pub const TrailingPositionals = struct {
    args: args.Args,
    index: usize,

    const Iterator = struct {
        args: args.Args,
        index: usize,

        pub fn next(it: *Iterator) ?[]const u8 {
            if (it.index >= it.args.len)
                return null;
            const arg = it.args.get(it.index);
            it.index += 1;
            return arg;
        }
    };

    pub fn init(_args: args.Args, index: usize) TrailingPositionals {
        return .{ .args = _args, .index = index };
    }

    pub fn iterator(self: *const TrailingPositionals) Iterator {
        return .{ .args = self.args, .index = self.index };
    }
};
