const std = @import("std");
const builtin = @import("builtin");
const args = @import("args.zig");

const Type = std.builtin.Type;

pub const Args = args.Args;
pub const OwnedArgs = args.OwnedArgs;
pub const SystemArgs = args.SystemArgs;
pub const Parser = @import("Parser.zig").Parser;
pub const Lexer = @import("Lexer.zig");
pub const fmt = @import("format.zig");
pub const types = @import("types.zig");

pub const Mode = union(enum(u1)) {
    /// Specifies the usage of a set of commands and subcommands (and sub-subcommands, and so on).
    /// Each command has its own set of commands *or* positional arguments and a dedicated set of flags pertaining
    /// to the command's function.
    commands: []const Command,
    /// A "standard" interface. Only allows positional arguments on top of any flags specified in
    /// its respective configuration environment. This is allowed in both the root [Config](argz.Config)
    /// and [Commands](argz.Command) nested at any level.
    positionals: []const Positional,

    pub const Tag = std.meta.Tag(Mode);

    pub inline fn ToType(comptime mode: Mode) type {
        switch (mode) {
            .positionals => |positionals| comptime {
                var struct_fields: [positionals.len]Type.StructField = undefined;
                for (positionals, 0..) |positional, i| {
                    const Resolved = types.StructField(positional.type, .positional);
                    struct_fields[i] = Type.StructField{
                        .name = positional.fieldName(),
                        .type = Resolved,
                        .alignment = 0,
                        .is_comptime = false,
                        .default_value_ptr = if (@typeInfo(Resolved) == .optional) &@as(Resolved, null) else if (Resolved.defaultValue(.positional)) |dv| &dv else &@as(Resolved, undefined) 
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
    /// A detailed string documenting the command's purpose. For use within instances of a `--help=cmd:<command>` flag;
    info: ?[]const u8,
    aliases: []const [:0]const u8 = &.{},
    callback: CommandCallback,

    pub const CommandCallback = fn(
        comptime config: Config,
        comptime cmd: Command,
        comptime command_stack: []const Command,
        comptime mode: Mode,
        comptime flags: []const Flag,
        parser: *const Parser,
        _: []const u8,
    ) anyerror!void;

    pub const Extra = struct {
        field_name: ?[:0]const u8 = null,
        is_help: bool = false,
        info: ?[]const u8 = null,
        callback: CommandCallback = noopCommandCallback,
    };

    pub fn noopCommandCallback(
        comptime _: Config,
        comptime _: Command,
        comptime _: []const Command,
        comptime _: Mode,
        comptime _: []const Flag,
        _: *const Parser,
        _: []const u8,
    ) anyerror!void {}

    pub const help: Command = .init("help", &.{}, .{ .positionals = &.{} }, "show this help", .{
        .info =
        \\The 'help' command displays help regarding program usage
        \\and exits the program gracefully.
        ,
        .callback = fmt.commandHelpDefaultCallback,
    });

    pub inline fn init(cmd: [:0]const u8, flags: []const Flag, mode: Mode, help_msg: ?[]const u8, extra: Extra) Command {
        if (extra.is_help) {
            if (flags.len != 0)
                @compileError("help command must not have any flags");
            switch (mode) {
                .positionals => |positionals| if (positionals.len != 0)
                    @compileError("help command must not have any positional arguments"),
                .commands => @compileError("help command must have a mode of '.positionals'"),
            }
        }
        return comptime .{
            .cmd = cmd,
            .flags = flags,
            .mode = mode,
            .help_msg = help_msg,
            .field_name = extra.field_name,
            .info = extra.info,
            .callback = extra.callback,
        };
    }

    pub fn fieldName(cmd: Command) [:0]const u8 {
        return cmd.field_name orelse cmd.cmd;
    }

    pub inline fn ToType(comptime cmd: Command) type {
        return types.WrapModeAndFlags(cmd.mode, cmd.flags);   
    }
};

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
    aliases: []const Identifier,
    dependencies: []const Dependency,

    pub inline fn init(comptime T: type, short: ?u21, long: ?[:0]const u8, default_value: ?types.StructField(T, .flag), help_msg: ?[]const u8, extra: Extra) Flag {
        return comptime .{
            .short = short,
            .long = long,
            .type = T,
            .default_value_ptr = if (default_value) |dv| @ptrCast(&@as(types.StructField(T, .flag), dv)) else null,
            .help_msg = help_msg,
            .info = extra.info,
            .field_name = extra.field_name,
            .alt_type_name = extra.alt_type_name,
            .aliases = extra.aliases,
            .dependencies = extra.dependencies,
        };
    }

    pub inline fn fieldName(comptime flag: Flag) [:0]const u8 {
        return flag.field_name orelse (flag.long orelse std.fmt.comptimePrint("{u}", .{flag.short.?}));
    }

    pub inline fn defaultValue(comptime flag: Flag) ?types.StructField(flag.type, .flag) {
        if (flag.default_value_ptr) |dvp|
            return @as(*const types.StructField(flag.type, .flag), @ptrCast(@alignCast(dvp))).*;
        if (flag.type == void)
            return false;
        if (types.custom.isCustomType(flag.type, .flag))
            return flag.type.defaultValue(.flag);
        return null;
    }

    pub inline fn flagString(comptime flag: Flag, comptime variant: enum { auto, long, short }) [:0]const u8 {
        return switch (variant) {
            .auto => if (flag.long) |long| "--" ++ long else std.fmt.comptimePrint("-{u}", .{flag.short.?}),
            .long => "--" ++ (flag.long.?),
            .short => std.fmt.comptimePrint("-{u}", .{flag.short.?}),
        };
    }

    pub inline fn StructField(comptime flag: Flag) type {
        return types.StructField(flag.type, .flag);
    }

    pub const help: Flag = .init(types.FlagHelp, 'h', "help", null, "display this help", .{ .info = 
        \\ The '--help' flag displays help regarding program usage.
        \\ An occurance of the '--help' flag will exit the program.
    });

    pub const Extra = struct {
        info: ?[]const u8 = null,
        field_name: ?[:0]const u8 = null,
        alt_type_name: ?[:0]const u8 = null,
        aliases: []const Identifier = &.{},
        dependencies: []const Dependency = &.{},
    };

    pub const Identifier = union(enum) {
        long: [:0]const u8,
        short: u21,

        pub inline fn flagString(comptime ident: Identifier) []const u8 {
            return switch (ident) {
                .long => |text| "--" ++ text,
                .short => |char| "-" ++ std.unicode.utf8EncodeComptime(char),
            };
        }
    };

    pub const Dependency = struct {
        flag: Identifier,
        pred: Predicate,
        pub const Predicate = union(enum) {
            /// Indicates that the flag containing this `Dependency` cannot be found alongside `flag`
            /// in a sequence of command line arguments.
            exclusive,
            /// Indicates that the flag containing this `Dependency` must be found alongside `flag`
            /// in a sequence of command line arguments.
            required_present,
            /// Indicates that an occurance of the flag containing this `Dependency` in a sequence of
            /// command line arguments implies `flag[=value]`. If `override` is `true`, this will override
            /// a previous occurance of `flag`.
            implies: struct {
                value: *const anyopaque,
                override: bool = false,
            },
            requires: union(enum) {
                @"and": []const Predicate,
                @"or": []const Predicate,
                eq: ?*const anyopaque,
            },
        };
    };
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

