const std = @import("std");
const builtin = @import("builtin");
const util = @import("util.zig");
pub const Lexer = @import("Lexer.zig");
const args = @import("args.zig");

const testing = std.testing;

pub const Args = args.Args;
pub const OwnedArgs = args.OwnedArgs;
pub const SystemArgs = args.SystemArgs;

/// Dummy type indicating that a flag should serve to print a message showing usage information.
pub const FlagHelp = @TypeOf(.{ .__argz_flaghelp_tag = {} });

pub const argParser = @import("Parser.zig").argParser;
pub const HelpPrinter = @import("help.zig").HelpPrinter;

pub const Mode = union(enum(u1)) {
    /// Specifies the usage of a set of commands and subcommands (and sub-subcommands, and so on).
    /// Each command has its own set of commands *or* positional arguments and a dedicated set of flags pertaining
    /// to the command's function.
    commands: []const Command,
    /// A "standard" interface. Only allows positional arguments on top of any flags specified in
    /// its respective configuration environment. This is allowed in both the root [Config](argz.Config)
    /// and [Commands](argz.Command) nested at any level.
    standard: []const Positional,
};

pub const Config = struct {
    pub const AnsiMode = enum(u2) {
        /// Don't use ANSI escape sequences, even if stdout/stderr support them.
        disable,
        /// Use ANSI escape sequences if stdout/stderr support them.
        detect,
        /// Force the usage of ANSI escape sequences, regardless of whether stdout/stderr
        /// support them. Note that using this is discouraged; if you don't want `argz`
        /// to check for ANSI escape sequence support, prefer `disable` instead.
        force,
    };
    /// Whether to use ANSI escape sequences if it is detected that stdout/stderr
    /// supports them.
    ansi_mode: AnsiMode = .detect,
    /// The name of the program that will be shown in descriptive help strings.
    program_name: ?[]const u8 = null,
    /// A brief description of how the program should be used.
    program_description: ?[]const u8 = null,
    /// Whether to suggest adding a `--` argument in order to
    /// make a would-be flag or command be treated as a positional instead.
    suggest_add_terminator: bool = true,
    /// Top-level flags for the CLI. These will be ignored after any commands
    /// if they have a flag with the same identifier.
    top_level_flags: []const Flag = &.{},
    /// The top-level [Mode](argz.Mode) for the CLI. This determines whether a project
    /// is command-based or flag-based.
    mode: Mode,
    /// Whether to support dynamic memory allocation. If `true`, variable length slices become
    /// legal as option types. It is the programmer's responsibility to free them when no longer needed.
    ///
    /// In the case of a flag of positional being of type `[]const u8`, no allocation will be performed;
    /// the argument will be returned as-is from the arguments provided by the user. If the type is an array or slice
    /// *and* the type is for a flag, then strings will be cloned by the allocator, with commas being able to
    /// be escaped using a backslash (e.g. the string "1\,2,3" will produce the array `.{ "1,2", "3" }`). These strings
    /// *will* have to be freed by the programmer.
    ///
    /// Arrays or slices of positionals are not handled as above -- as positionals are unique to different arguments,
    /// there is no possible ambiguities between separate arguments, and as such the argument provided by the user
    /// will be used as-is in the respective slice. Note that *the slice containing the strings* is still dynamically
    /// allocated, and must be freed by the user, only the strings contained within are not.
    support_allocation: bool,
};

pub const Command = struct {
    /// The textual representation of the command.
    cmd: [:0]const u8,
    field_name: ?[:0]const u8 = null,
    /// A brief help message describing the command's usage. Use `info` to provide
    /// a more in-depth help message.
    help_msg: ?[]const u8 = null,
    /// Custom flags for the command. Any prior flags will *not* be parsed.
    flags: []const Flag = &.{},
    /// The parsing [Mode](argz.Mode) for this command. `.standard` should be used for bottom-level
    /// commands, i.e. those which should perform a particular action. `.commands` should be used to
    /// to group clusters of related actions.
    mode: Mode,
    /// Whether to designate this flag as a help command. Note that multiple such help commands can
    /// exist in one set of commands, and it is not checked that there is at most one help command.
    is_help: bool = false,
    /// A detailed string documenting the command's usage.
    info: ?[]const u8 = null,

    pub fn fieldName(cmd: Command) [:0]const u8 {
        return cmd.field_name orelse cmd.cmd;
    }
};

pub fn Pair(comptime First: type, comptime Second: type, comptime separator: u21) type {
    return @TypeOf(.{
        .__argz_pair_tag = {},
        .__argz_pair_result = .{ First, Second },
        .__argz_pair_separator = separator,
    });
}

pub const Flag = struct {
    /// The short form of the flag. If equal to `null`, `long` must have a valid representation.
    short: ?u21 = null,
    /// The long form of the flag. If equal to `null`, `short` must have a valid representation.
    long: ?[:0]const u8 = null,
    /// A brief description of the flag's purpose and usage.
    help_msg: ?[]const u8 = null,
    /// The name of the field representing the flag in the resulting flag `struct`. If `null`, will
    /// be equivalent to either `long` or `short` represented as a UTF-8 encoded string.
    field_name: ?[:0]const u8 = null,
    /// The type of the flag. If equal to `void`, then the corresponding `struct` field will be a
    /// boolean indicating whether this flag was found in the argument list.
    type: type,
    /// A default value for the flag. A value of `null` indicates that this flag *must* be supplied
    /// by the user. Otherwise, the data pointed to must have a type equal to the `type` provided.
    default_value: ?*const anyopaque = null,
    /// An alternative type name to display in place of a flag's type. For example, one might specify
    /// this to be `"PATH"` if a string argument should represent a filesystem path. By convention,
    /// this should be in all caps, with spaces being used to separate words, meaning that a string
    /// like `"FRIED CHICKEN"` should be preferred over `"fried chicken"` or `"FRIED_CHICKEN"`.
    alt_type_name: ?[:0]const u8 = null,

    pub fn fieldName(flag: Flag) [:0]const u8 {
        return flag.field_name orelse (flag.long orelse std.fmt.comptimePrint("{u}", .{flag.short.?}));
    }

    pub fn hasDynamicValue(comptime flag: Flag, comptime support_allocation: bool) bool {
        return if (comptime util.isDynamicMulti(flag.type))
            true
        else switch (comptime @typeInfo(flag.type)) {
            .pointer => |ptr| ptr.size == .Slice and flag.type != []const u8,
            .array => |arr| support_allocation and arr.child == []const u8,
            else => false,
        };
    }

    /// Returns a type string representing the flag's type. Will *not* append suffixes
    /// to an alternate type name if found, and will cause a compile error if `flag.type` is
    /// equal to `void`.
    pub fn typeString(comptime flag: Flag, comptime ignore_alternate: bool) [:0]const u8 {
        return if (!ignore_alternate and flag.alt_type_name != null) flag.alt_type_name.? else {
            const check = struct {
                fn func(comptime T: type) [:0]const u8 {
                    if (comptime util.isCounter(T))
                        return "COUNTER"
                    else if (comptime util.isPair(T)) {
                        const Result = @as(T, .{}).__argz_pair_result;
                        const sep = @as(T, .{}).__argz_pair_separator;
                        return comptime std.fmt.comptimePrint("{s}{u}{s}", .{ func(Result[0]), sep, func(Result[1]) });
                    }
                    if (comptime util.isBoundedMulti(T)) {
                        const Child = @as(T, .{}).__argz_bmulti_child;
                        if (comptime util.isPair(Child))
                            return func(Child);
                    } else if (comptime util.isDynamicMulti(T)) {
                        const Child = @as(T, .{}).__argz_dmulti_child;
                        if (comptime util.isPair(Child))
                            return func(Child);
                    }
                    const Inner = switch (@typeInfo(T)) {
                        .optional => |opt| opt.child,
                        else => T,
                    };
                    return switch (@typeInfo(Inner)) {
                        .void => @compileError("Flag.typeString requires a non-void type"),
                        .int => "INTEGER",
                        .float => "FLOAT",
                        .bool => "BOOLEAN",
                        .pointer => if (Inner == []const u8) "STRING" else switch (@typeInfo(Inner)) {
                            .int => "INTEGER",
                            .float => "FLOAT",
                            .bool => "BOOLEAN",
                            else => @compileError("invalid type for flag: '" ++ @typeName(flag.type) ++ "'"),
                        } ++ "...",
                        .array => |arr| switch (@typeInfo(arr.child)) {
                            .int => "INTEGER",
                            .float => "FLOAT",
                            .bool => "BOOLEAN",
                            .pointer => if (arr.child == []const u8) "STRING" else @compileError("invalid type for flag"),
                            else => @compileError("invalid type for flag: '" ++ @typeName(flag.type) ++ "'"),
                        } ++ std.fmt.comptimePrint("[{d}]", .{arr.len}),
                        .@"enum" => |info| blk: {
                            if (info.fields.len == 0)
                                @compileError("empty enums are not supported");
                            comptime var string = info.fields[0].name;
                            inline for (info.fields[1..]) |field| {
                                string = string ++ "|" ++ field.name;
                            }
                            break :blk string;
                        },
                        else => @compileError("invalid type for flag: '" ++ @typeName(flag.type) ++ "'"),
                    };
                }
            }.func;
            return check(flag.type);
        };
    }

    pub fn flagString(comptime flag: Flag, variant: util.FlagType) [:0]const u8 {
        return switch (variant) {
            .long => "--" ++ (flag.long orelse unreachable),
            .short => std.fmt.comptimePrint("-{u}", .{flag.short orelse unreachable}),
        };
    }
};

pub const Positional = struct {
    /// The string that will be displayed in parentheses or braces in the CLI's help message. This should
    /// be brief yet descriptive, such as `"PATH"` or `"ITERATIONS"`.
    display: [:0]const u8,
    /// The string that will identify the positional's field in the resulting struct. If equal to `null`,
    /// [fieldName] will return `display` instead.
    field_name: ?[:0]const u8,
    /// The positional's type. Can *not* be `void`. May be an optional value if and only if all successive positionals are optional.
    type: type,
    /// A help string describing the positional argument's use.
    help_msg: ?[]const u8 = null,

    pub fn fieldName(comptime pos: Positional) [:0]const u8 {
        return pos.field_name orelse pos.display;
    }

    pub fn displayString(comptime pos: Positional) [:0]const u8 {
        return comptime pos.display ++ pos.suffix();
    }

    pub fn suffix(pos: Positional) [:0]const u8 {
        return switch (@typeInfo(pos.type)) {
            .array => |arr| std.fmt.comptimePrint("[{d}]", .{arr.len}),
            .pointer => if (pos.type == []const u8) "" else "...",
            else => "",
        };
    }
};

pub fn Counter(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .int => |int| if (int.bits == 0)
            @compileError("counter's backing int may not be `u0` or `i0`")
        else
            @TypeOf(.{ .__argz_counter_type = T }),
        else => @compileError("counter's backing type must be an integer; `" ++ @typeName(T) ++ "` is not supported"),
    };
}

pub fn BoundedMulti(comptime T: type, comptime max_elems: usize) type {
    if (max_elems == 0)
        @compileError("cannot have a `BoundedMulti` with 0 max elements");
    return @TypeOf(.{ .__argz_bmulti_child = T, .__argz_bmulti_len = max_elems, .__argz_bmulti_backing_type = std.BoundedArray(T, max_elems) });
}

pub fn DynamicMulti(comptime T: type) type {
    return @TypeOf(.{ .__argz_dmulti_child = T, .__argz_dmulti_backing_type = std.ArrayListUnmanaged(@import("Parser.zig").ResolveType(T)) });
}
