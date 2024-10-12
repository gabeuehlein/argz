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
    /// Custom flags for the command. Any prior flags will *not* additionally be parsed.
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

/// TODO verify First and Second to make sure they are valid.
pub fn Pair(comptime First: type, comptime Second: type, comptime separator: u21) type {
    return struct {
        const __argz_pair_tag = {};

        const Result = struct { First, Second };

        pub fn hasDynamicValue(comptime support_allocation: bool) bool {
            return util.typeHasDynamicValue(First, .pair, support_allocation) or util.typeHasDynamicValue(Second, .pair, support_allocation);
        }

        pub fn tryParse(string: []const u8) !Result {
            var buf = @as([4]u8, undefined);
            const bytes = std.fmt.bufPrint(&buf, "{u}", .{separator}) catch unreachable;
            var it = (try std.unicode.Utf8View.init(string)).iterator();
            const first, const second, const sep_found = while (it.nextCodepointSlice()) |data| {
                if (std.mem.eql(u8, bytes, data))
                    break .{ string[0 .. it.i - bytes.len], string[it.i..], true };
            } else if (@typeInfo(Second) == .Optional) .{ string, undefined, false } else return error.SeparatorNotFound;
            const first_val = try util.parseStaticValue(First, first);
            const second_val = switch (@typeInfo(Second)) {
                .Optional => |opt| if (!sep_found) null else try util.parseStaticValue(opt.child, second),
                else => try util.parseStaticValue(Second, second),
            };
            return .{ first_val, second_val };
        }

        pub fn tryParseAlloc(allocator: std.mem.Allocator, string: []const u8) !Result {
            var buf = @as([4]u8, undefined);
            const bytes = std.fmt.bufPrint(&buf, "{u}", .{separator}) catch unreachable;
            var it = (try std.unicode.Utf8View.init(string)).iterator();
            const first, const second, const sep_found = while (it.nextCodepointSlice()) |data| {
                if (std.mem.eql(u8, bytes, data))
                    break .{ string[0 .. it.i - bytes.len], string[it.i..], true };
            } else if (@typeInfo(Second) == .Optional) .{ string, undefined, false } else return error.SeparatorNotFound;
            const first_val = try util.parseStaticValue(First, first);
            const second_val = if (@typeInfo(Second) == .Optional and !sep_found)
                null
            else if (comptime util.typeHasDynamicValue(Second, .pair, true))
                try util.parseSlice(@typeInfo(Second).Pointer.child, allocator, second)
            else
                try util.parseStaticValue(Second, second);
            return .{ first_val, second_val };
        }
    };
}

test "Pair.tryParse" {
    inline for (.{ .{ Pair(u32, []const u8, '='), "92=bar", .{ 92, "bar" } }, .{ Pair(bool, [4]f32, '='), "true=1.2,3.4,5.6,7.8", .{ true, [4]f32{ 1.2, 3.4, 5.6, 7.8 } } }, .{ Pair([5]bool, bool, '='), "true,true,false,true,false=true", .{ [5]bool{ true, true, false, true, false }, true } }, .{ Pair([]const u8, []const u8, '='), "string=other-string", .{ "string", "other-string" } }, .{ Pair([2][]const u8, u32, '='), "a,string=1", .{ [2][]const u8{ "a", "string" }, 1 } }, .{ Pair(u0, u0, '='), "0=0", .{ 0, 0 } }, .{ Pair([]const u8, ?u16, '='), "foobar", .{ "foobar", null } } }) |data| {
        const val = try data[0].tryParse(data[1]);
        try std.testing.expectEqualDeep(val, data[2]);
    }

    const pair = Pair(enum { @"emit-bin", @"enable-asserts", @"link-mode", @"use-mold" }, []u32, '🄯');
    const string = "use-mold🄯1,2,3,4,5";
    const val = try pair.tryParseAlloc(std.testing.allocator, string);
    defer std.testing.allocator.free(val[1]);
    try std.testing.expectEqualDeep(val, .{ .@"use-mold", &[5]u32{ 1, 2, 3, 4, 5 } });
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
            .Pointer => |ptr| ptr.size == .Slice and flag.type != []const u8,
            .Array => |arr| support_allocation and arr.child == []const u8,
            else => false,
        };
    }

    /// Returns a type string representing the flag's type. Will *not* append suffixes
    /// to an alternate type name if found, and will cause a compile error if `flag.type` is
    /// equal to `void`.
    pub fn typeString(comptime flag: Flag, comptime ignore_alternate: bool) [:0]const u8 {
        return if (!ignore_alternate and flag.alt_type_name != null) flag.alt_type_name.? else blk: {
            const Inner = switch (@typeInfo(flag.type)) {
                .Optional => |opt| opt.child,
                else => flag.type,
            };
            break :blk switch (@typeInfo(Inner)) {
                .Void => @compileError("Flag.typeString requires a non-void type"),
                .Int => "INTEGER",
                .Float => "FLOAT",
                .Bool => "BOOLEAN",
                .Pointer => if (Inner == []const u8) "STRING" else switch (@typeInfo(Inner)) {
                    .Int => "INTEGER",
                    .Float => "FLOAT",
                    .Bool => "BOOLEAN",
                    else => @compileError("invalid type for flag: '" ++ @typeName(flag.type) ++ "'"),
                } ++ "...",
                .Array => |arr| switch (arr.child) {
                    .Int => "INTEGER",
                    .Float => "FLOAT",
                    .Bool => "BOOLEAN",
                    .Pointer => if (arr.child == []const u8) "STRING" else @compileError("invalid type for flag"),
                } ++ std.fmt.comptimePrint("[{d}]", .{arr.len}),
                else => @compileError("invalid type for flag: '" ++ @typeName(type.flag) ++ "'"),
            };
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
        comptime return pos.display ++ pos.suffix();
    }

    pub fn suffix(pos: Positional) [:0]const u8 {
        return switch (@typeInfo(pos.type)) {
            .Array => |arr| std.fmt.comptimePrint("[{d}]", .{arr.len}),
            .Pointer => if (pos.type == []const u8) "" else "...",
        };
    }
};

pub fn Counter(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .Int => |int| if (int.bits == 0)
            @compileError("counter's backing int may not be `u0` or `i0`")
        else
            .{ .__argz_counter_type = T },
        else => @compileError("counter's backing type must be an integer; `" ++ @typeName(T) ++ "` is not supported"),
    };
}

pub fn BoundedMulti(comptime T: type, comptime max_elems: usize) type {
    if (max_elems == 0)
        @compileError("cannot have a `BoundedMulti` with 0 max elements");
    return @TypeOf(.{ .__argz_bmulti_child = T, .__argz_bmulti_len = max_elems, .__argz_bmulti_backing_type = std.BoundedArray(T, max_elems) });
}

pub fn DynamicMulti(comptime T: type) type {
    return @TypeOf(.{ .__argz_dmulti_child = T, .__argz_dmulti_backing_type = std.ArrayListUnmanaged(T) });
}
