const std = @import("std");
const builtin = @import("builtin");
const args = @import("args.zig");

const Type = std.builtin.Type;

pub const Args = args.Args;
pub const OwnedArgs = args.OwnedArgs;
pub const SystemArgs = args.SystemArgs;
pub const Parser = @import("Parser.zig").Parser;
pub const Tokenizer = @import("Tokenizer.zig");
pub const fmt = @import("format.zig");
pub const types = @import("types.zig");

pub const Flag = struct {
    /// The short form of the flag. If equal to `null`, `long` must have a valid representation.
    short: ?u21,
    /// The long form of the flag. If equal to `null`, `short` must have a valid representation.
    long: ?[:0]const u8,
    /// The type of the flag. If equal to `void`, then the corresponding `struct` field will be a
    /// boolean indicating whether this flag was found in the argument list.
    type: type,
    /// A brief description of the flag's purpose and usage.
    help_msg: ?[]const u8,
    ident: [:0]const u8,
    /// An alternative type name to display in place of a flag's type. For example, one might specify
    /// this to be `"PATH"` if a string argument should represent a filesystem path.
    alt_type_name: ?[:0]const u8,
    repeatable: bool,
    required: bool,

    pub inline fn init(comptime T: type, id: [:0]const u8, options: Options) Flag {
        if (options.long == null and options.short == null)
            @compileError("flag must have either a default long representation or a default short representation");
        // TODO check for duplicate aliases in a way that isn't horrendously slow in comptime.
        // I've considered reifying error sets, but that makes for bad error messages.
        return comptime .{
            .short = options.short,
            .long = options.long,
            .type = T,
            .help_msg = options.help_msg,
            .ident = id,
            .alt_type_name = options.alt_type_name,
            .repeatable = options.repeatable,
            .required = options.required,
        };
    }

    pub inline fn flagString(comptime flag: Flag, comptime variant: enum { auto, long, short }) [:0]const u8 {
        return switch (variant) {
            .auto => if (flag.long) |long| "--" ++ long else std.fmt.comptimePrint("-{u}", .{flag.short.?}),
            .long => "--" ++ (flag.long.?),
            .short => std.fmt.comptimePrint("-{u}", .{flag.short.?}),
        };
    }

    pub inline fn Resolve(comptime flag: Flag) type {
        return types.ResolveType(flag.type, .flag);
    }

    pub const Options = struct {
        short: ?u21 = null,
        long: ?[:0]const u8 = null,
        info: ?[:0]const u8 = null,
        help_msg: ?[:0]const u8 = null,
        alt_type_name: ?[:0]const u8 = null,
        repeatable: bool = false,
        required: bool = true,
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
    ident: [:0]const u8,
    required: bool,
    repeatable: bool,

    pub const Options = struct {
        help_msg: ?[:0]const u8 = null,
        required: bool = true,
        repeatable: bool = false,
    };

    pub inline fn init(comptime T: type, id: [:0]const u8, display: [:0]const u8, options: Options) Positional {
        return .{
            .type = T,
            .display = display,
            .help_msg = options.help_msg,
            .ident = id,
            .required = options.required,
            .repeatable = options.repeatable,
        };
    }

    pub inline fn Resolve(comptime positional: Positional) type {
        return types.ResolveType(positional.type, .positional);
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

pub const Pair = @import("types/pair.zig").Pair;
pub const Sequence = @import("types/sequence.zig").Sequence;

comptime {
    std.testing.refAllDecls(@This());
}
