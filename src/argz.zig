const std = @import("std");
const assert = std.debug.assert;
const builtin = @import("builtin");
const options = @import("build_options");

const Type = std.builtin.Type;
const Writer = std.Io.Writer;
const Reader = std.Io.Reader;

pub const args = @import("args.zig");
pub const Parser = @import("Parser.zig").Parser;
pub const Lexer = @import("Lexer.zig");
pub const fmt = @import("format.zig");
pub const types = @import("types.zig");

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

pub const Option = struct {
    /// The short form of the option. If equal to `null`, `long` must have a valid representation.
    short: ?u21,
    /// The long form of the option. If equal to `null`, `short` must have a valid representation.
    long: ?[:0]const u8,
    /// The type of the option. If equal to `void`, then the corresponding `struct` field will be a
    /// boolean indicating whether this option was found in the argument list.
    type: type,
    /// A default value for the option.
    default_value_ptr: ?*const anyopaque,
    /// The name of the field representing the option in the resulting option `struct`. If `null`, the
    /// field name will be equal to the option's long form, or the short form if no long form was provided.
    field_name: [:0]const u8,

    pub inline fn defaultValue(comptime option: Option) ?option.type {
        if (option.default_value_ptr) |dvp|
            return @as(*const option.type, @ptrCast(@alignCast(dvp))).*;
        return null;
    }

    /// Converts `option` to an alternative that can easily be passed around at runtime.
    /// This happens by removing any reference to `comptime`-only values, namely `type`
    /// and `default_value`, converting them into stringly-typed representations. These
    /// could *technically* be re-parsed to obtain the original type and default value
    /// at runtime, but this is discouraged.
    pub fn toRuntime(comptime option: Option) Runtime {
        return .{
            .short = option.short,
            .long = option.long,
            .type_name = types.name(option.type),
            .default_value_repr = if (option.defaultValue()) |dv| blk: {
                switch (@typeInfo(option.type)) {
                    .@"struct", .@"union" => {
                        if (@hasDecl(option.type, "format"))
                            break :blk comptime std.fmt.comptimePrint("{f}", .{dv})
                        else
                            break :blk null;
                    },
                    else => break :blk null,
                }
            } else null,
            .field_name = option.field_name,
        };
    }

    pub const Runtime = struct {
        /// The short form of the option. If equal to `null`, `long` must have a valid representation.
        short: ?u21,
        /// The long form of the option. If equal to `null`, `short` must have a valid representation.
        long: ?[:0]const u8,
        /// The type of the option. If equal to `void`, then the corresponding `struct` field will be a
        /// boolean indicating whether this option was found in the argument list.
        type_name: [:0]const u8,
        default_value_repr: ?[:0]const u8,
        /// The name of the field representing the option in the resulting option `struct`. If `null`, the
        /// field name will be equal to the option's long form, or the short form if no long form was provided.
        field_name: [:0]const u8,

        pub fn format(rt: *const Runtime, writer: *Writer) Writer.Error!void {
            if (rt.long != null) {
                // Prefer to use the long representation over the short representation,
                // since long-form options tend to convey more information.
                try writer.print("--{s}", .{rt.long.?});
            } else {
                try writer.print("-{u}", .{rt.short.?});
            }
        }
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
    /// The string that will identify the positional's field in the resulting struct. If equal to `null`,
    /// [fieldName] will return `display` instead.
    field_name: [:0]const u8,

    pub inline fn toRuntime(comptime positional: Positional, audience: Audience) Runtime {
        return .{
            .info = positional.info,
            .field_name = positional.field_name,
            .help_msg = positional.help_msg,
            .display = positional.display,
            .type_name = types.name(positional.type, audience),
        };
    }

    pub const Runtime = struct {
        type_name: [:0]const u8,
        display: [:0]const u8,
        field_name: [:0]const u8,
    };
};

/// Suggests how `argz` and extensions should behave when needing to
/// produce formatted output. Information about each predefined audience
/// is provided in the docstrings for each variant.
///
/// Built-in `argz` parsers will always conform to these requirements.
/// Note, however, that extensions to `argz` (e.g. custom parsers) do
/// not strictly have to conform to the requirements.
pub const Audience = enum(u8) {
    /// Produces strings tailored to users. For instance, passing a `u32`
    /// argument to [typeName] may return `"integer"`. Types formatted in
    /// this way do not need to be in any particular format.
    user = 0,
    /// Produces strings tailored to developers of software using `argz`.
    /// This audience is similar to `user`, except for the following:
    ///  1. `types.name(T)` should use `@typeName` to produce a precise type name
    ///     to convey the source type provided to it.
    ///  2. `values.repr(val)` should use the '{any}' format specifier option for
    ///     `val` if `@TypeOf(val)` has a `format` method, and should return
    ///     `null` otherwise.
    developer = 1,
    /// Performs no work prettifying strings. This audience is similar to `developer`
    /// except for that no code should be executed to enhance a string for human consumption.
    /// Examples of "enhancing" a string follow:
    ///  1. `types.name(T, .computer)` should always return `@typeName(T)`.
    ///  2. `values.repr(val)` should always return `null`
    ///  3. Padding to a certain column width with spaces should never happen.
    ///  4. Terminal information should never be queried to produce more legible output.
    ///     This includes color and insertion of line breaks to aid in the viewing
    ///     of long texts in narrow terminals.
    computer = 2,


    pub const default: Audience = @enumFromInt(@intFromEnum(options.target_audience));
};
