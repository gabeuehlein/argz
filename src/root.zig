const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;
const util = @import("util.zig");

const tokenizer = @import("Tokenizer.zig");

pub const Args = tokenizer.Args;
pub const SystemArgs = if (builtin.link_libc or !(builtin.os.tag == .windows or builtin.os.tag == .wasi))
    tokenizer.SystemArgs
else switch (builtin.os.tag) {
    .windows => @compileError("non-libc Windows platforms don't support `SystemArgs`, as `std.os.argv` isn't supported; use `OwnedArgs` instead"),
    .wasi => @compileError("non-libc WASI platforms don't support `SystemArgs`, as `std.os.argv` isn't supported; use `OwnedArgs` instead"),
    else => unreachable,
};
pub const OwnedArgs = tokenizer.OwnedArgs;
pub const Tokenizer = tokenizer.Tokenizer;

pub const Help = struct {
    short: bool = true,
    long: bool = true,
    command: bool = false,
    info: ?[]const u8 = null,
};

pub const Parser = @import("Parser.zig").argParser;
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

pub const Fixes = struct {
    const FixMode = enum(u2) {
        /// Only check character differences when comparing two flags or commands. This
        /// may miss more fine-grained errors, such as the difference between a typoed 'b'
        /// character that was supposed to be an 'a' character.
        simple,
        /// Computes the [Hamming Distance](https://en.wikipedia.org/wiki/Hamming_distance)
        /// beween two flags or commands and picks the valid flag or command with the closest
        /// bit representation within a specified threshold.
        hamming_dist,
        /// Uses a Hamming Distance comparison *as well* as checking for spurious or missing
        /// `-` or `--` character sequences in a flag or command.
        advanced,
    };
    enable: bool = true,
    mode: FixMode,
    diff_threshold: u32,
    /// Whether to inform the user of the correct type of a flag or
    /// positional if they entered an incorrect one. Also suggests
    /// using `-f--blah` or `--flag=--blah` in the case an extra flag was
    /// found when a string was expected.
    inform_correct_type: bool = true,
};

pub const Config = struct {
    pub const AnsiMode = enum(u2) {
        /// Don't use ANSI escape sequences, even if stdout/stderr support them.
        disable,
        /// Use ANSI escape sequences if stdout/stderr support them. Currently
        /// has no effect.
        ///
        /// **Note**: using this generates two near-identical variations of the
        /// help string and any error messages which will be used depending on
        /// whether ANSI escape sequence support was detected. Normally, this
        /// shouldn't be a problem, but if it is, you should either use `.disable`
        /// or `.force` instead, preferrably `.disable`.
        detect,
        force,
    };
    help: Help = .{},
    /// Whether to use ANSI escape sequences if it is detected that stdout/stderr
    /// supports them.
    ///
    /// TODO implement ANSI coloring
    ansi_mode: AnsiMode = .detect,
    /// Fix suggestion config for flags.
    ///
    /// TODO implement fix suggestions
    flag_fixes: Fixes = .{
        .enable = true,
        .mode = .advanced,
        .diff_threshold = 8,
    },
    /// The name of the program that will be shown in descriptive help strings.
    program_name: ?[]const u8 = null,
    /// A brief description of how the program should be used.
    program_description: ?[]const u8 = null,
    /// Fix suggestion config for commands.
    command_fixes: Fixes = .{ .enable = true, .mode = .advanced, .diff_threshold = 12 },
    /// Whether to suggest adding a `--` argument in order to
    /// make a would-be flag or command be treated as a positional instead.
    suggest_add_terminator: bool = true,
    /// Whether to force UTF-8 encoded inputs for command-line arguments.
    force_utf8: bool = true,
    /// Top-level flags for the CLI. These will be ignored after any commands
    /// if they have a flag with the same identifier.
    top_level_flags: []const Flag = &.{},
    /// The top-level [Mode](argz.Mode) for the CLI. This determines whether a project
    /// is command-based or flag-based.
    mode: Mode,
    /// Whether to support dynamic memory allocation. If `true`, variable length slices become
    /// legal as option types and any `[]const u8` values will be cloned via the provided allocator.
    /// It is the programmer's responsibility to free them when no longer needed.
    support_allocation: bool,
};

pub const Command = struct {
    /// The textual representation of the command. May *not* contain
    /// an ASCII space character (` `, hex `0x20`) or ASCII equals character
    /// (`=`, hex `0x3d`)
    cmd: [:0]const u8,
    field_name: ?[:0]const u8 = null,
    /// A brief help message describing the command's usage. Use `help.info` to provide
    /// a more in-depth help message.
    help_msg: ?[]const u8 = null,
    /// Custom flags for the command. Be aware that these will take precedence over any prior
    /// flags with the same long/short identifiers when being parsed.
    flags: []const Flag = &.{},
    /// The parsing [Mode](argz.Mode) for this command. `.standard` should be used for bottom-level
    /// commands, i.e. those which should perform a particular action. `.commands` should be used to
    /// to group clusters of related actions.
    mode: Mode,
    /// Additional help information for the command.
    help: Help = .{
        .long = false,
        .short = false,
        .command = true,
    },
};

pub const Flag = struct {
    /// The short form of the flag. If equal to `null`, `long` must have a valid representation.
    short: ?u21 = null,
    /// The long form of the flag. If equal to `null`, `short` must have a valid representation.
    long: ?[]const u8 = null,
    /// A brief description of the flag's purpose and usage.
    help_msg: ?[]const u8 = null,
    /// The name of the field representing the flag in the resulting flag `struct`.
    field_name: [:0]const u8,
    /// The type of the flag. Must *not* be equal to `void`.
    type: type = void,
    /// A default value for the flag. A value of `null` indicates that a value *must* be supplied
    /// by the user. Otherwise, the data pointed to must have a type equal to the `type` provided.
    ///
    /// The exception to this rule is when `type == bool`. In this case, then the user doesn't have
    /// to specify a particular value; it will be set to the *opposite* of whatever the default value is.
    /// If no default value was provided, an occurrence will set the value to `true`.
    default_value: ?*const anyopaque = null,
    alt_type_name: ?[]const u8 = null,
};

/// Returns a dummy type representing a flag whose occurances should be counted.
/// `T` *must* be an integer type. It c;an be either signed or unsigned, but must not be zero-sized
/// (i.e. `i0` or `u0` values are not allowed). Integer overflow is handled via saturating arithmetic.
pub fn Counter(comptime T: type) type {
    if (@typeInfo(T) != .Int)
        @compileError("`Counter` only supports integers; `" ++ @typeName(T) ++ "` is not supported");

    if (@bitSizeOf(T) == 0)
        @compileError("`Counter` doesn't support the `i0` or `u0` types");

    return struct { __argz_counter_tag: void = {}, __argz_counter_type: type = T };
}

/// Returns a dummy type representing a flag that can occur multiple times in the argument list.
/// The result will be an `ArrayListUnManaged(T)` containing the argument found with each occurrence. Optional
/// types are *not* supported. Only legal if dynamic memory allocation is enabled, *unless* a maximum value
/// is provided to ensure
pub fn Multi(comptime T: type, comptime size: ?usize) type {
    if (size == 0)
        @compileError("zero-length stack-based Multi is not supported; use `null` to force heap allocation");

    // TODO the type checking *needs* to be improved. For example, optionals are allowed here, when
    // they shouldn't. This goes for the entire codebase.
    util.checkValidFlagType(T, true);

    return struct { __argz_multi_tag: void = {}, __argz_multi_type: type = MultiValue(T, size), __argz_multi_stack_len: ?usize = size, __argz_multi_child_type: type = T };
}

pub fn MultiValue(comptime T: type, comptime size: ?usize) type {
    return if (size) |max| union(enum(u1)) {
        stack: std.BoundedArray(T, max),
        heap: std.ArrayListUnmanaged(T),

        pub fn appendNoAlloc(self: *@This(), value: T) error{ Overflow, OnHeap }!void {
            switch (self.*) {
                .stack => |*val| {
                    try val.append(value);
                },
                .heap => return error.OnHeap,
            }
        }

        pub fn append(self: *@This(), allocator: std.mem.Allocator, value: T) error{OutOfMemory}!void {
            switch (self.*) {
                .stack => |*val| {
                    val.append(value) catch {
                        var alu = try std.ArrayListUnmanaged(T).initCapacity(allocator, max + 1);
                        alu.appendSliceAssumeCapacity(val.constSlice());
                        alu.appendAssumeCapacity(value);
                        self.heap = alu;
                    };
                },
                .heap => |*val| {
                    try val.append(allocator, value);
                },
            }
        }

        pub fn deinitAlloc(self: *@This(), allocator: std.mem.Allocator) void {
            switch (self.*) {
                .stack => {},
                .heap => |*val| val.deinit(allocator),
            }
        }
    } else std.ArrayListUnmanaged(T);
}

pub const Positional = struct {
    /// The string that will be displayed in parentheses or braces in the CLI's help message.
    display: []const u8,
    /// The string that will identify the positional's field in the resulting struct.
    field_name: [:0]const u8,
    /// The positional's type. Can *not* be `void`. May be an optional value if and only if all successive positionals are optional.
    /// A `Multi` is permitted as this type.
    type: type,
    /// An alternative type name to display in the positional list. This can be used to provide more context for what a positional
    /// represents, e.g. `"path"` instead of simply `"string"`.
    alt_type_name: ?[]const u8 = null,
    /// A help string describing the positional argument's use.
    help_msg: ?[]const u8 = null,
};
