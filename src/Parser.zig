const std = @import("std");
const mem = std.mem;
const argz = @import("argz.zig");
const Lexer = @import("Lexer.zig");
const builtin = @import("builtin");
const values = @import("values.zig");

const assert = std.debug.assert;

const Option = argz.Option;
const Positional = argz.Positional;
const Allocator = std.mem.Allocator;
const Args = @import("Args.zig");
const types=  @import("types.zig");
const Writer = std.Io.Writer;

const Type = std.builtin.Type;
pub const Parser = @This();

interface: Interface,
color_mode: argz.ColorMode = .detect,
allocator: ?Allocator = null,
stdout_config: std.Io.tty.Config,
stderr_config: std.Io.tty.Config,
program_name: []const u8,
program_description: ?[]const u8,

pub const Interface = struct {
    vtable: *const VTable,
    context: *anyopaque,

    pub fn next(iface: *Interface) ?Token {
        return iface.vtable.next(iface.context);
    }

    pub fn argument(iface: *Interface, allow_leading_dash: bool) ?[]const u8 {
        return iface.vtable.argument(iface.context, allow_leading_dash);
    }

    pub fn formatHelp(
        iface: *Interface,
        parser: *Parser,
        options: []const Option.Runtime,
        positionals: []const Positional.Runtime,
        writer: *Writer,
    ) Writer.Error!void {
        return iface.vtable.format_help(iface.context, parser, options, positionals, writer);
    }

    pub fn handleError(iface: *Interface, parser: *const Parser, err: *const Error, writer: *Writer) error{HandlingFailed}!void {
        return iface.vtable.handle_error(iface, parser, err, writer);
    }

    pub const VTable = struct {
        next: *const fn(context: *anyopaque) ?Token,

        argument: *const fn(context: *anyopaque, allow_leading_dash: bool) ?[]const u8,

        handle_error: *const fn(
            context: *anyopaque,
            parser: *const Parser,
            err: *const Error,
            writer: *Writer,
        ) error{HandlingFailed}!void,

        format_help: *const fn(
            context: *anyopaque,
            parser: *Parser,
            options: []const Option.Runtime,
            positionals: []const Positional.Runtime,
            writer: *Writer,
        ) Writer.Error!void,
    };

    pub const Advanced = @import("Parser/advanced.zig").Advanced;

    pub const Simple = @import("Parser/Simple.zig");
};

pub const Token = union(Tag) {
    long_option: LongOpt,
    short_option: ShortOpt,
    word: []const u8,
    err: TokenizeError,

    pub const Option = union(enum) {
        long: LongOpt,
        short: ShortOpt,

        pub fn attachedArg(opt: Token.Option) ?[]const u8 {
            return switch (opt) {
                inline else => |v| v.attached_arg,
            };
        }

        pub fn getArg(opt: Token.Option, p: *Parser, accept_leading_dash: bool) ?[]const u8 {
            return switch (opt) {
                inline else => |v| v.getArg(p, accept_leading_dash)
            };
        }
    };

    pub const LongOpt = struct {
        repr: []const u8,
        attached_arg: ?[]const u8,

        pub fn getArg(opt: LongOpt, p: *Parser, accept_leading_dash: bool) ?[]const u8 {
            return opt.attached_arg orelse p.interface.argument(accept_leading_dash);
        }
    };

    pub const ShortOpt = struct {
        repr: u21,
        attached_arg: ?[]const u8,

        pub fn getArg(opt: ShortOpt, p: *Parser, accept_leading_dash: bool) ?[]const u8 {
            return opt.attached_arg orelse p.interface.argument(accept_leading_dash);
        }
    };

    pub const ErrorTag = enum {
        unexpected_force_stop,
        empty_argument,
    };

    pub const TokenizeError = union(ErrorTag) {
        unexpected_force_stop,
        empty_argument: usize,

        const Tag = enum {
            unexpected_force_stop,
            empty_argument,
        };
    };

    pub const Tag = enum {
        long_option,
        short_option,
        word,
        err,
    };
};

pub const Options = struct {
    /// Whether to emit ANSI escape sequences to enable support for colored output.
    color_mode: argz.ColorMode = .detect,
    /// The name of the program that will be shown in descriptive help strings.
    program_name: []const u8,
    /// A brief description of how the program should be used.
    program_description: ?[]const u8 = null,
    allocator: ?Allocator = null,
};

pub const Context = union(Tag) {
    option: *const Option.Runtime,
    positional: *const Positional.Runtime,

    const Tag = enum {
        option,
        positional,
    };
};

/// A type to represent general tokenization or semantic errors that the type-erased
/// `Parser` may encounter. 
pub const Error = union(enum) {
    unexpected_arg_for_option: struct {
        option: *const Option.Runtime,
        arg_string: []const u8,
    },
    expected_arg_for_option: *const Option.Runtime,
    invalid_arg_for_option: struct {
        option: *const Option.Runtime,
        arg_repr: []const u8,
    },
    unknown_short_option: struct {
        candidates: []const Option.Runtime,
        found: u21,
    },
    unknown_long_option: struct {
        /// This slice is filled with candidate options that may or may not
        /// be associaed with
        candidates: []const Option.Runtime,
        found: []const u8,
    },
    invalid_positional: struct {
        positional: *const Positional.Runtime,
        arg_repr: []const u8,
    },
    /// Payload is the first extra positional.
    too_many_positionals: []const u8,
    /// Payload is the display names of the required positionals.
    missing_positionals: []const Positional.Runtime,
    missing_required_option: *const Option.Runtime,
    duplicate_short_option: u21,
    duplicate_long_option: []const u8,
    /// Payload is the message.
    custom: []const u8,
    /// Identical to `custom`, except that [format] will *not* prepend an `"error: ..."`
    /// to the message and will not add coloration to the output.
    raw: []const u8,

    /// Returns a statically-allocated buffer to use for specialized
    /// formatting of errors beyond what can be done using the variants
    /// provided in the type-erased [Parser.Error].
    pub fn getStackBuffer() ?[]u8 {
        if (stack_buffer_granted)
            return null;

        stack_buffer_granted = true;
        return &stack_buffer;
    }

    /// Asserts that the stack buffer has already been lended out via a call
    /// to [getStackBuffer]. The `threadlocal` stack buffer that was previously
    /// lended out is undefined after a call to this function.
    pub fn releaseStackBuffer() void {
        assert(stack_buffer_granted);
        stack_buffer_granted = false;
        stack_buffer = undefined;
    }

    pub fn initFmtBuf(buf: []u8, tty_config: std.Io.tty.Config, comptime format_string: []const u8, args: anytype) Writer.Error!Error {
        var w: Writer = .fixed(buf);
        return initFmt(&w, tty_config, format_string, args);
    }

    pub fn initFmt(writer: *Writer, comptime format_string: []const u8, args: anytype) Writer.Error!Error {
        try writer.print(format_string, args);
        try writer.writeByte('\n');

        return .{ .custom = writer.buffered() };
    }

    // This will silently handle a small buffer by stating that the message was truncated.
    pub fn initFmtStatic(comptime format_string: []const u8, args: anytype) error{AlreadyInUse}!Error {
        const buf = getStackBuffer() orelse return error.AlreadyInUse;
        var w: Writer = .fixed(buf);
        return initFmt(&w, format_string, args) catch blk: {
            const truncated_msg = "<truncated>";
            const space = w.buffered();
            @memcpy(
                space[space.len -| truncated_msg.len..],
                truncated_msg[@min(space.len, truncated_msg.len)],
            );
            break :blk .{ .custom = w.buffered() };
        };
    }

    /// Global, per-thread stack-allocated buffer reserved for use in formatting errors.
    threadlocal var stack_buffer: [4096]u8 = undefined;
    /// Whether `stack_buffer` has already been lended out. Set to `true` and `false`
    /// in [getStackBuffer] and [releaseStackBuffer] respectively.
    threadlocal var stack_buffer_granted: bool = false;
};

pub fn init(interface: Interface, options: Options) Parser {
    const stdout_color, const stderr_color = switch (options.color_mode) {
        .disable => .{ .no_color, .no_color },
        .detect => blk: {
            const stdout = std.fs.File.stdout();
            const stderr = std.fs.File.stderr();
            break :blk .{ std.Io.tty.detectConfig(stdout), std.Io.tty.detectConfig(stderr) };
        },
        .force => .{ .escape_codes, .escape_codes },
    };

    return .{
        .stdout_config = stdout_color,
        .stderr_config = stderr_color,
        .program_name = options.program_name,
        .program_description = options.program_description,
        .allocator = options.allocator,
        .interface = interface,
    };
}

pub fn parse(p: *Parser, comptime T: type, comptime Config: type) (error{ParseError} || Allocator.Error)!T {
    return p.parseAdvanced(
        T,
        Config,
        gatherOptionCandidates(T, if (@hasDecl(Config, "options")) Config.options else struct {}),
        gatherPositionals(T, if (@hasDecl(Config, "positionals")) Config.positionals else struct {}),
    );
}

pub fn parseAdvanced(
    p: *Parser,
    comptime T: type,
    comptime Config: type,
    comptime options: []const Option,
    comptime positionals: []const Positional
) error{ParseError}!T {
    // TODO tune this
    @setEvalBranchQuota(200 * options.len + 200 * positionals.len);

    var result: T = undefined;

    var found_options = if (@hasField(T, "options"))
        std.StaticBitSet(options.len).initEmpty()
    else 
        {};

    const OptsConfig = getConfigOption(Config, "options") orelse struct {};
    const DefaultValueCallbacks = getConfigOption(OptsConfig, "default_value_callbacks") orelse struct {};
    const PositionalConfig = getConfigOption(Config, "positionals") orelse struct {};
    _ = PositionalConfig;

    comptime {
        if (@hasField(T, "options") and options.len == 0)
            @compileError("unnecessary 'options' field with zero fields of its own");

        if (@hasField(T, "positionals") and positionals.len == 0)
            @compileError("unnecessary 'positionals' field with zero fields of its own");
    }

    var positional_index: usize = 0;

    top: while (p.interface.vtable.next(p.interface.context)) |tok| {
        switch (tok) {
            .long_option => |opt| {
                if (@hasField(T, "options")) {
                    inline for (0.., options) |i, comptime_opt| {
                       if (comptime_opt.long) |long| {
                           if (std.mem.eql(u8, long, opt.repr)) {
                                if (found_options.isSet(i))
                                    return p.fail(.{ .duplicate_long_option = long });
                                if (getConfigOption(OptsConfig, "help_option")) |ho| {
                                    if (comptime std.mem.eql(u8, comptime_opt.field_name, ho)) {
                                        var buf: [4096]u8 = undefined;
                                        var stdout: std.fs.File = .stdout();
                                        var w = stdout.writer(&buf);
                                        defer w.interface.flush() catch {};

                                        p.interface.formatHelp(p, toRuntimeSlice(Option, options), toRuntimeSlice(Positional, positionals), &w.interface) catch {};
                                        std.process.exit(0);
                                    }
                                }
                               try p.handleOption(comptime_opt, .{ .long = opt }, &@field(result.options, comptime_opt.field_name));
                               found_options.set(i);
                               continue :top;
                           }
                       }
                    }
                }

                return p.fail(.{ .unknown_long_option = .{
                    .found = opt.repr,
                    .candidates = &.{},
                } });

            },
            .short_option => |opt| {
                if (@hasField(T, "options") and getConfigOption(OptsConfig, "short_mappings") != null) {
                    inline for (0.., options) |i, comptime_opt| {
                        if (comptime_opt.short) |short| {
                            if (short == opt.repr) {
                                if (found_options.isSet(i))
                                    return p.fail(.{ .duplicate_short_option = short });
                                if (getConfigOption(OptsConfig, "help_option")) |ho| {
                                    if (comptime std.mem.eql(u8, comptime_opt.field_name, ho)) {
                                        var buf: [4096]u8 = undefined;
                                        var stdout: std.fs.File = .stdout();
                                        var w = stdout.writer(&buf);
                                        defer w.interface.flush() catch {};

                                        p.interface.formatHelp(p, toRuntimeSlice(Option, options), toRuntimeSlice(Positional, positionals), &w.interface) catch {};
                                        std.process.exit(0);
                                    }
                                }
                                try p.handleOption(comptime_opt, .{ .short = opt }, &@field(result.options, comptime_opt.field_name));
                                found_options.set(i);
                                continue :top;
                            }
                        }
                    }
                }

                return p.fail(.{ .unknown_short_option = .{
                    .found = opt.repr,
                    .candidates = &.{},
                } });
            },
            .word => |word| {
                if (positionals.len == 0)
                    return p.fail(.{ .too_many_positionals = word });

                switch (positional_index) {
                    inline 0...positionals.len - 1 => |i| {
                        const comptime_pos = positionals[i];
                        try p.handlePositional(comptime_pos, word, &@field(result.positionals, comptime_pos.field_name));
                        positional_index += 1;
                    },
                    else => return p.fail(.{ .too_many_positionals = word }),
                }
            },
            .err => @panic("TODO: tokenization errors"),
        }
    }
    if (@hasField(T, "options")) {
        var unset = found_options.iterator(.{ .kind = .unset });
        
        while (unset.next()) |unset_bit| {
            switch (unset_bit) {
                inline 0...options.len - 1 => |i| {
                    if (options[i].defaultValue()) |dv|
                        @field(result.options, options[i].field_name) = dv
                    else if (getConfigOption(DefaultValueCallbacks, options[i].field_name)) |cb|
                        @field(result.options, options[i].field_name) = try cb(p)
                    else
                        return p.fail(.{ .missing_required_option = comptime &options[i].toRuntime() });
                },
                else => unreachable,
            }
        }
    }

    if (positional_index != positionals.len) {
        const runtime_positionals: []const Positional.Runtime = comptime blk: {
            var runtime_space: [positionals.len]Positional.Runtime = undefined;

            for (0.., positionals) |i, positional|
                runtime_space[i] = positional.toRuntime();

            const as_const = runtime_space;
            break :blk &as_const;
        };

        return p.fail(.{ .missing_positionals = runtime_positionals[positional_index..] });
    }

    return result;
}

pub fn handlePositional(
    p: *Parser,
    comptime pos: Positional,
    word: []const u8,
    dest_ptr: anytype 
) error{ParseError}!void {
    if (isVariadicArgument(pos))
        @compileError("TODO");

    dest_ptr.* = try values.parseValue(p, word, .{ .positional = pos });
}

pub fn handleOption(
    p: *Parser,
    comptime opt: Option,
    token: Token.Option,
    dest_ptr: anytype
) error{ParseError}!void {
    switch (@typeInfo(opt.type)) {
        .optional => {
            if (token.attachedArg()) |arg| {
                dest_ptr.* = try values.parseValue(p, arg, .{ .option = opt });
            } else {
                dest_ptr.* = null;
            }
        },
        .bool => {
            if (token.attachedArg()) |arg| {
                if (mem.eql(u8, "true", arg))
                    dest_ptr.* = true
                else if (std.mem.eql(u8, "false", arg))
                    dest_ptr.* = false
                else return p.fail(.{ .invalid_arg_for_option = .{
                    .arg_repr = arg,
                    .option = comptime &opt.toRuntime()
                } });
            } else {
                dest_ptr.* = true;
            }
        },
        else => |info| {
            const arg = token.getArg(p, types.mayHaveLeadingDash(@Type(info))) orelse return p.fail(.{ .expected_arg_for_option = comptime &opt.toRuntime() });
            dest_ptr.* = try values.parseValue(p, arg, .{ .option = opt });
        },
    }
}

fn printHelp(p: *Parser, exit_code: u8, file: std.fs.File, comptime options: []const Option.Runtime, positionals: []const Positional.Runtime, option_descriptions: std.StaticStringMap([]const u8)) noreturn {
    defer std.posix.exit(exit_code);
    var buf: [4096]u8 = undefined;
    var file_writer = file.writer(&buf);
    defer file_writer.interface.flush() catch {};
    p.interface.formatHelp(options, positionals, option_descriptions, &file_writer.interface);
}

pub fn fail(p: *Parser, err: Error) error{ParseError} {
    var buf: [128]u8 = undefined;

    var stderr = std.fs.File.stderr();
    stderr.lock(.exclusive) catch return error.ParseError;
    defer stderr.unlock();

    var w = stderr.writer(&buf);
    defer w.interface.flush() catch {};

    p.interface.handleError(p, &err, &w.interface) catch {};

    return error.ParseError;
}

pub fn failFmt(p: *Parser, comptime format_string: []const u8, args: anytype) error{ParseError} {
    const err: Error = .initFmtStatic(p.stderr_config, format_string, args);
    defer Error.releaseStackBuffer();
    return p.fail(err);
}

pub fn fatal(p: *const Parser, comptime fmt: []const u8, args: anytype) noreturn {
    const err: Error = .initFmtStatic(p.stderr_config, fmt, args);
    var buf: [128]u8 = undefined;

    var stderr = std.fs.File.stderr();
    stderr.lock(.exclusive) catch {};
    defer stderr.unlock();

    var w = stderr.writer(&buf);
    defer w.interface.flush() catch {};

    p.interface.handleError(p, &err, &w.interface) catch {};
    std.process.exit(1);
}

inline fn isVariadicArgument(comptime positional: Positional) bool {
    return comptime state: switch (@typeInfo(positional.type)) {
        .optional => |opt| continue :state opt.child,
        .pointer => |info| info.size == .slice and !(info.child == u8 and info.is_const),
        else => false,
    };
}

inline fn gatherOptionCandidates(comptime T: type, comptime Config: type) []const Option {
    if (!@hasField(T, "options"))
        return &.{};

    const struct_fields = @typeInfo(@FieldType(T, "options")).@"struct".fields;
    const substitute_underscore_with_minus = getConfigOption(Config, "substitute_underscore_with_minus") orelse true;

    comptime var result: [struct_fields.len]Option = undefined;
    inline for (0.., struct_fields) |i, field| {
        comptime var opt: Option = .{
            .field_name = field.name,
            .default_value_ptr = field.default_value_ptr,
            .type = field.type,
            .long = null,
            .short = null,
            .info = null,
        };

        if (substitute_underscore_with_minus) {
            comptime var new: [field.name.len:0]u8 = @splat(0);
            inline for (0.., field.name) |j, byte| {
                if (byte == '_')
                    new[j] = '-'
                else
                    new[j] = byte;
            }

            const as_const = new;
            opt.long = &as_const;
        } else {
            opt.long = field.name;
        }

        if (getConfigOption(Config, "alt_type_names")) |AltTypeNames| {
            if (@hasDecl(AltTypeNames, field.name))
                opt.alt_type_name = @field(AltTypeNames, field.name);
        }

        if (getConfigOption(Config, "short_mappings")) |ShortMappings| {
            if (@hasDecl(ShortMappings, field.name)) {
                const char: u21 = @field(ShortMappings, field.name);
                // Special case: if the field name is the same as the (string) representation
                // of the short mapping, then just the short flag is non-null in the resulting option.
                if (comptime std.mem.eql(u8, field.name, &std.unicode.utf8EncodeComptime(char)))
                    opt.long = null;
                opt.short = @field(ShortMappings, field.name);
            }
        }

        if (getConfigOption(Config, "info_messages")) |InfoMessages| {
            if (@hasDecl(InfoMessages, field.name))
                opt.info = @field(InfoMessages, field.name);
        }

        result[i] = opt;
    }

    const as_const = result;
    return &as_const;
}

pub inline fn gatherPositionals(comptime T: type, comptime Config: type) []const Positional {
    if (!@hasField(T, "positionals"))
        return &.{};

    const decls = @typeInfo(@FieldType(T, "positionals")).@"struct".fields;
    const substitute_underscore_with_minus = getConfigOption(Config, "substitute_underscore_with_minus") orelse true;
    comptime var result: [decls.len]Positional = undefined;

    inline for (0.., decls) |i, decl| {
        comptime var pos: Positional = .{
            .type = @FieldType(@FieldType(T, "positionals"), decl.name),
            .field_name = decl.name,
            .display = decl.name
        };

        if (substitute_underscore_with_minus) {
            comptime var new: [decl.name.len :0]u8 = @splat(0);
            inline for (0.., decl.name) |j, byte| {
                if (byte == '_')
                    new[j] = '-'
                else
                    new[j] = byte;
            }
            const as_const = new;
            pos.display = &as_const;
        }

        result[i] = pos;
    }

    const as_const = result;
    return &as_const;
}

pub inline fn getConfigOption(comptime Config: type, comptime option: []const u8) GetConfigOptionReturnType(Config, option) {
    if (@hasDecl(Config, option))
        return @field(Config, option)
    else
        return null;
}

inline fn GetConfigOptionReturnType(comptime Config: type, comptime option: []const u8) type {
    if (@hasDecl(Config, option))
        return ?@TypeOf(@field(Config, option))
    else
        return @Type(.null);
}

inline fn toRuntimeSlice(comptime T: type, comptime slice: []const T) []const T.Runtime {
    comptime var space: [slice.len]T.Runtime = undefined;

    comptime{
        for (slice, 0..) |elem, i|
            space[i] = elem.toRuntime();
    }

    const result = space;
    return &result;
}
