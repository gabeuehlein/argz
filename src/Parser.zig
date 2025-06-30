const std = @import("std");
const argz = @import("argz.zig");
const Tokenizer = @import("Tokenizer.zig");
const builtin = @import("builtin");
const values = @import("Parser/values.zig");

const assert = std.debug.assert;

const Flag = argz.Flag;
const Positional = argz.Positional;
const Allocator = std.mem.Allocator;
const Args = @import("args.zig").Args;
const fmt = @import("format.zig");
const types = @import("types.zig");

const TtyConfig = std.io.tty.Config;

const Type = std.builtin.Type;

pub const Parser = @This();

pub const Options = struct {
    /// Whether to emit ANSI escape sequences to enable support for colored output.
    color_mode: ColorMode = .detect,
    /// The name of the program that will be shown in descriptive help strings.
    program_name: ?[]const u8 = null,
    /// A brief description of how the program should be used.
    program_description: ?[]const u8 = null,
    /// Gives a hint to error handlers that they should attempt to
    /// make suggestions to users to correct invalid inputs.
    make_suggestions: bool = true,
    allocator: ?Allocator = null,
};

pub const Context = union(enum) {
    pub const Tag = std.meta.Tag(@This());

    flag: struct {
        repr: union(enum) {
            long: []const u8,
            short: u21,

            pub inline fn toStringComptime(comptime repr: @This()) [:0]const u8 {
                return switch (repr) {
                    .long => |l| "--" ++ l,
                    .short => |s| "-" ++ std.unicode.utf8EncodeComptime(s),
                };
            }
        },
        ty_string: ?[:0]const u8,
    },
    positional: struct {
        display: []const u8,
        ty_string: ?[:0]const u8,
    },
};

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

stdout_config: std.io.tty.Config,
stderr_config: std.io.tty.Config,
program_name: ?[]const u8,
program_description: ?[]const u8,
make_suggestions: bool = true,
found_stop: bool,
allocator: ?Allocator,
tokenizer: Tokenizer,

pub fn init(args: Args, options: Options) !Parser {
    const stdout_color, const stderr_color = switch (options.color_mode) {
        .disable => .{ .no_color, .no_color },
        .detect => blk: {
            const stdout = std.io.getStdOut();
            const stderr = std.io.getStdErr();
            break :blk .{ std.io.tty.detectConfig(stdout), std.io.tty.detectConfig(stderr) };
        },
        .force => .{ .escape_codes, .escape_codes },
    };
    return .{
        .stdout_config = stdout_color,
        .stderr_config = stderr_color,
        .program_name = options.program_name,
        .program_description = options.program_description,
        .allocator = options.allocator,
        .tokenizer = try Tokenizer.init(args),
        .found_stop = false,
    };
}

pub fn nextArg(
    p: *Parser,
    comptime flags: []const Flag,
    comptime positionals: []const Positional,
    context: *ParseContext(flags, positionals),
) error{ ParseError, OutOfMemory }!?NextArgReturnType(flags, positionals) {
    if (p.found_stop) {
        const word = p.tokenizer.skip() orelse return null;
        if (comptime positionals.len == 0)
            return p.fail("found extra positional '{s}'", .{word});
        switch (context.positional_index) {
            inline 0...positionals.len - 1 => |i| {
                return p.handlePositional(NextArgReturnType(flags, positionals), positionals[i], word);
            },
            inline positionals.len => {
                return p.handlePositional(NextArgReturnType(flags, positionals), positionals[positionals.len - 1], word);
            },
            else => unreachable,
        }
    }
    const tok: Tokenizer.Token = p.tokenizer.next() orelse return null;
    // Needed for similarity checkecking.
    // TODO re-implement this
    // const flag_candidates = comptime gatherFlagCandidates(flags);
    state: switch (tok) {
        .long_flag => |long| {
            inline for (flags) |flag| {
                if (flag.long) |flag_long| {
                    if (std.mem.eql(u8, flag_long, long.repr)) {
                        return try p.handleFlag(NextArgReturnType(flags, positionals), flag, context, long.arg, .{ .flag = .{
                            .repr = .{ .long = flag_long },
                            .ty_string = flag.alt_type_name orelse types.typeName(flag.type, .flag, 0),
                        } });
                    }
                }
            }
            return p.fail("unknown flag '--{s}'", .{long.repr});
        },
        .short_flag => |short| {
            inline for (flags) |flag| {
                if (flag.short) |flag_short| {
                    if (flag_short == short) {
                        return try p.handleFlag(NextArgReturnType(flags, positionals), flag, context, null, .{ .flag = .{
                            .repr = .{ .short = flag_short },
                            .ty_string = flag.alt_type_name orelse types.typeName(flag.type, .flag, 0),
                        } });
                    }
                }
            }
            return p.fail("unknown flag '-{u}'", .{short});
        },
        .word => |word| {
            const index = context.positional_index;
            try context.foundPositional(p, word);
            if (comptime positionals.len == 0)
                unreachable; // context.foundPositional checks if no positionals exist

            switch (index) {
                inline positionals.len => {
                    return try p.handlePositional(NextArgReturnType(flags, positionals), positionals[positionals.len - 1], word);
                },
                inline 0...positionals.len - 1 => |i| {
                    return try p.handlePositional(NextArgReturnType(flags, positionals), positionals[i], word);
                },
                else => unreachable,
            }
        },
        .stop => {
            p.found_stop = true;
            const word = p.tokenizer.skip() orelse return null;
            continue :state .{ .word = word };
        },
    }
}

fn handlePositional(p: *Parser, comptime ReturnType: type, comptime positional: Positional, word: []const u8) error{ ParseError, OutOfMemory }!?ReturnType {
    const ToParse = positional.Resolve();
    var space: ToParse = undefined;
    try values.parseValueAuto(positional.type, &space, p, word, .{ .positional = .{
        .display = positional.display,
        .ty_string = types.typeName(positional.type, .positional, 0),
    } }, 0);
    return .{ .positional = @unionInit(ReturnType._PositionalUnion, positional.ident, space) };
}

fn handleFlag(p: *Parser, comptime ReturnType: type, comptime flag: Flag, parse_context: anytype, long_arg: ?[]const u8, comptime context: Context) error{ ParseError, OutOfMemory }!ReturnType {
    try parse_context.foundFlag(p, flag.ident, context.flag.repr.toStringComptime());
    if (flag.type == void) {
        if (long_arg) |arg|
            return p.fail("found unexpected argument '{s}' for flag '{s}'", .{ arg, context.flag.repr.toStringComptime() });
        return .{ .flag = @unionInit(ReturnType._FlagUnion, flag.ident, {}) };
    }
    var space: flag.Resolve() = undefined;
    if (@typeInfo(flag.type) == .optional) opt: {
        const arg: []const u8 = long_arg orelse (p.tokenizer.optionalArgument() orelse {
            space = null;
            break :opt;
        });
        try values.parseValueAuto(flag.type, &space, p, arg, context, 0);
    } else {
        const arg: []const u8 = long_arg orelse (p.tokenizer.argument() catch |e| switch (e) {
            error.ExpectedArgument, error.LeadingDashInArgument => return p.fail("expected argument for flag '{s}'", .{context.flag.repr.toStringComptime()}),
        });
        try values.parseValueAuto(flag.type, &space, p, arg, context, 0);
    }
    return .{ .flag = @unionInit(ReturnType._FlagUnion, flag.ident, space) };
}

inline fn gatherFlagCandidates(comptime flags: []const Flag) []const []const u8 {
    comptime var candidates: [][]const u8 = &.{};
    inline for (flags) |flag| {
        if (flag.long) |long|
            candidates = candidates ++ .{"--" ++ long};
        if (flag.short) |short|
            candidates = candidates ++ .{comptime std.fmt.comptimePrint("-{u}", .{short})};
    }
    // TODO check for duplicates + support aliases
    const as_const = candidates;
    return as_const;
}

pub inline fn NextArgReturnType(comptime flags: []const Flag, comptime positionals: []const Positional) type {
    const FlagEnum, const FlagUnion = comptime blk: {
        var union_fields: [flags.len]Type.UnionField = undefined;
        var enum_fields: [flags.len]Type.EnumField = undefined;
        for (flags, 0..) |flag, i| {
            enum_fields[i] = Type.EnumField{
                .name = flag.ident,
                .value = i,
            };
            union_fields[i] = Type.UnionField{
                .type = flag.Resolve(),
                .alignment = 0,
                .name = flag.ident,
            };
        }
        const enum_fields_const = enum_fields;
        const union_fields_const = union_fields;
        const Enum = @Type(.{ .@"enum" = Type.Enum{
            .decls = &.{},
            .tag_type = std.math.IntFittingRange(0, flags.len),
            .fields = &enum_fields_const,
            .is_exhaustive = true,
        } });
        const Union = @Type(.{ .@"union" = Type.Union{
            .decls = &.{},
            .tag_type = Enum,
            .fields = &union_fields_const,
            .layout = .auto,
        } });
        break :blk .{ Enum, Union };
    };

    const PositionalEnum, const PositionalUnion = comptime blk: {
        var union_fields: [positionals.len]Type.UnionField = undefined;
        var enum_fields: [positionals.len]Type.EnumField = undefined;
        for (positionals, 0..) |positional, i| {
            enum_fields[i] = Type.EnumField{
                .name = positional.ident,
                .value = i,
            };
            union_fields[i] = Type.UnionField{
                .type = positional.Resolve(),
                .alignment = 0,
                .name = positional.ident,
            };
        }
        const enum_fields_const = enum_fields;
        const union_fields_const = union_fields;
        const Enum = @Type(.{ .@"enum" = Type.Enum{
            .decls = &.{},
            .tag_type = std.math.IntFittingRange(0, positionals.len),
            .fields = &enum_fields_const,
            .is_exhaustive = true,
        } });
        const Union = @Type(.{ .@"union" = Type.Union{
            .decls = &.{},
            .tag_type = Enum,
            .fields = &union_fields_const,
            .layout = .auto,
        } });
        break :blk .{ Enum, Union };
    };

    return union(enum(u1)) {
        flag: FlagUnion,
        positional: PositionalUnion,

        pub const FlagTag = FlagEnum;
        pub const PositionalTag = PositionalEnum;

        pub const _FlagUnion = FlagUnion;
        pub const _PositionalUnion = PositionalUnion;
    };
}

pub inline fn ParseContext(comptime flags: []const Flag, comptime positionals: []const Positional) type {
    const FlagsInt = std.meta.Int(.unsigned, flags.len);
    const FoundFlags, const RequiredFlags = blk: {
        var found_members: [flags.len]Type.StructField = undefined;
        var required_members: [flags.len]Type.StructField = undefined;
        for (flags, 0..) |flag, i| {
            found_members[i] = Type.StructField{
                .type = bool,
                .alignment = 0,
                .default_value_ptr = &@as(bool, false),
                .is_comptime = false,
                .name = flag.ident,
            };
            required_members[i] = Type.StructField{
                .type = bool,
                .alignment = 0,
                .default_value_ptr = &(flag.repeatable or flag.required),
                .is_comptime = false,
                .name = flag.ident,
            };
        }
        const found_as_const = found_members;
        const required_as_const = required_members;
        break :blk .{ @Type(.{ .@"struct" = .{
            .backing_integer = FlagsInt,
            .decls = &.{},
            .fields = &found_as_const,
            .is_tuple = false,
            .layout = .@"packed",
        } }), @Type(.{ .@"struct" = .{
            .backing_integer = FlagsInt,
            .decls = &.{},
            .fields = &required_as_const,
            .is_tuple = false,
            .layout = .@"packed",
        } }) };
    };

    return struct {
        found_flags: FoundFlags = .{},
        required_flags: RequiredFlags = .{},
        positional_index: u32 = 0,

        /// Automatically fails if a flag was found multiple times but is not repeatable.
        pub fn foundFlag(ctx: *@This(), p: *Parser, comptime id: [:0]const u8, repr: []const u8) error{ParseError}!void {
            inline for (flags, 0..) |flag, i| {
                if (comptime std.mem.eql(u8, flag.ident, id)) {
                    if (@field(ctx.found_flags, flag.ident)) {
                        if (!flags[i].repeatable)
                            return p.fail("found duplicate flag '{s}'", .{repr});
                        return;
                    } else {
                        @field(ctx.found_flags, flag.ident) = true;
                        return;
                    }
                }
            }
            @compileError("no flag is designated the identifier '" ++ id ++ "'");
        }

        pub fn foundPositional(ctx: *@This(), p: *Parser, positional: []const u8) error{ParseError}!void {
            if (comptime positionals.len == 0)
                return p.fail("found extra positional '{s}'", .{positional});
            if (ctx.positional_index == positionals.len) {
                if (!positionals[positionals.len - 1].repeatable)
                    return p.fail("found extra positional '{s}'", .{positional});
            } else {
                ctx.positional_index += 1;
            }
        }

        pub fn checkRequirements(ctx: *const @This(), p: *const Parser) error{ParseError}!void {
            // TODO check if doing this in a more traditional way results in
            // similar or better codegen.
            const found_flags: FlagsInt = @bitCast(ctx.found_flags);
            const required_flags: FlagsInt = @bitCast(ctx.required_flags);
            if (found_flags & required_flags != required_flags) {
                assert(comptime flags.len != 0);
                const first_difference_index = @ctz(found_flags ^ required_flags);
                switch (first_difference_index) {
                    inline 0...flags.len - 1 => |i| {
                        return p.fail("missing required flag '{s}'", .{flags[i].flagString(.auto)});
                    },
                    else => unreachable,
                }
            }

            if (comptime positionals.len != 0) {
                comptime var all_positional_displays: [positionals.len][]const u8 = undefined;
                inline for (positionals, 0..) |positional, i| {
                    all_positional_displays[i] = positional.display;
                }
                switch (ctx.positional_index) {
                    inline 0...positionals.len - 1 => |i| {
                        if (comptime !positionals[i].required) {
                            return p.fail("missing required positional '{s}'", .{all_positional_displays[i]});
                        }
                    },
                    positionals.len => {},
                    else => unreachable,
                }
            }
        }
    };
}

pub fn fail(p: *const Parser, comptime format: []const u8, args: anytype) error{ParseError} {
    const err = Error.init(format, args);
    return p.failWithError(&err);
}

pub fn failWithError(p: *const Parser, err: *const Error) error{ParseError} {
    var stderr = std.io.getStdErr();
    stderr.lock(.exclusive) catch return error.ParseError;
    defer stderr.unlock();
    err.emit(stderr.writer(), p.stderr_config);
    return error.ParseError;
}

pub fn fatal(p: *const Parser, comptime format: []const u8, args: anytype) noreturn {
    const err = Error.init(format, args);
    p.failWithError(&err) catch {};
    std.process.exit(1);
}

const Error = @import("Error.zig");
