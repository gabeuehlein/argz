//! Support for user-defined types alongside some utilities for working with them.
//! As `argz` currently uses `comptime` for all argument parsing, all user-provided types
//! must conform to a specific set of requirements to be recognized as a "custom" type:
//!     1. The custom type *must* be a container type that is not `opaque`.
//!     2. The custom type must have the following functions with these exact signatures:
//!     ```zig
//!     /// Returns a non-`null` type based on whether `context` is a flag or a positional.
//!     /// This allows e.g. a custom type to be used in a flag, but not a positional.
//!     pub fn StructFieldType(comptime context: Parser.Context.Tag) ?type;
//!     /// Returns a default value, if any, for the custom type provided the context in which
//!     /// the default value is to be used. If `null`, the flag has no default value and thus
//!     /// must be provided by the user.
//!     pub fn defaultValue(comptime context: Parser.Context.Tag) ?StructFieldType(context).?;
//!     /// Given `opt_string`, the parser's current state, and a pointer to the current state
//!     /// of the parse data for either the flag or positional, manipulates `data` to accomodate any
//!     /// changes caused by the occurrance of `opt_string` in the command line arguments.
//!     /// For types that do not provide a default value and are not repeatable, `data` will always
//!     /// point to undefined memory. Otherwise, it will point to either the default value or the value
//!     /// that a prior call to `parseWithContext` mutated `data` to.
//!     pub fn parseWithContext(comptime env: Parser.Environment, opt_string: ?[]const u8, parser: *Parser, data: *StructFieldType(context).?, comptime depth: u32) !void;
//!     /// Used to free any resources used by `data`. The callee is allowed to assume that `data` has
//!     /// been initialized with meaningful data. For types that don't need to allocate any resources,
//!     /// this may be a no-op (i.e. an empty body).
//!     /// 
//!     /// Once `deinitWithContext` is called once with a particular `data`, `data` will never be referenced
//!     again in a further call `deinitWithContext`.
//!     pub fn deinitWithContext(comptime context: Parser.Context.Tag, parser: *Parser, data: *StructFieldType(context).?) void; 
//!     ```
//!     3. Optionally, the custom type may provide these declarations (*not* fields) to convey
//!     additional information to the parser:
//!     ```zig
//!     /// Indicates that an allocator is mandatory for this type to be parsed.
//!     /// Will cause a compile error if allocation support is disabled and an attempt
//!     /// to parse this type was made or will cause a runtime panic if an allocator was
//!     /// not provided to the parser and an attempt to parse this type is made.
//!     pub const requires_allocator: bool; // defaults to false if not found or `@TypeOf(requires_allocator) != bool`
//!     /// Indicates whether the type is repeatable (that is, can occur multiple times
//!     /// as a flag's type or will consume remaining positionals as a positional's type).
//!     pub const repeatable: bool; // defaults to false if not found or `@TypeOf(repeatable) != bool`
//!     /// Indicates whether the string-representation of the type can have a dash (`'-'`) in front of it.
//!     /// This is used to e.g. allow numeric types to be passed separately from the main argument of a flag.
//!     pub const allow_leading_dash: bool; // defaults to false if not found or `@TypeOf(allow_leading_dash) != bool`
//!     /// For flags, indicates whether the parser should expect at least one argument to be passed to the flag in order
//!     /// to update the state of `data` in `parseWithContext`. If `false`, `opt_string` will *always* be `null`, and if
//!     /// `true`, `opt_string` will *always* be non-null, as the parser will return an error before calling `parseWithContext`.
//!     ///
//!     /// For positionals, a value of `true` implies that this positional is required. If `false`, the positional may or may not
//!     /// be found in the positional list. In the case that it is not, `null` will be passed to `parseWithContext` instead of the parser
//!     /// emitting an error automatically. 
//!     pub const argument_mode: ArgumentMode; // defaults to .mandatory if not found or `@TypeOf(expect_argument) != ArgumentMode`
//!     /// Existence of this function (with the right siganture) allows a custom type to create a prettier format string of its default value
//!     /// as opposed to the `"{any}"` style used by default. If the returned value is `null`, then it is suggested that a default value should
//!     /// not be displayed in any informative strings about a flag or positional.
//!     pub fn defaultValueString(comptime context: Parser.Context.Tag) ?[:0]const u8;
//!     /// Existence of this function (with the right signature allows a custom type to override the default type name printed, which is
//!     /// `@typeName(T)`. This allows custom types to print a more helpful representation of what they expect as an argument, instead of
//!     /// e.g. `Sequence(&.{ u32, u32, u32 })`.
//!     pub fn typeNameString(comptime context: Parser.Context.Tag) ?[:0]const u8;
//!     ```
//! If all of the above is accounted for, the parser will understand a type to be custom and will
//! use the provided interface for parsing such values.
// TODO some of the custom types here may leak memory on parse failure. This should be fixed.

const std = @import("std");
const assert = std.debug.assert;

const Args = @import("args.zig").Args;
const argz = @import("argz.zig");
const Parser = @import("Parser.zig");
const values = @import("Parser/values.zig");

pub const custom = struct {
    pub const ArgumentMode = enum {
        /// Indicates that an argument is mandatory.
        mandatory,
        /// Indicates that an argument is optional. A flag will only receive a
        /// non-`null` value if if the value is attached to the end of the flag 
        /// with an equals symbol (e.g. `--foo=bar` or `-a=b` will interpret `bar` and `b` as
        /// the arguments to `--foo` and `-a` respectively, `but `--foo bar` and `-ab` will interpret
        /// `bar` as a positional or command and `b` as another short flag.
        ///
        /// For positionals, if no positional is provided, the type's `parseWithContext` handler will still
        /// be called.
        optional,
        /// Only legal for flags. Indicates that the flag takes no argument. Using this should generally be avoided
        /// (use [Counter] for counting occurances of a flag), but there are some cases in which calling a handler may be
        /// useful
        none,
    };

    pub const StructFieldTypeSignature = fn(comptime Parser.Context.Tag) ?type;

    pub inline fn ParseWithContextType(comptime StateTy: type) type {
        // Note that the anyerror error set type is just a placeholder. The actual function can return an inferred
        // error set, a concrete one, or `anyerror`.
        return fn(comptime Parser.Environment, ?[]const u8, *Parser, *StateTy, comptime u32) anyerror!void;
    }

    pub inline fn isCustomType(comptime T: type, comptime context: Parser.Context.Tag) bool {
        return (@typeInfo(T) == .@"struct" or @typeInfo(T) == .@"union") and @hasDecl(T, "StructFieldType") and @TypeOf(T.StructFieldType) == StructFieldTypeSignature and
            T.StructFieldType(context) != null and
            @hasDecl(T, "defaultValue") and
            @hasDecl(T, "deinitWithContext") and
            @hasDecl(T, "parseWithContext");
    }

    /// Requires `T` to be a custom type.
    pub inline fn customTypeIsRepeatable(comptime T: type) bool {
        comptime assert(isCustomType(T, .positional) or isCustomType(T, .flag));
        
        return @hasDecl(T, "repeatable") and @TypeOf(T.repeatable) == bool and T.repeatable;
    }

    /// Requires `T` to be a custom type.
    pub inline fn customTypeAllowsLeadingDash(comptime T: type) bool {
        comptime assert(isCustomType(T, .positional) or isCustomType(T, .flag));

        return @hasDecl(T, "allows_leading_dash") and @TypeOf(T.allows_leading_dash) == bool and T.allows_leading_dash;
    }

    /// Requires `T` to be a custom type.
    pub inline fn customTypeArgumentMode(comptime T: type) ArgumentMode {
        return if (@hasDecl(T, "argument_mode") and @TypeOf(T.argument_mode) == ArgumentMode)
            T.argument_mode
        else 
            .mandatory;
    }

    /// Requires `T` to be a custom type.
    pub inline fn customTypeOverridesDefaultValueString(comptime T: type) bool {
        return @hasDecl(T, "defaultValueString") and @TypeOf(T.defaultValueString) == fn(comptime Parser.Context.Tag) ?[:0]const u8;
    }

    /// Requires `T` to be a custom type.
    pub inline fn customTypeOverridesDefaultTypeString(comptime T: type) bool {
        return @hasDecl(T, "typeName") and @TypeOf(T.typeName) == fn(comptime Parser.Context.Tag) ?[:0]const u8;
    }

    pub inline fn verifyCustomType(comptime T: type, comptime context: Parser.Context.Tag) void {
        if (!isCustomType(T, context))
            @compileError("type '" ++ @typeName(T) ++ "' is not legal in the context of '" ++ @tagName(context) ++ "'");
    }

    pub fn nullDefaultValueString(comptime _: Parser.Context.Tag) ?[:0]const u8 {
        return null;
    }

    pub inline fn noopDeinit(comptime StateTy: type) fn (comptime Parser.Context.Tag, *Parser, *StateTy) void {
        return struct {
            fn func(comptime _: Parser.Context.Tag, _: *Parser, _: *StateTy) void {}
        }.func;
    }
};

pub const MultiStorage = union(enum) {
    /// Value is the maximum number of elements that can be parsed.
    bounded: usize,
    dynamic,
};

pub inline fn Counter(comptime T: type) type {
    return struct {
        comptime {
            custom.verifyCustomType(@This(), .flag);
        }
        
        pub const repeatable = true;
        pub const argument_mode: custom.ArgumentMode = .none;

        pub fn StructFieldType(comptime context: Parser.Context.Tag ) ?type {
            return if (context == .flag) T else null;
        }

        pub fn defaultValue(comptime _: Parser.Context.Tag) ?T {
            return 0;
        }

        pub fn parseWithContext(comptime env: Parser.Environment, opt_string: ?[]const u8, _: *Parser, state: *T, comptime depth: u32) !void {
            comptime assert(env.context == .flag);
            comptime assert(depth == 0); // Counter is only legal in top-level flags.
            assert(opt_string == null);
            state.* +|= 1;
        }

        pub const deinitWithContext = custom.noopDeinit(T);

        pub const defaultValueString = custom.nullDefaultValueString;
    };
}


pub inline fn Pair(comptime First: type, comptime Second: type, comptime separator: u21, comptime recursion_direction: enum { start, end }) type {
    return struct {
        comptime {
            custom.verifyCustomType(@This(), .flag);
            custom.verifyCustomType(@This(), .positional);
        }
        
        pub const requires_allocator = requiresAllocator(First) or requiresAllocator(Second); 
        pub const Target = struct { First, Second };

        const separator_byte_repr: []const u8 = &std.unicode.utf8EncodeComptime(separator);
        
        pub fn StructFieldType(comptime _: Parser.Context.Tag) ?type {
            return Target;
        }

        pub fn defaultValue(comptime _: Parser.Context.Tag) ?Target {
            return null;
        }

        pub fn deinitWithContext(comptime context: Parser.Context.Tag, parser: *Parser, state: *Target) void {
            defer state.* = undefined;
            const allocator = parser.allocator.?;

            if (custom.isCustomType(First, context)) {
                First.deinitWithContext(context, parser, &state[0]);
            } else if (requiresAllocator(First)) {
                allocator.free(state[0]);
            }

            if (custom.isCustomType(Second, context)) {
                Second.deinitWithContext(context, parser, &state[1]);
            } else if (requiresAllocator(First)) {
                allocator.free(state[1]);
            }
        }

        pub fn parseWithContext(comptime env: Parser.Environment, opt_string: ?[]const u8, parser: *Parser, state: *Target, comptime depth: u32) !void {
            const string = opt_string.?;
            const separator_index = switch (recursion_direction) {
                .start => std.mem.indexOf(u8, string, separator_byte_repr),
                .end => std.mem.lastIndexOf(u8, string, separator_byte_repr),
            } orelse {
                if (@typeInfo(Second) != .optional) {
                    switch (env.context) {
                        .flag => |data| return parser.fail(.fmt(env.context, "argument to flag '{s}' doesn't contain required separator character '{u}'", .{data.flag_string, separator})),
                        .positional => |data| return parser.fail(.fmt(env.context, "separator '{u}' for positional '{s}' required but not found", .{separator, data.positional_display})),
                    }
                } else {
                    try values.parseValueAuto(First, &state[0], parser, string, env, depth + 1);
                    state[1] = null;
                    return;
                }
            };
            try values.parseValueAuto(First, &state[0], parser, string[0..separator_index], env, depth + 1);
            errdefer values.deinitValueAuto(First, env.context, parser, &state[0]);
            try values.parseValueAuto(Second, &state[1], parser, string[separator_index + separator_byte_repr.len..], env, depth + 1);
        }
    };
}

pub inline fn Multi(comptime T: type, comptime storage: MultiStorage) type {
    return struct {

        comptime {
            custom.verifyCustomType(@This(), .flag);
        }

        comptime {
            if (@typeInfo(T) == .optional)
                @compileError("Multi does not support optional data types");
        }

        pub const repeatable: bool = true;

        pub const requires_allocator = storage == .dynamic or requiresAllocator(T);

        pub const argument_mode: custom.ArgumentMode = if (custom.isCustomType(T, .flag) or custom.isCustomType(T, .positional))
            custom.customTypeArgumentMode(T)
        else .mandatory;

        pub fn StructFieldType(comptime context: Parser.Context.Tag) ?type {
            const Resolved = if (comptime custom.isCustomType(T, .flag) or custom.isCustomType(T, .positional))
                T.StructFieldType(context) orelse return null
            else StructField(T, context);
            return switch (storage) {
                .bounded => |n| std.BoundedArray(Resolved, n),
                .dynamic => std.ArrayListUnmanaged(Resolved),
            };
        }
        
        pub fn defaultValue(comptime context: Parser.Context.Tag) ?StructFieldType(context).? {
            return switch (storage) {
                .bounded => .{ .buffer = undefined, .len = 0 },
                .dynamic => .empty,
            };
        }

        pub fn parseWithContext(comptime env: Parser.Environment, opt_string: ?[]const u8, parser: *Parser, data: *StructFieldType(env.context).?, comptime depth: u32) !void {
            comptime assert(depth == 0); // Multi is only legal in top-level flags and positionals.
            if (custom.isCustomType(T, env.context)) {
                assert(opt_string == null);
                const ptr = switch (storage) {
                    .dynamic => blk: {
                        const allocator = parser.allocator orelse @panic("No allocator provided, but one is required. This is a bug");
                        break :blk try data.addOne(allocator);
                    },
                    .bounded => try data.addOne(),
                };
                try T.parseWithContext(env, opt_string, parser, ptr, depth + 1);
            } else {
                if (parser.allocator) |gpa| {
                    switch (storage) {
                        .dynamic => try data.ensureUnusedCapacity(gpa, 1),
                        .bounded => try data.ensureUnusedCapacity(1),
                    }
                    const string = opt_string.?;
                    const val = if (requiresAllocator(T)) 
                        try values.parseValueAlloc(T, gpa, env.context, parser, string, depth + 1)
                    else 
                        try values.parseValueNoAlloc(T, env.context, parser, string, depth + 1);
                    data.appendAssumeCapacity(val);
                    return;
                } else {
                    if (requiresAllocator(T))
                        @panic("No allocator provided, but one is required to parse '" ++ @typeName(T) ++ ".' This is a bug.");
                    switch (storage) {
                        .dynamic => @panic("No allocator provided, but one is required. This is a bug."),
                        .bounded => try data.ensureUnusedCapacity(1),
                    }
                    const string = opt_string.?;
                    const val = try values.parseValueNoAlloc(T, env.context, parser, string, depth + 1);
                    data.appendAssumeCapacity(val);
                    return;
                }
            }
        }

        pub fn deinitWithContext(comptime context: Parser.Context.Tag, parser: *Parser, data: *StructFieldType(context).?) void {
            switch (storage) {
                .bounded => {
                    if (custom.isCustomType(T, context)) {
                        for (data.slice()) |*item| {
                            item.deinitWithContext(context, parser, item);
                        }
                    } else if (requiresAllocator(T)) {
                        const allocator = parser.allocator.?;
                        for (data.slice()) |item| {
                            allocator.free(item);
                        }
                    }
                },
                .dynamic => {
                    const allocator = parser.allocator.?;
                    if (custom.isCustomType(T, context)) {
                        for (data.items) |*item| {
                            item.deinitWithContext(context, parser, item);
                        }
                    } else if (requiresAllocator(T)) {
                        for (data.items) |item| {
                            allocator.free(item);
                        }
                    }
                    data.deinit(allocator);
                }
            }
        }

        pub const defaultValueString = custom.nullDefaultValueString;
        
        pub fn typeName(comptime context: Parser.Context.Tag) ?[:0]const u8 {
            // TODO change the internal name so this workaround isn't needed
            const types_mod = @import("types.zig"); 
            return types_mod.typeName(T, context);
        }
    };
}

pub const TrailingPositionals = struct {
    comptime {
        custom.verifyCustomType(@This(), .positional);
    }

    // so the parser will set the arguments to an empty argument list
    // if a positional isn't found
    pub const repeatable = true;

    pub fn StructFieldType(comptime context: Parser.Context.Tag) ?type {
        return switch (context) {
            .flag => null,
            .positional => struct {
                args: Args,
                index: usize,

                pub const argument_mode: custom.ArgumentMode = .optional;
                
                pub const Iterator = struct {
                    args: Args,
                    index: usize,

                    pub fn next(it: *Iterator) ?[]const u8 {
                        if (it.index >= it.args.len)
                            return null;
                        const arg = it.args.get(it.index);
                        it.index += 1;
                        return arg;
                    }
                };

                pub fn init(args: Args, index: usize) @This() {
                    return .{ .args = args, .index = index };
                }

                pub fn iterator(self: @This()) Iterator {
                    return .{ .args = self.args, .index = self.index };
                }
            },
        };
    }

    pub fn defaultValue(comptime context: Parser.Context.Tag) ?StructFieldType(.positional).? {
        comptime assert(context == .positional);
        return .init(.empty, 0);
    }

    pub fn parseWithContext(comptime env: Parser.Environment, opt_string: ?[]const u8, parser: *Parser, data: *StructFieldType(.positional).?, comptime depth: u32) !void {
        comptime assert(env.context == .positional);
        comptime assert(depth == 0);

        if (opt_string != null) {
            data.* = .{ .args = .empty, .index = 0 };
        } else {
            if (!parser.lexer.found_force_stop) {
                return parser.fail(.{
                    .invalid_positional = .{
                        .arg_string = opt_string.?,
                        .arg_ty_string = env.context.positional.positional_ty_string,
                        .positional_display_name = env.context.positional.positional_display,
                    },
                });
            }
            data.* = .{ .args = parser.lexer.args, .index = parser.lexer.argi - 1 };
            parser.lexer.argi = parser.lexer.args.len;
        }
    }

    pub const deinitWithContext = custom.noopDeinit(StructFieldType(.positional).?);
};

pub const FlagHelp = struct {
    comptime {
        custom.verifyCustomType(@This(), .flag);
    }

    pub const argument_mode: custom.ArgumentMode = .optional;

    pub fn StructFieldType(comptime context: Parser.Context.Tag) ?type {
        return switch (context) {
            .flag => void,
            .positional => null,
        };
    }

    pub fn defaultValue(comptime _: Parser.Context.Tag) ?void {
        return {};
    }

    pub fn parseWithContext(comptime env: Parser.Environment, opt_string: ?[]const u8, parser: *Parser, _: *void, comptime depth: u32) !void {
        comptime assert(env.context == .flag);
        comptime assert(depth == 0);
        var stdout = std.io.getStdOut();
        const config = parser.stdout_config;
        const writer = stdout.writer();

        if (opt_string) |string| {
            const PairTy = Pair([]const u8, ?[]const u8, ':', .start);
            var storage: PairTy.Target = undefined;
            try values.parseValueAuto(PairTy, &storage, parser, string, env, depth);
            if (storage[1] != null) {
                const help_category, const help_topic_nullable = storage;
                const help_topic = help_topic_nullable.?;
                const Category = enum { cmd, command, pos, positional, flag };
                var tmp: [32]u8 = undefined;
                for (0.., help_category[0..@min(help_category.len, 32)]) |i, chr| {
                    tmp[i] = std.ascii.toLower(chr);
                }
                switch (std.meta.stringToEnum(Category, tmp[0..help_category.len]) orelse return error.UnknownHelpCategory) {
                    .command, .cmd => switch (env.current_mode) {
                        .commands => |cmds| {
                            inline for (cmds) |cmd| {
                                if (std.mem.eql(u8, cmd.cmd, help_topic)) {
                                    try config.setColor(writer, .white);
                                    try config.setColor(writer, .bold);
                                    try writer.writeAll("Help for command '" ++ cmd.cmd ++ "':");
                                    try config.setColor(writer, .reset);
                                    const help = cmd.info orelse (cmd.help_msg orelse return error.NoHelpAvailable);
                                    try writer.writeAll("\n" ++ help ++ "\n");
                                    std.process.exit(0);
                                }
                            }
                            return parser.fail(.fmt(env.context, "unknown help topic '{s}'", .{string}));
                        },
                        .positionals => return parser.fail(.fmt(env.context, "unknown help topic '{s}'", .{string})),
                    },
                    .positional, .pos => switch (env.current_mode) {
                        .positionals => |positionals| {
                            inline for (positionals) |pos| {
                                if (std.mem.eql(u8, pos.displayString(), help_topic)) {
                                    const help = pos.info orelse (pos.help_msg orelse return error.NoHelpAvailable);
                                    try config.setColor(writer, .white);
                                    try config.setColor(writer, .bold);
                                    try writer.writeAll("Help for command '" ++ pos.displayString() ++ "':");
                                    try config.setColor(writer, .reset);
                                    try writer.writeAll("\n" ++ help ++ "\n");
                                    std.process.exit(0);
                                }
                            }
                            return parser.fail(.fmt(env.context, "unknown help topic '{s}'", .{string}));
                        },
                        .commands => return parser.fail(.fmt(env.context, "unknown help topic '{s}'", .{string})),
                    },
                    .flag => {
                        inline for (env.flags) |flag| {
                            const matches_long = if (flag.long) |long|
                                std.mem.eql(u8, long, help_topic)
                            else
                                false;
                            const matches_short = if (flag.short) |short|
                                std.mem.eql(u8, &comptime std.unicode.utf8EncodeComptime(short), help_topic)
                            else
                                false;
                            if (matches_long or matches_short) {
                                const flag_string = switch (matches_long) {
                                    inline true => if (flag.long == null) unreachable else flag.flagString(.long),
                                    inline false => if (flag.short == null) unreachable else flag.flagString(.short),
                                };
                                const help = flag.info orelse (flag.help_msg orelse return error.NoHelpAvailable);
                                try config.setColor(writer, .white);
                                try config.setColor(writer, .bold);
                                try writer.writeAll("Help for flag '");
                                try writer.writeAll(flag_string);
                                try writer.writeAll("':");
                                try config.setColor(writer, .reset);
                                try writer.writeAll("\n" ++ help ++ "\n");
                                std.process.exit(0);
                            }
                        }
                        return parser.fail(.fmt(env.context, "unknown help topic '{s}'", .{string}));
                    },
                }
            } else {
                return parser.fail(.fmt(env.context, "TODO: guess help topic without category", .{})); 
            }
        } else {
            try Parser.formatters.prologue(
                config,
                env.top_level_config,
                env.command_stack,
                env.current_mode,
                env.flags,
                parser.program_name orelse parser.lexer.args.get(0),
                parser.program_description,
                writer.any(),
            );
            switch (env.current_mode) {
                .positionals => {},
                .commands => |commands| {
                    try Parser.formatters.commands(config, commands, writer.any());
                },
            }
            try Parser.formatters.flags(config, env.flags, writer.any());
        }
        std.process.exit(0);
    }

    pub const deinitWithContext = custom.noopDeinit(StructFieldType(.flag).?);

    pub const defaultValueString = custom.nullDefaultValueString;

    pub fn typeName(comptime _: Parser.Context.Tag) ?[:0]const u8 {
        return null;
    }
};

pub fn Sequence(comptime types: []const type) type {
    if (types.len == 0)
        @compileError("empty Sequences carry no information; to carry a single bit of information, use a type of void or bool");
    return struct {
        comptime {
            custom.verifyCustomType(@This(), .flag);
        }

        const Target = std.meta.Tuple(types);

        pub const argument_mode: custom.ArgumentMode = .none;
        
        pub const requires_allocator = blk: {
            break :blk for (types) |Ty| {
                if (requiresAllocator(Ty))
                    break true;
            } else false;
        };

        pub fn StructFieldType(comptime context: Parser.Context.Tag) ?type {
            return switch (context) {
                .flag => Target,
                .positional => null,
            };
        }

        pub fn defaultValue(comptime context: Parser.Context.Tag) ?Target {
            comptime assert(context == .flag);
            return null;
        }

        pub fn parseWithContext(comptime env: Parser.Environment, opt_string: ?[]const u8, parser: *Parser, data: *Target, comptime depth: u32) !void {
            comptime assert(env.context == .flag);
            assert(opt_string == null);
            var last_index: usize = 0;

            errdefer {
                for (0..last_index) |i| {
                    switch (i) {
                        inline 0...types.len - 1 => |inline_i| {
                            values.deinitValueAuto(types[inline_i], env.context, parser, &data[inline_i]);
                        },
                        else => unreachable,
                    }
                }
            }
            
            inline for (types, 0..) |Ty, i| {
                const string = try parser.lexer.argument(typeSupportsLeadingDash(Ty, .flag));
                try values.parseValueAuto(Ty, &data[i], parser, string, env, depth + 1);
                last_index = i;
            }
        }

        pub fn deinitWithContext(comptime context: Parser.Context.Tag, parser: *Parser, data: *Target) void {
            inline for (types, 0..) |Ty, i| {
                values.deinitValueAuto(Ty, context, parser, &data[i]);
            }
        }

        pub const defaultValueString = custom.nullDefaultValueString;

        pub fn typeName(comptime context: Parser.Context.Tag) ?[:0]const u8 {
            comptime assert(context == .flag);
            // TODO change the internal name so this workaround isn't needed
            const types_mod = @import("types.zig"); 
            comptime var result: [:0]const u8 = types_mod.typeName(types[0], context) orelse "[missing type name: " ++ @typeName(types[0]) ++ "]";
            inline for (types[1..]) |Type| {
                result = result ++ "> <" ++ (types_mod.typeName(Type, context) orelse "[missing type name: " ++ @typeName(types[0]) ++ "]");
            }
            return result;
        }
    };
}

pub inline fn typeSupportsLeadingDash(comptime T: type, comptime context: Parser.Context.Tag) bool {
    if (custom.isCustomType(T, context))
        return custom.customTypeAllowsLeadingDash(T);
    return switch (@typeInfo(T)) {
        .int => |i| i.signedness == .signed,
        .float => true,
        .pointer => |p| (p.is_const and p.child == u8) or typeSupportsLeadingDash(p.child),
        else => false,
    };
}

pub fn StructField(comptime T: type, context: Parser.Context.Tag) type {
    if (comptime custom.isCustomType(T, context))
        return T.StructFieldType(context) orelse @compileError("type '" ++ @typeName(T) ++ "' cannot exist in a struct in a context of '" ++ @tagName(context) ++ "'")
    else switch (@typeInfo(T)) {
        .int, .float, .bool, => return T,
        .array => |info| {
            if (info.len == 0)
                @compileError("arrays may not have a length of 0");
            state: switch (@typeInfo(info.child)) {
                .int, .bool, .float => {},
                .array => |child_info| {
                    if (child_info.child != u8)
                        @compileError("arrays of u8 (i.e. fixed-length strings) are the only arrays allowed as the child element of arrays");
                },
                .optional => |child_info| {
                    if (@typeInfo(child_info.child) == .optional)
                        @compileError("nested optionals are not allowed");
                    continue :state @typeInfo(child_info.child);
                },
                .@"enum", .error_set, => {},
                else => @compileError("type '" ++ @typeName(info.child) ++ "' is not allowed as an array's child type"),
            }
            return T;
        },
        // the Zig compiler checks vector types to make sure they're sane, so nothing
        // we need to do here.
        .vector => return T,
        .optional => |info| {
            if (@typeInfo(info.child) == .optional)
                @compileError("nested optionals are not allowed");
            return ?StructField(info.child, context); 
        },
        .pointer => |info| {
            if (info.size != .slice)
                @compileError("non-slice pointer types are not legal in argz");
            if (info.is_const and info.child == u8)
                // strings are always allowed
                return T;
            switch (@typeInfo(info.child)) {
                .int, .bool, .float => return T,
                .array => |child_info| {
                    if (child_info.child != u8)
                        @compileError("arrays of u8 (i.e. fixed-length strings) are the only arrays allowed as the child element of slices");
                },
                .pointer => |child_info| {
                    if (!(child_info.is_const and child_info.child == u8))
                        @compileError("slices of u8 (i.e. strings) are the only pointer types allowed as the child element of slices");
                },
                else => @compileError("type '" ++ @typeName(info.child) ++ "' is not allowed as a slice's child type")
            }
            return T;
        },
        // void is for flags that take no argument (i.e. the only data they convey
        // is whether they were found in an argument list).
        .void => {
            if (context != .flag)
                @compileError("type 'void' is only legal in the context of a flag");
            return bool;
        },
        else => @compileError("type '" ++ @typeName(T) ++ "' is not legal in argz"),
    }
}


/// Requires `T` to be a container type.
pub inline fn hasCustomStructFieldType(comptime T: type) bool {
    return @hasDecl(T, "StructFieldType") and @TypeOf(T.StructFieldType) == fn (Parser.Context.Tag) ?type;
}

pub inline fn customTypeParseableAs(comptime T: type, comptime context: std.meta.Tag(Parser.Context)) bool {
    if (hasCustomStructFieldType(T)) {
        const StructFieldType = T.StructFieldType(context) orelse return false;
        if (@hasDecl(T, "parseWithContext")) {
            switch (@typeInfo(@TypeOf(T.parseWithContext))) {
                .@"fn" => |info| {
                    if (info.params.len == 5 and
                        info.params[0].type == Parser.Context and
                        info.params[1].type == *StructFieldType and 
                        info.params[2].type == *Parser and
                        info.params[3].type == ?[]const u8 and
                        info.params[4].type == u32 and
                        info.return_type != null and
                        @typeInfo(info.return_type.?) == .error_union and
                        @typeInfo(info.return_type.?).error_union.payload == void)
                    {
                        return true;
                    }
                },
                else => return false,
            }
        }
    }
    return false;
}

pub inline fn requiresAllocator(comptime T: type) bool {
    if (custom.isCustomType(T, .flag) or custom.isCustomType(T, .positional))
        return @hasDecl(T, "requires_allocator") and @TypeOf(T.requires_allocator) == bool and T.requires_allocator;
    switch (@typeInfo(T)) {
        .int, .float, .bool => return false,
        .array => |info| return requiresAllocator(info.child),
        .optional => |info| return requiresAllocator(info.child),
        .pointer => |info| return info.sentinel_ptr != null or !(info.child == u8 and info.is_const),
        .@"union" => |info| {
            for (info.fields) |field| {
                if (requiresAllocator(field.type))
                    return true;
            }
            return false;
        },
        else => return false,
    }
}

pub inline fn typeName(comptime T: type, comptime context: Parser.Context.Tag) ?[:0]const u8 {
    if (custom.isCustomType(T, context)) {
        if (custom.customTypeOverridesDefaultTypeString(T)) {
            if (T.typeName(context)) |s|
                return s
            else 
                return null;
        }
    }
    return switch (@typeInfo(T)) {
        .int => "integer",
        .float => "number",
        .bool => "true | false",
        .pointer => |ptr_info| 
            if (ptr_info.child == u8 and ptr_info.is_const)
                "STRING"
            else 
                typeName(ptr_info.child, context) ++ "...",
        .array => |array_info| (typeName(array_info.child, context) orelse return null) ++ "[" ++ comptime std.fmt.comptimePrint("{d}", .{array_info.len}) ++ "]",
        .void => null,
        .@"enum" => |info| blk: {
            assert(info.fields.len != 0);
            if (false and info.fields.len > 5)
                break :blk @typeName(T);
            comptime var string: []const u8 = "(" ++ info.fields[0].name;
            inline for (info.fields[1..]) |field| {
                string = string ++ " | " ++ field.name;
            }
            break :blk string ++ ")";
        },
        .optional => |info| (typeName(info.child, context) orelse return null),
        else => blk: {
            break :blk @typeName(T);
        },
    };
}

pub fn StructFromFlags(comptime flags: []const argz.Flag) type {
    comptime var fields: [flags.len]std.builtin.Type.StructField = undefined;
    inline for (flags, 0..) |flag, i| {
        fields[i] = std.builtin.Type.StructField{
            .type = StructField(flag.type, .flag),
            .alignment = 0,
            .default_value_ptr = flag.default_value_ptr,
            .is_comptime = false,
            .name = flag.fieldName(),
        };
    }

    return @Type(.{ .@"struct" = .{
        .decls = &.{},
        .fields = &fields,
        .is_tuple = false,
        .layout = .auto,
    } });
}

pub fn TypeFromMode(comptime mode: argz.Mode) type {
    const Type = std.builtin.Type;
    switch (mode) {
        .commands => |commands| {
            comptime var union_fields: [commands.len]Type.UnionField = undefined;
            inline for (commands, 0..) |cmd, i| {
                union_fields[i] = cmd.ToType();
            }
            return 
                @Type(.{ .@"union" = .{
                    .layout = .auto,
                    .fields = &union_fields,
                    .decls = &.{},
                } });
        },
        .positionals => |positionals| {
            comptime var struct_fields: [positionals.len]Type.StructField = undefined;
            inline for (positionals, 0..) |positional, i| {
                struct_fields[i] = StructField(positional.type, .positional);
            }
            return 
                @Type(.{ .@"struct" = .{
                    .layout = .auto,
                    .fields = &struct_fields,
                    .decls = &.{},
                    .is_tuple = false,
                } });
        },
    }
}

pub fn WrapModeAndFlags(comptime mode: argz.Mode, comptime flags: []const argz.Flag) type {
    const FlagsType = StructFromFlags(flags);
    switch (mode) {
        .commands => |commands| {
            comptime var union_fields: [commands.len]std.builtin.Type.UnionField = undefined;
            comptime var enum_fields: [commands.len]std.builtin.Type.EnumField = undefined;
            inline for (commands, 0..) |cmd, i| {
                union_fields[i] = std.builtin.Type.UnionField{
                    .type = cmd.ToType(),
                    .name = cmd.fieldName(),
                    .alignment = 0,
                };
                enum_fields[i] = .{ .name = cmd.fieldName(), .value = i };
            }
            const as_const = union_fields;
            const enum_fields_as_const = enum_fields;
            return struct {
                command: @Type(.{ .@"union" = .{
                    .layout = .auto,
                    .fields = &as_const,
                    .decls = &.{},
                    .tag_type = @Type(.{ .@"enum" = std.builtin.Type.Enum{
                        .fields = &enum_fields_as_const,
                        .tag_type = std.math.IntFittingRange(0, commands.len),
                        .decls = &.{},
                        .is_exhaustive = true,
                    } }),
                } }),
                flags: FlagsType,
            };
        },
        .positionals => |positionals| {
            comptime var struct_fields: [positionals.len]std.builtin.Type.StructField = undefined;
            inline for (positionals, 0..) |positional, i| {
                struct_fields[i] = std.builtin.Type.StructField{
                    .type = StructField(positional.type, .positional),
                    .default_value_ptr = null,
                    .alignment = 0,
                    .is_comptime = false,
                    .name = positional.fieldName(),
                };
            }
            const as_const = struct_fields;
            return struct {
                positionals: @Type(.{ .@"struct" = .{
                    .layout = .auto,
                    .fields = &as_const,
                    .decls = &.{},
                    .is_tuple = false,
                } }),
                flags: FlagsType,
            };
        },
    }
}

// TODO test failures too

test Pair {
    inline for (.{ 
        .{ Pair([]const u8, []const u8, '=', .start), .{
            "a=b",
            "this=a-string",
            "1=23",
            "4=999",
            "blah=blah",
            // these are still valid, since empty strings are perfectly fine
            "=b",
            "c=", 
        } },
        .{ Pair([]const u8, u32, ':', .start), .{
            "foo:1",
            "name:33333",
            "yeah:0",
            "test:9",
            ":4294967295",
            "q:0",
        } },
        .{ Pair(f32, f32, '≠', .start), .{
            "5.4≠9",
            "NaN≠NaN",
            "0≠0.5",
            "9≠22.5",
            "inf≠-inf",
            "0≠NaN", 
        } },
        .{ Pair(bool, ?[]const u8, '=', .start), .{
            "false",
            "true",
            "false=false",
            "true=true",
            "false=foiejrofiejrfoierjfoierjfoierjfoerjfoie",
            "false=foijerogh2tuonwpgneqoivqwprfjwroug", 
        } },
    }) |packed_data| {
        const PairTy, const successes = packed_data;
        inline for (successes) |success| {
            var space: PairTy.Target = undefined;
            var p: Parser = undefined;
            try values.parseValueAuto(PairTy, &space, &p, success, .{ .context = .{ .flag = .{ .flag_string = "--cool-story-bro", .flag_ty_string = "MEME"  } }, .current_mode = .{ .positionals = &.{} }, .flags = &.{}, .command_stack = &.{}, .top_level_config = .{ .mode = .{ .positionals = &.{} } } }, 0);
        }
    }
}
