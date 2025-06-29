pub fn Sequence(comptime sequence_types: []const type) type {
    if (sequence_types.len == 0)
        @compileError("empty Sequences carry no information; to carry a single bit of information, use a type of void or bool");
    
    inline for (sequence_types, 0..) |Ty, i| {
        if (@typeInfo(Ty) == .optional) {
            inline for (sequence_types[i..]) |OtherTy| {
                comptime assert(@typeInfo(OtherTy) == .optional);
            }
        }
    }

    return struct {
        const Target = std.meta.Tuple(sequence_types);

        pub const argz_custom_type_data: CustomTypeMetadata = .{
            .ResolveType = struct {
                inline fn func(comptime context: Parser.Context.Tag) ?type {
                    return switch (context) {
                        .flag => Target,
                        .positional => null,
                    };
                }
            }.func,
            
            .initWithDefaultValue = struct {
                inline fn func(comptime _: Parser.Context.Tag, data_ptr: anytype) bool {
                    comptime assert(@TypeOf(data_ptr) == *Target);
                    return false;
                }
            }.func,

            .parseWithContext = struct {
                fn func(comptime context: Parser.Context, parser: *Parser, data_ptr: anytype, comptime depth: u32) anyerror!void {
                    comptime assert(depth == 0);
                    comptime assert(context == .flag);

                    var last_index: usize = 0;

                    errdefer {
                        for (0..last_index) |i| {
                            switch (i) {
                                inline 0...sequence_types.len - 1 => |inline_i| {
                                    values.deinitValueAuto(sequence_types[inline_i], context, parser, &data_ptr[inline_i]);
                                },
                                else => unreachable,
                            }
                        }
                    }
                    
                    inline for (sequence_types, 0..) |Ty, i| {
                        var ok = true;
                        switch (@typeInfo(Ty)) {
                            .optional => |opt_info| {
                                if (types.isCustomType(opt_info.child)) {
                                    if (parser.lexer.peek() == null) {
                                        data_ptr[i] = null;
                                        continue;
                                    }
                                    const data = types.customTypeData(opt_info.child);
                                    var storage: data.ResolveType(context) = undefined;
                                    try data.parseWithContext(context, parser, &storage, depth + 1);
                                    data_ptr[i] = storage;
                                } else {
                                    const arg = parser.lexer.argument(types.supportsLeadingDash(opt_info.child, context)) catch |e| switch (e) {
                                        error.MissingArgument => blk: {
                                            data_ptr[i] = null;
                                            ok = false;
                                            break :blk undefined;
                                        },
                                        else => return e, 
                                    };
                                    if (ok) {
                                        try values.parseValueAuto(opt_info.child, &data_ptr[i], parser, arg, context, depth + 1);
                                    }
                                }
                            },
                            else => {
                                const string = try parser.lexer.argument(types.supportsLeadingDash(Ty, .flag));
                                try values.parseValueAuto(Ty, &data_ptr[i], parser, string, context, depth + 1);
                                last_index = i;
                            },
                        }
                    }
                }
            }.func,

            .deinitWithContext = struct {
                pub fn func(comptime context: Parser.Context, parser: *Parser, state: *Target) void {
                    inline for (sequence_types, 0..) |Ty, i| {
                        switch (@typeInfo(Ty)) {
                            .optional => |opt_info| {
                                if (state[i]) |*real_ptr| {
                                    values.deinitValueAuto(opt_info.child, context, parser, real_ptr);
                                }
                            },
                            else => values.deinitValueAuto(Ty, context, parser, &state[i]),
                        }
                    }
                }
            }.func,

            .defaultTypeName = struct {
                inline fn func(comptime context: Parser.Context.Tag, comptime depth: u32) ?[:0]const u8 {
                    comptime assert(context == .flag);
                    comptime assert(depth == 0);
                    comptime var result: [:0]const u8 = types.typeName(types[0], context) orelse "[missing type name: " ++ @typeName(sequence_types[0]) ++ "]";
                    inline for (types[1..]) |Type| {
                        result = result ++ "> <" ++ (types.typeName(Type, context) orelse "[missing type name: " ++ @typeName(Type) ++ "]");
                    }
                    return result;
                }
            }.func
        };
    };
}

test Sequence {
    const S = Sequence(&.{ u32, u8, []const u8, ?bool, ?f32, ?[]const u8 });
    const context: Parser.Context = .{ .flag = .{
        .flag_string = "--test",
        .flag_ty_string = "counter",
    } };
    const argv: []const [:0]const u8 = &.{
        "program",
        "99",
        "255",
        "blah",
        "false",
    };
    var arguments: args.OwnedArgs = .init(argv);
    var p: Parser = try .init(arguments.args(), .{});
    var space: S.Target = undefined;
    try values.parseValueAuto(S, &space, &p, null, context, 0);
    try std.testing.expectEqualDeep(space, S.Target{
        99,
        255,
        "blah",
        false,
        null,
        null,
    });
}

const Parser = @import("../Parser.zig");
const CustomTypeMetadata = @import("../CustomTypeMetadata.zig");
const std = @import("std");
const types = @import("../types.zig");
const assert = std.debug.assert;
const values = @import("../Parser/values.zig");
const args = @import("../args.zig");
