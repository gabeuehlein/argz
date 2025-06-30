pub fn Sequence(comptime sequence_types: []const type) type {
    if (sequence_types.len == 0)
        @compileError("empty Sequences carry no information; to carry a single bit of information, use a type of void or bool")
    else if (sequence_types.len == 1)
        return sequence_types[0];

    inline for (sequence_types) |Ty| {
        if (@typeInfo(Ty) == .optional) {
            @compileError("optional type '" ++ @typeName(Ty) ++ "' not allowed in Sequence");
        }
    }

    return struct {
        const Target = std.meta.Tuple(sequence_types);

        pub const argz_custom_type_data: CustomTypeMetadata = .{ .Resolve = struct {
            inline fn func(comptime context: Parser.Context.Tag) ?type {
                return switch (context) {
                    .flag => Target,
                    .positional => null,
                };
            }
        }.func, .parseWithContext = struct {
            fn func(comptime context: Parser.Context, parser: *Parser, data_ptr: anytype, first_arg: ?[]const u8, comptime depth: u32) anyerror!void {
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

                try values.parseValueAuto(
                    sequence_types[0],
                    &data_ptr[0],
                    parser,
                    first_arg orelse (parser.tokenizer.argument() catch
                        return parser.fail("expected an argument for flag '{s}'", .{context.flag.repr.toStringComptime()})),
                );

                last_index = 1;

                inline for (sequence_types[1..], 1..) |Ty, i| {
                    const string = try parser.lexer.argument(types.supportsLeadingDash(Ty, .flag));
                    try values.parseValueAuto(Ty, &data_ptr[i], parser, string, context, depth + 1);
                    last_index = i + 1;
                }
            }
        }.func, .deinitWithContext = struct {
            pub fn func(comptime context: Parser.Context, parser: *Parser, state: *Target) void {
                inline for (sequence_types, 0..) |Ty, i| {
                    values.deinitValueAuto(Ty, context, parser, &state[i]);
                }
            }
        }.func, .defaultTypeName = struct {
            inline fn func(comptime context: Parser.Context.Tag, comptime depth: u32) ?[:0]const u8 {
                comptime assert(context == .flag);
                comptime assert(depth == 0);
                comptime var result: [:0]const u8 = types.typeName(types[0], context) orelse return null;
                inline for (types[1..]) |Type| {
                    result = result ++ " + " ++ (types.typeName(Type, context) orelse return null);
                }
                return result;
            }
        }.func };
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
