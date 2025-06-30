pub inline fn Pair(comptime First: type, comptime Second: type, comptime separator: u21, comptime recursion_direction: enum { start, end }) type {
    return struct {
        pub const Target = struct { First, Second };

        pub const argz_custom_type_data: CustomTypeMetadata = .{
            .Resolve = CustomTypeMetadata.always(@as(?type, Target)),

            .parseWithContext = struct {
                fn func(comptime context: Parser.Context, p: *Parser, data_ptr: anytype, first_arg: ?[]const u8, comptime depth: u32) error{ ParseError, OutOfMemory }!void {
                    const string = first_arg orelse (p.tokenizer.argument() catch return p.fail("expected an argument for flag '{s}'", .{context.flag.repr.toStringComptime()}));
                    const separator_index = switch (recursion_direction) {
                        .start => std.mem.indexOf(u8, string, separator_byte_repr),
                        .end => std.mem.lastIndexOf(u8, string, separator_byte_repr),
                    } orelse {
                        if (@typeInfo(Second) != .optional) {
                            switch (context) {
                                .flag => |data| return p.fail("argument to flag '{s}' doesn't contain required separator character '{u}'", .{ data.repr.toStringComptime(), separator }),
                                .positional => |data| return p.fail("separator '{u}' for positional '{s}' required but not found", .{ separator, data.display }),
                            }
                        } else {
                            try values.parseValueAuto(First, &data_ptr[0], p, string, context, depth + 1);
                            data_ptr[1] = null;
                            return;
                        }
                    };
                    try values.parseValueAuto(First, &data_ptr[0], p, string[0..separator_index], context, depth + 1);
                    errdefer values.deinitValueAuto(First, context, p, &data_ptr[0]);
                    try values.parseValueAuto(Second, &data_ptr[1], p, string[separator_index + separator_byte_repr.len ..], context, depth + 1);
                }
            }.func,

            .deinitWithContext = struct {
                fn func(comptime context: Parser.Context, parser: *Parser, state: *Target) void {
                    defer state.* = undefined;
                    values.deinitValueAuto(First, context, parser, &state[0]);
                    values.deinitValueAuto(Second, context, parser, &state[1]);
                }
            }.func,

            .defaultTypeName = struct {
                inline fn func(comptime context: Parser.Context.Tag, comptime depth: u32) ?[:0]const u8 {
                    const first_name = types.typeName(First, context, depth + 1) orelse return null;
                    const second_name = types.typeName(Second, context, depth + 1) orelse return null;
                    const base = first_name ++ separator_byte_repr ++ second_name;
                    return if (depth != 0)
                        "(" ++ base ++ ")"
                    else
                        base;
                }
            }.func,
        };

        inline fn requiresAllocator(comptime context: Parser.Context.Tag) bool {
            return types.requiresAllocator(First, context) or types.requiresAllocator(Second, context);
        }

        const first_custom_data: ?CustomTypeMetadata = if (types.isCustomType(First))
            types.customTypeData(First)
        else
            null;

        const second_custom_data: ?CustomTypeMetadata = if (types.isCustomType(Second))
            types.customTypeData(Second)
        else
            null;

        const separator_byte_repr: []const u8 = &std.unicode.utf8EncodeComptime(separator);

        pub fn StructFieldType(comptime _: Parser.Context.Tag) ?type {
            return Target;
        }

        pub fn defaultValue(comptime _: Parser.Context.Tag) ?Target {
            return null;
        }
    };
}

test Pair {
    // TODO also test failures
    const context: Parser.Context = .{ .flag = .{
        .flag_string = "--test",
        .flag_ty_string = "pair",
    } };
    inline for (.{
        .{
            Pair([]const u8, []const u8, '=', .start), .{
                "a=b",
                "this=a-string",
                "1=23",
                "4=999",
                "blah=blah",
                // these are still valid, since empty strings are perfectly fine
                "=b",
                "c=",
            },
        },
        .{ Pair([:0]const u8, u32, ':', .start), .{
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
            "false",
        } },
    }) |packed_data| {
        const PairTy, const successes = packed_data;
        inline for (successes) |success| {
            var space: PairTy.Target = undefined;
            const argv: [2][:0]const u8 = .{ "program", success };
            var arguments: args.OwnedArgs = .init(&argv);
            var p: Parser = try .init(arguments.args(), .{
                .allocator = std.testing.allocator,
            });
            defer values.deinitValueAuto(PairTy, context, &p, &space);
            try values.parseValueAuto(PairTy, &space, &p, null, context, 0);
        }
    }
}

const Parser = @import("../Parser.zig");
const CustomTypeMetadata = @import("../CustomTypeMetadata.zig");
const std = @import("std");
const types = @import("../types.zig");
const assert = std.debug.assert;
const values = @import("../Parser/values.zig");
const args = @import("../args.zig");
