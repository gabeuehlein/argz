pub const Storage = union(enum) {
    bounded: usize,
    dynamic,
};

pub inline fn Multi(comptime T: type, comptime storage: Storage) type {
    if (@typeInfo(T) == .optional)
        @compileError("Multi does not support optional data types");

    return struct {
        pub const argz_custom_type_data: CustomTypeMetadata = .{
            .ResolveType = struct {
                inline fn func(comptime context: Parser.Context.Tag) ?type {
                    const Child = if (types.isCustomType(T))
                        types.customTypeData(T).ResolveType(context)
                    else 
                        T;
                    return switch (storage) {
                        .bounded => |n| std.BoundedArray(Child, n),
                        .dynamic => std.ArrayListUnmanaged(Child),
                    };
                }
            }.func,

            .parseWithContext = struct {
                fn func(comptime context: Parser.Context, parser: *Parser, data_ptr: anytype, comptime depth: u32) anyerror!void {
                    comptime assert(depth == 0);
                    const elem_ptr = switch (storage) {
                        .bounded => try data_ptr.addOne(),
                        .dynamic => try data_ptr.addOne(parser.allocator.?),
                    };

                    const opt_string: ?[]const u8 = if (types.isCustomType(T))
                        null
                    else 
                        try parser.lexer.argument(context != .positional and types.supportsLeadingDash(T, context));

                    try values.parseValueAuto(@TypeOf(elem_ptr.*), elem_ptr, parser, opt_string, context, depth + 1);
                }
            }.func,

            .deinitWithContext = struct {
                fn func(comptime context: Parser.Context, parser: *Parser, data_ptr: anytype) void {
                    switch (storage) {
                        .bounded => {
                            for (data_ptr.slice()) |*elem| {
                                values.deinitValueAuto(@TypeOf(elem.*), context, parser, elem);
                            }
                        },
                        .dynamic => {
                            for (data_ptr.items) |*elem| {
                                values.deinitValueAuto(@TypeOf(elem.*), context, parser, elem);
                            }
                            data_ptr.deinit(parser.allocator.?);
                        },
                    }
                }
            }.func,

            .repeatable = CustomTypeMetadata.always(true),

            .defaultTypeName = struct {
                inline fn func(comptime context: Parser.Context.Tag, comptime depth: u32) ?[:0]const u8 {
                    switch (context) {
                        .flag => types.typeName(T, context, depth),
                        .positional => (types.typeName(T, context, depth + 1) orelse return null) ++ "...",
                    }
                }
            }.func,

            .initWithDefaultValue = struct {
                inline fn func(comptime _: Parser.Context.Tag, data_ptr: anytype) bool {
                    switch (storage) {
                        .bounded => data_ptr.* = .{ .buffer = undefined, .len = 0 },
                        .dynamic => data_ptr.* = .empty,
                    }
                    return true;
                }
            }.func,
        };
    };
}

test Multi {
    const context: Parser.Context = .{ .flag = .{
        .flag_string = "--test",
        .flag_ty_string = "pair",
    } };
    const MultiTy = Multi([:0]const u8, .dynamic);
    const gpa = std.testing.allocator;
    var space: MultiTy.argz_custom_type_data.ResolveType(context).? = undefined;
    const argv: [6][:0]const u8 = .{ "program", "foo", "bar", "baz", "123", "this is a string with spaces" };
    var arguments: args.OwnedArgs = .init(&argv);
    var p: Parser = try .init(arguments.args(), .{ .allocator = gpa });
    defer values.deinitValueAuto(MultiTy, context, &p, &space);
    assert(MultiTy.argz_custom_type_data.initWithDefaultValue(context, &space));
    inline for (0..5) |_| 
        try values.parseValueAuto(MultiTy, &space, &p, null, context, 0);
    const expected: [5][:0]const u8 = .{ "foo", "bar", "baz", "123", "this is a string with spaces" };
    inline for (0..5) |i|
        try std.testing.expectEqualStrings(space.items[i], expected[i]);
}

const Parser = @import("../Parser.zig");
const CustomTypeMetadata = @import("../CustomTypeMetadata.zig");
const std = @import("std");
const types = @import("../types.zig");
const assert = std.debug.assert;
const values = @import("../Parser/values.zig");
const args = @import("../args.zig");
