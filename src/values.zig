const std = @import("std");
const util = @import("util.zig");
const argz = @import("argz.zig");
const assert = std.debug.assert;

const fmt = std.fmt;

const bool_table = std.StaticStringMap(bool).initComptime(.{
    .{ "true", true },
    .{ "false", false },
});

pub fn ParseStaticValueReturnType(comptime T: type) type {
    return switch (util.ArgzType.fromZigType(T)) {
        .pair => |p| struct { p.lhs_type, p.rhs_type },
        .zig_primitive => |ty| blk: {
            if (@typeInfo(ty) == .pointer)
                assert(ty == []const u8);
            break :blk ty;
        },
        .counter => unreachable,
        .trailing,
        .flag_help,
        .multi, // special cases handled elsewhere
        => unreachable,
    };
}

pub fn ParseDynamicValueReturnType(comptime T: type) type {
    return switch (util.ArgzType.fromZigType(T)) {
        .pair => |p| struct { p.lhs_type, p.rhs_type },
        .zig_primitive => |ty| ty,
        .counter => unreachable,
        .trailing,
        .flag_help,
        .multi, // special cases handled elsewhere
        => unreachable,
    };
}

pub fn parseStaticValue(comptime T: type, string: []const u8) !ParseStaticValueReturnType(T) {
    var result = @as(ParseStaticValueReturnType(T), undefined);
    switch (util.ArgzType.fromZigType(T)) {
        .pair => |p| {
            const lhs_info = util.ArgzType.fromZigType(p.lhs_type);
            const forward_sep_index = std.mem.indexOfScalar(u8, string, p.separator);
            if (lhs_info == .pair) {
                @compileError("TODO: nesting pairs in pairs");
            } else if (@typeInfo(p.rhs_type) == .optional) {
                if (forward_sep_index) |idx| {
                    result.lhs = try parseStaticValue(p.lhs_type, string[0..idx]);
                    result.rhs = try parseStaticValue(@typeInfo(p.rhs_type).optional.child, string[idx + 1 ..]);
                } else {
                    result.lhs = try parseStaticValue(p.lhs_type, string);
                    result.rhs = null;
                }
            } else if (forward_sep_index) |idx| {
                result.lhs = try parseStaticValue(p.lhs_type, string[0..idx]);
                result.rhs = try parseStaticValue(@typeInfo(p.rhs_type).optional.child, string[idx + 1 ..]);
            } else {
                return error.InvalidPair;
            }
        },
        .zig_primitive => |prim| switch (@typeInfo(prim)) {
            .int => result = try fmt.parseInt(prim, string, 0),
            .float => result = try fmt.parseFloat(prim, string),
            .array => |arr| {
                var tmp: [arr.len]arr.child = if (@typeInfo(arr.child) == .optional) @splat(null) else undefined;
                var split = std.mem.splitScalar(u8, string, ',');
                var i: usize = 0;
                const is_optional = @typeInfo(arr.child) == .optional;
                while (split.next()) |elem| : (i += 1) {
                    if (i >= arr.len)
                        return error.TooManyElements;
                    tmp[i] = try parseStaticValue(if (is_optional) @typeInfo(arr.child).optional.child else arr.child, elem);
                }
                if (!is_optional and i != arr.len)
                    return error.TooFewElements;
                result = tmp;
            },
            .pointer => {
                comptime assert(prim == []const u8);
                result = string;
            },
            .bool => result = bool_table.get(string) orelse return error.InvalidBool,
            .@"enum" => result = std.meta.stringToEnum(prim, string) orelse return error.InvalidEnumVariant,
            inline else => comptime unreachable,
        },
        inline else => comptime unreachable,
    }
    return result;
}

pub fn parseDynamicValue(comptime T: type, allocator: std.mem.Allocator, string: []const u8) !ParseDynamicValueReturnType(T) {
    var result = @as(ParseDynamicValueReturnType(T), undefined);
    switch (util.ArgzType.fromZigType(T)) {
        .pair => |p| {
            const lhs_info = util.ArgzType.fromZigType(p.lhs_type);
            const forward_sep_index = std.mem.indexOfScalar(u8, string, p.separator);
            if (lhs_info == .pair) {
                @compileError("TODO: nesting pairs in pairs");
            } else if (@typeInfo(p.rhs_type) == .optional) {
                if (forward_sep_index) |idx| {
                    result.lhs = try parseDynamicValue(p.lhs_type, allocator, string[0..idx]);
                    result.rhs = try parseDynamicValue(@typeInfo(p.rhs_type).optional.child, allocator, string[idx + 1 ..]);
                } else {
                    result.lhs = try parseDynamicValue(p.lhs_type, allocator, string);
                    result.rhs = null;
                }
            } else if (forward_sep_index) |idx| {
                result.lhs = try parseDynamicValue(p.lhs_type, allocator, string[0..idx]);
                result.rhs = try parseDynamicValue(@typeInfo(p.rhs_type).optional.child, allocator, string[idx + 1 ..]);
            } else {
                return error.InvalidPair;
            }
        },
        .zig_primitive => |prim| switch (@typeInfo(prim)) {
            .int,
            .float,
            .bool,
            .@"enum",
            => result = try parseStaticValue(prim, string),
            .array => |arr| {
                var split = std.mem.splitScalar(u8, string, ',');
                var i: usize = 0;
                errdefer if (util.ArgzType.fromZigType(arr.child).requiresAllocator()) {
                    for (result[0..i]) |elem| {
                        allocator.free(elem);
                    }
                };
                while (split.next()) |elem| : (i += 1) {
                    if (i > arr.len)
                        return error.TooManyElements;
                    result[i] = try parseDynamicValue(arr.child, elem);
                }
                if (i != arr.len)
                    return error.TooFewElements;
            },
            .pointer => |ptr| {
                if (ptr.is_const and ptr.child == u8) {
                    // string, maybe sentinel-terminated
                    if (ptr.sentinel()) |sentinel| {
                        if (std.mem.indexOfScalar(u8, string, sentinel) != null)
                            return error.InvalidCharacter;
                        const buf = try allocator.allocSentinel(u8, string.len, sentinel);
                        @memcpy(buf, string);
                        result = buf;
                    } else {
                        // no sentinel
                        result = string;
                    }
                } else {
                    var list = std.ArrayListUnmanaged(ptr.child).empty;
                    errdefer if (util.ArgzType.fromZigType(ptr.child).requiresAllocator()) {
                        for (list.items) |item| {
                            allocator.free(item);
                        }
                    };
                    var split = std.mem.splitScalar(u8, string, ',');
                    while (split.next()) |elem| {
                        try list.append(try parseDynamicValue(ptr.child, allocator, elem));
                    }
                }
            },
            else => unreachable,
        },
        inline else => comptime unreachable,
    }
    return result;
}

pub fn freeExt(allocator: std.mem.Allocator, value: anytype) void {
    const argzType = util.ArgzType.fromZigType;
    switch (argzType(@TypeOf(value))) {
        .pair => |p| {
            if (argzType(p.lhs_type).requiresAllocator())
                freeExt(p[0]);
            if (argzType(p.rhs_type).requiresAllocator())
                freeExt(p[1]);
        },
        .multi => |m| {
            if (argzType(m.child).requiresAllocator()) {
                for (value.items) |itm| {
                    freeExt(allocator, itm);
                }
            }
            value.deinit(allocator);
        },
        .zig_primitive => |prim| switch (@typeInfo(prim)) {
            inline .pointer, .array => |agg| if (prim != []const u8) {
                if (argzType(agg.child).requiresAllocator()) {
                    for (value[0..]) |itm| {
                        freeExt(allocator, itm);
                    }
                }
                allocator.free(value);
            },
            else => {},
        },
        else => {},
    }
}

test parseStaticValue {
    inline for (.{
        .{ u32, "12345", 12345 },
        .{ bool, "true", true },
        .{ [4]bool, "true,true,false,false", .{ true, true, false, false } },
        .{ []const u8, "foobar bar baz", "foobar bar baz" },
    }) |data| {
        try std.testing.expectEqual(@as(data[0], data[2]), try parseStaticValue(data[0], data[1]));
    }
}
