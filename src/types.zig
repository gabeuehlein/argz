const std = @import("std");
const argz = @import("argz.zig");
const assert = std.debug.assert;

pub inline fn name(comptime T: type) ?[:0]const u8 {
    return comptime switch (@typeInfo(T)) {
        .void => null,
        .bool => "bool",
        .int => "integer",
        .float => "number",
        .array => |arr| blk: {
            var space: []const u8 = "";
            const child_name = name(arr.child);
            for (0..arr.len) |i| {
                space = space ++ child_name;
                if (i + 1 < space.len)
                    space = space ++ ",";
            }
            break :blk space;
        },
        .pointer => |ptr| blk: {
            assert(ptr.size == .slice);
            if (ptr.child == u8 and ptr.is_const)
                break :blk "string";
            const child_name = name(ptr.child);
            break :blk child_name ++ "[," ++ child_name ++ "...]";
        },
        .@"struct", .@"union" => @compileError("TODO"),
        else => unreachable,
    };
}

pub inline fn mayHaveLeadingDash(comptime T: type) bool {
    return comptime state: switch (@typeInfo(T)) {
        .optional => |opt| continue :state opt.child,
        .int => |info| info.signedness == .signed,
        .float => true,
        .pointer => |info| {
            if (info.child == u8 and info.is_const)
                break :state true
            else
                continue :state info.child;
        },
        else => false,
    };
}

