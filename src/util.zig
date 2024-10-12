const root = @import("root.zig");
const std = @import("std");
const builtin = @import("builtin");

const assert = std.debug.assert;
pub const ParseValueError = error{ IntegerOverflow, NotEnoughArguments, TooManyArguments, InvalidBool, InvalidEnumField } || std.fmt.ParseIntError || std.fmt.ParseFloatError;

pub const FlagType = enum { long, short };

pub const OptionType = enum {
    flag,
    pair,
    positional,
};

pub fn typeHasDynamicValue(comptime T: type, comptime type_for: OptionType, comptime support_allocation: bool) bool {
    if (comptime isCounter(T) or T == root.FlagHelp)
        return false
    else if (comptime isBoundedMulti(T)) comptime {
        assert(type_for == .flag);
        return typeHasDynamicValue(@as(T, .{}).__argz_bmulti_child, type_for, support_allocation);
    } else if (comptime isDynamicMulti(T))
        true
    else
        return switch (@typeInfo(T)) {
            .Int, .Float, .Bool, .Void => false,
            .Array => |arr| switch (@typeInfo(arr.child)) {
                .Int, .Float, .Bool => false,
                .Pointer => |ptr| if (ptr.size != .Slice) internalTypeError(T) else if (type_for == .positional) T != []const u8 else support_allocation,
                // TODO it might be useful to support [N]const u8 arrays here, so things
                // like 'abcd,efgh,ijkl,mnop' would work.
                else => internalTypeError(T),
            },
            .Pointer => |ptr| blk: {
                if (ptr.size != .Slice)
                    internalTypeError(T);
                if (T == []const u8)
                    break :blk false
                else
                    break :blk switch (@typeInfo(ptr.child)) {
                        .Pointer => if (ptr.child != []const u8)
                            internalTypeError(T)
                        else
                            true,
                        else => true,
                    };
            },
            else => unreachable,
        };
}

pub fn internalTypeError(comptime T: type) noreturn {
    @compileError("`argz` internal error: unexpected type `" ++ @typeName(T) ++ "` found");
}

/// Parses `string` into a float, int, or array. All slices except `[]const u8` are
/// not permitted.
pub fn parseStaticValue(comptime T: type, string: []const u8) ParseValueError!T {
    return switch (@typeInfo(T)) {
        .Int => std.fmt.parseInt(T, string, 0),
        .Float => std.fmt.parseFloat(T, string),
        .Array => |arr| blk: {
            var result = @as(T, undefined);
            var split = std.mem.splitScalar(u8, string, ',');
            var i = @as(usize, 0);
            switch (@typeInfo(arr.child)) {
                .Int => {
                    while (split.next()) |itm| : (i += 1) {
                        if (i >= arr.len)
                            return error.TooManyArguments;
                        result[i] = try std.fmt.parseInt(arr.child, itm, 0);
                    }
                },
                .Float => {
                    while (split.next()) |itm| : (i += 1) {
                        if (i >= arr.len)
                            return error.TooManyArguments;
                        result[i] = try std.fmt.parseFloat(arr.child, itm);
                    }
                },
                .Pointer => |ptr| {
                    if (!(ptr.is_const and ptr.child == u8))
                        internalTypeError(T);

                    // we don't pay attention to escaped commas here, since we can't
                    // allocate our own memory.
                    while (split.next()) |itm| : (i += 1) {
                        if (i >= arr.len)
                            return error.TooManyArguments;
                        result[i] = itm;
                    }
                },
                .Bool => {
                    while (split.next()) |itm| : (i += 1) {
                        if (i >= arr.len)
                            return error.TooManyArguments;
                        result[i] = if (std.mem.eql(u8, itm, "true"))
                            true
                        else if (std.mem.eql(u8, itm, "false"))
                            false
                        else
                            return error.InvalidBool;
                    }
                },
                else => internalTypeError(T),
            }

            break :blk if (i != arr.len)
                error.NotEnoughArguments
            else
                result;
        },
        .Enum => inline for (std.meta.fields(T), 0..) |field, i| {
            if (std.mem.eql(u8, field.name, string))
                break @as(T, @enumFromInt(i));
        } else error.InvalidEnumField,
        .Pointer => blk: {
            if (T != []const u8)
                internalTypeError(T);
            break :blk string;
        },
        .Bool => blk: {
            break :blk if (std.mem.eql(u8, string, "true"))
                true
            else if (std.mem.eql(u8, string, "false"))
                false
            else
                error.InvalidBool;
        },
        else => internalTypeError(T),
    };
}

pub fn parseDynamicValue(comptime T: type, allocator: std.mem.Allocator, string: []const u8) !T {
    return blk: {
        switch (@typeInfo(T)) {
            .Pointer => |ptr| {
                if (ptr.size != .Slice)
                    internalTypeError(T);
                switch (@typeInfo(ptr.child)) {
                    inline .Int, .Float, .Bool => |_, tag| {
                        var it = std.mem.splitScalar(string, ',');
                        var buf = std.ArrayListUnmanaged(T){};
                        errdefer buf.deinit(allocator);

                        while (it.next()) |itm| {
                            const val = switch (tag) {
                                .Int => try std.fmt.parseInt(ptr.child, itm, 0),
                                .Float => try std.fmt.parseFloat(ptr.child, itm),
                                .Bool => if (std.mem.eql(u8, itm, "true")) true else if (std.mem.eql(u8, itm, "false")) false else return error.InvalidBool,
                            };
                            try buf.append(allocator, val);
                        }
                        break :blk buf.toOwnedSlice();
                    },
                    .Pointer => {
                        comptime assert(ptr.child == []const u8);
                        var buf = std.ArrayListUnmanaged([]const u8){};
                        errdefer for (buf.items) |itm| {
                            allocator.free(itm);
                        };

                        var tmp = std.ArrayListUnmanaged(u8){};
                        errdefer tmp.deinit(allocator);

                        var i = @as(usize, 0);
                        while (i < string.len) : (i += 1) {
                            if (string[i] == '\\')
                                if (i + 1 < string.len and string[i + 1] == ',') {
                                    try tmp.append(allocator, ',');
                                    i += 1;
                                } else try tmp.append(allocator, '\\')
                            else if (string[i] == ',') {
                                try buf.append(allocator, try allocator.dupe(u8, tmp.items));
                                tmp.clearRetainingCapacity();
                            } else try tmp.append(allocator, string[i]);
                        }
                        try buf.append(allocator, try tmp.toOwnedSlice(allocator));
                        break :blk buf.toOwnedSlice(allocator);
                    },
                    else => internalTypeError(T),
                }
            },
            .Array => |arr| {
                comptime assert(arr.child == []const u8);

                var result_idx = @as(usize, 0);
                var result = @as([arr.len][]const u8, undefined);
                errdefer for (result[0..result_idx]) |itm| {
                    allocator.free(itm);
                };

                var string_idx = @as(usize, 0);
                var tmp = std.ArrayListUnmanaged(T){};
                errdefer tmp.deinit(allocator);

                while (string_idx < string.len) : (string_idx += 1) {
                    if (result_idx >= arr.len) return error.TooManyArguments;

                    if (string[string_idx] == '\\') {
                        if (string_idx + 1 > string.len and string[string_idx + 1] == ',') {
                            string_idx += 1;
                            try tmp.append(allocator, ',');
                        } else try tmp.append(allocator, '\\');
                    } else if (string[string_idx] == ',') {
                        result[result_idx] = try allocator.dupe(tmp.items);
                        tmp.clearRetainingCapacity();
                        result_idx += 1;
                    } else try tmp.append(allocator, string[string_idx]);
                }
                if (result_idx >= string.len) return error.TooManyArguments else if (result_idx + 1 != string.len) return error.NotEnoughArguments;
                result[result_idx] = try tmp.toOwnedSlice(allocator);
                tmp.deinit(allocator);

                break :blk result;
            },
            else => internalTypeError(T),
        }
    };
}

pub fn parseSlice(comptime Child: type, allocator: std.mem.Allocator, string: []const u8) ![]Child {
    var buf = std.ArrayListUnmanaged(Child){};
    errdefer {
        if (Child == []const u8) {
            for (buf.items) |itm| {
                allocator.free(itm);
            }
        }
        buf.deinit(allocator);
    }
    switch (@typeInfo(Child)) {
        .Int => {
            var it = std.mem.splitScalar(u8, string, ',');
            while (it.next()) |itm| {
                try buf.append(allocator, try std.fmt.parseInt(Child, itm, 0));
            }
        },
        .Float => {
            var it = std.mem.splitScalar(u8, string, ',');
            while (it.next()) |itm| {
                try buf.append(allocator, try std.fmt.parseFloat(Child, itm));
            }
        },
        .Pointer => if (Child == []const u8) {
            // We have to iterate through the string ourselves because we need to handle
            // any '\,' sequences.
            var i = @as(usize, 0);
            var tmp = std.ArrayListUnmanaged(u8){};
            errdefer tmp.deinit(allocator);
            while (i < string.len) : (i += 1) {
                if (string[i] == '\\')
                    if (i + 1 < string.len and string[i + 1] == ',') {
                        try tmp.append(allocator, ',');
                        i += 1;
                    } else try tmp.append(allocator, '\\')
                else if (string[i] == ',') {
                    try buf.append(allocator, try allocator.dupe(u8, tmp.items));
                    tmp.clearRetainingCapacity();
                } else try tmp.append(allocator, string[i]);
            }
            try buf.append(allocator, try tmp.toOwnedSlice(allocator));
        } else internalTypeError(Child),
        .Bool => {
            var it = std.mem.splitScalar(u8, string, ',');
            while (it.next()) |itm| {
                try buf.append(allocator, if (std.mem.eql(u8, itm, "true")) true else if (std.mem.eql(u8, itm, "false")) false else return error.InvalidBool);
            }
        },
        else => internalTypeError(Child),
    }
    return buf.toOwnedSlice(allocator);
}

pub fn runtimeCheck(check: bool, comptime fmt: []const u8, args: anytype) void {
    if (!std.debug.runtime_safety)
        return; // don't do anything on ReleaseFast/ReleaseSmall

    if (!check) {
        var buf = @as([4096]u8, undefined);
        const msg = std.fmt.bufPrint(&buf, "fatal argz arror: " ++ fmt, args) catch blk: {
            const truncated_msg = "... <truncated>";
            @memcpy(buf[buf.len - truncated_msg.len ..][0..truncated_msg.len], truncated_msg);
            break :blk &buf;
        };
        @panic(msg);
    }
}

pub fn hammingDist(a: []const u8, b: []const u8) usize {
    const maxSize = std.math.maxInt(usize) / 8; // `usize` bytes large
    runtimeCheck(a.len < maxSize and b.len < maxSize, "tried computing the Hamming Distance of (an) exceptionally large byte slice(s) (lengths {d} and {d}); possible data corruption?", .{ a.len, b.len });

    var result = @as(usize, 0);
    for (a, b) |ae, be| {
        result += @popCount(ae ^ be);
    }
    return result;
}

pub fn isCounter(comptime T: type) bool {
    if (@typeInfo(T) == .Struct) {
        return if (@hasField(T, "__argz_counter_type"))
            switch (@typeInfo(T.__argz_counter_type)) {
                .Int => |int| int.bits != 0,
                else => false,
            }
        else
            false;
    } else return false;
}

pub fn isBoundedMulti(comptime T: type) bool {
    return @typeInfo(T) == .Struct and @hasField(T, "__argz_bmulti_child") and @TypeOf(@as(T, .{}).__argz_bmulti_child) == type and @hasField(T, "__argz_bmulti_len") and @hasField(T, "__argz_bmulti_backing_type");
}

pub fn isDynamicMulti(comptime T: type) bool {
    if (!(@typeInfo(T) == .Struct and @hasField(T, "__argz_dmulti_child") and @hasField(T, "__argz_dmulti_backing_type"))) {
        return false;
    } else {
        const Child = @as(T, .{}).__argz_dmulti_child;
        return @TypeOf(Child) == type;
    }
}

pub fn isPair(comptime T: type) bool {
    return @typeInfo(T) == .Struct and @hasDecl(T, "__argz_pair_tag") and @hasDecl(T, "Result");
}

test parseStaticValue {
    inline for (.{ .{ u32, "12345", 12345 }, .{ f32, "123.45", 123.45 }, .{ [4]u32, "123,456,789,10", [4]u32{ 123, 456, 789, 10 } }, .{ [2][]const u8, "this shouldn't\\, be escaped", [2][]const u8{ "this shouldn't\\", " be escaped" } }, .{ []const u8, "this is a string", "this is a string" }, .{ enum { a, cool, @"enum" }, "cool", .cool } }) |data| {
        const val = try parseStaticValue(data[0], data[1]);
        try std.testing.expectEqualDeep(val, data[2]);
    }
}

test parseSlice {
    const allocator = std.testing.allocator;
    inline for (.{ .{ u32, "1,2,3,4", &[4]u32{ 1, 2, 3, 4 } }, .{ f32, "1.2,3.4,5.6,7.8", &[4]f32{ 1.2, 3.4, 5.6, 7.8 } }, .{ []const u8, "this should\\, be escaped, but this should not", &[2][]const u8{ "this should, be escaped", " but this should not" } } }) |data| {
        const val = try parseSlice(data[0], allocator, data[1]);
        defer allocator.free(val);
        defer if (data[0] == []const u8) for (val) |itm| {
            allocator.free(itm);
        };
        try std.testing.expectEqualDeep(val, data[2]);
    }
}
