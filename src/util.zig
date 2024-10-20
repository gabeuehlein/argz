const root = @import("argz.zig");
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

const bool_table = std.StaticStringMap(bool).initComptime(.{ .{ "true", true }, .{ "false", false } });

pub fn typeHasDynamicValue(comptime T: type) bool {
    @setEvalBranchQuota(30000);
    if (comptime isCounter(T) or T == root.FlagHelp)
        return false
    else if (comptime isBoundedMulti(T)) comptime {
        return typeHasDynamicValue(@as(T, .{}).__argz_bmulti_child);
    } else if (comptime isDynamicMulti(T)) {
        return true;
    } else if (comptime isPair(T)) {
        const PairResult = @as(T, .{}).__argz_pair_result;
        inline for (PairResult) |Ty| {
            switch (@typeInfo(Ty)) {
                .int, .float, .bool, .optional, .array, .pointer => return true,
                else => @compileError("invalid type: '" ++ @typeName(Ty) ++ "'"),
            }
        }
        return typeHasDynamicValue(PairResult[0]) or typeHasDynamicValue(PairResult[1]);
    } else {
        comptime var array = false;
        comptime var optional = false;
        comptime var slice = false;
        comptime return sw: switch (@typeInfo(T)) {
            .int, .float, .bool, .@"enum" => break :sw false,
            .void => if (optional or slice or array) @compileError("invalid type: '" ++ @typeName(T) ++ "'") else break :sw false,
            .array => |arr| if (slice or array) @compileError("nested arrays or slices are not supported") else {
                array = true;
                continue :sw @typeInfo(arr.child);
            },
            .optional => |opt| {
                if (slice)
                    @compileError("invalid type: '" ++ @typeName(T) ++ "'");
                optional = true;
                continue :sw @typeInfo(opt.child);
            },
            .pointer => |ptr| {
                if (ptr.size != .Slice)
                    @compileError("pointer types must be slices");
                if (ptr.child == u8 and ptr.sentinel == null and ptr.is_const)
                    break :sw slice
                else if (slice or array)
                    @compileError("invalid type: '" ++ @typeName(T) ++ "'")
                else {
                    slice = true;
                    continue :sw @typeInfo(ptr.child);
                }
            },
            else => unreachable,
        };
    }
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
    if (@typeInfo(T) == .@"struct") {
        return if (@hasField(T, "__argz_counter_type"))
            switch (@typeInfo(@as(T, .{}).__argz_counter_type)) {
                .int => |int| int.bits != 0,
                else => false,
            }
        else
            false;
    } else return false;
}

pub fn isBoundedMulti(comptime T: type) bool {
    return @typeInfo(T) == .@"struct" and @hasField(T, "__argz_bmulti_child") and @TypeOf(@as(T, .{}).__argz_bmulti_child) == type and @hasField(T, "__argz_bmulti_len") and @hasField(T, "__argz_bmulti_backing_type");
}

pub fn isDynamicMulti(comptime T: type) bool {
    if (!(@typeInfo(T) == .@"struct" and @hasField(T, "__argz_dmulti_child") and @hasField(T, "__argz_dmulti_backing_type"))) {
        return false;
    } else {
        const Child = @as(T, .{}).__argz_dmulti_child;
        return @TypeOf(Child) == type;
    }
}

pub fn isPair(comptime T: type) bool {
    return @typeInfo(T) == .@"struct" and @hasField(T, "__argz_pair_tag") and @hasField(T, "__argz_pair_result") and @hasField(T, "__argz_pair_separator");
}
