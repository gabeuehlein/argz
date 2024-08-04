const std = @import("std");
const builtin = @import("builtin");

pub const ParseValueError = error{ IntegerOverflow, NotEnoughArguments, TooManyArguments, InvalidBool } || std.fmt.ParseIntError || std.fmt.ParseFloatError;

pub fn internalTypeError(comptime T: type) noreturn {
    @compileError("`argz` internal error: unexpected type `" ++ @typeName(T) ++ "` found");
}

pub fn typeIsDynamic(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .Pointer => |ptr| if (ptr.size != .Slice)
            internalTypeError(T)
        else
            !ptr.is_const and ptr.child != u8,
        else => false,
    };
}

/// Parses `string` into a float, int, or array. Slices of anything (save for strings)
/// are not permitted.
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
                else => internalTypeError(T),
            }

            break :blk if (i != arr.len)
                error.NotEnoughArguments
            else
                result;
        },
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

pub fn checkValidFlagType(comptime T: type, comptime alloc: bool) void {
    if (isCounter(T))
        return;
    if (isMulti(T))
        if (alloc)
            return
        else
            @compileError("flags with type `Multi` are not supported without a memory allocator");

    switch (@typeInfo(T)) {
        .Int, .Float, .Bool => {},
        .Optional => |opt| {
            if (@typeInfo(opt.child) == .Optional)
                @compileError("`argz` does not support nested optional types");
            checkValidFlagType(opt.child, alloc);
        },
        .Pointer => |ptr| {
            if (T == []const u8)
                return; // []const u8 is unconditionally supported as a type that can be parsed
            if (ptr.size != .Slice)
                @compileError("`argz` does not support parsing non-slice pointer types");
            if (!alloc and (!ptr.is_const and ptr.child != u8)) // []const u8 implies string, []u8 implies slice of `u8`
                @compileError("`argz` does not support parsing variable-length slices without a memory allocator");
            switch (@typeInfo(ptr.child)) {
                .Int, .Float => {},
                .Pointer => |maybe_str| {
                    if (maybe_str.size != .Slice)
                        @compileError("`argz` does not support parsing non-slice pointer types");
                    if (maybe_str.child == u8) {
                        if (!maybe_str.is_const)
                            @compileError(
                                \\ `argz` does not support parsing slices of slices of `u8`; if you wanted to
                                \\ parse slices of strings instead, use `[]const u8` as the slice child type.
                            );
                    } else @compileError("`argz` does not support parsing slices of slices of `" ++ @typeName(maybe_str.child) ++ "`");
                },
                else => @compileError("`argz` does not support parsing slices of `" ++ @typeName(ptr.child) ++ "`"),
            }
        },
        .Array => |arr| {
            switch (@typeInfo(arr.child)) {
                .Int, .Float => {},
                .Pointer => |ptr| {
                    if (ptr.size != .Slice)
                        @compileError("`argz` does not support parsing non-slice pointer types");
                    if (ptr.child == u8) {
                        if (!ptr.is_const)
                            @compileError(
                                \\ `argz` does not support parsing arrays of slices of `u8`; if you wanted to
                                \\ parse arrays of strings instead, use `[]const u8` as the array child type.
                            );
                    } else @compileError("`argz` does not support parsing arrays of slices of `" ++ @typeName(ptr.child) ++ "`");
                },
                else => @compileError("`argz` does not support parsing arrays of `" ++ @typeName(arr.child) ++ "`"),
            }
        },
        else => @compileError("`argz` does not support parsing options with values of type `" ++ @typeName(T) ++ "`"),
    }
}

pub fn checkValidPositionalType(comptime T: type, last_arg: bool) void {
    switch (@typeInfo(T)) {
        .Int, .Float, .Bool => {},
        .Optional => |opt| {
            if (@typeInfo(opt.child) == .Optional) comptime @compileError("`argz` does not support nested optional types");
            checkValidFlagType(opt.child);
        },
        .Pointer => |ptr| {
            if (ptr.size != .Slice)
                @compileError("`argz` does not support parsing non-slice pointer types");
            switch (@typeInfo(ptr.child)) {
                .Int, .Float => {},
                .Pointer => |maybe_str| {
                    if (maybe_str.size != .Slice)
                        @compileError("`argz` does not support parsing non-slice pointer types");
                    if (maybe_str.child == u8) {
                        if (!maybe_str.is_const)
                            @compileError(
                                \\ `argz` does not support parsing slices of slices of `u8`; if you wanted to
                                \\ parse slices of strings instead, use `[]const u8` as the slice child type.
                            );
                    } else @compileError("`argz` does not support parsing slices of slices of `" ++ @typeName(maybe_str.child) ++ "`");
                },
                else => @compileError("`argz` does not support parsing slices of `" ++ @typeName(ptr.child) ++ "`"),
            }
            if (T != []const u8 and !last_arg)
                @compileError("`argz` does not support parsing positionals of type `" ++ @typeName(T) ++ "` that aren't the last positional in the list");
        },
        .Array => |arr| {
            switch (@typeInfo(arr.child)) {
                .Int, .Float => {},
                .Pointer => |ptr| {
                    if (ptr.size != .Slice)
                        @compileError("`argz` does not support parsing non-slice pointer types");
                    if (ptr.child == u8) {
                        if (!ptr.is_const)
                            @compileError(
                                \\ `argz` does not support parsing arrays of slices of `u8`; if you wanted to
                                \\ parse arrays of strings instead, use `[]const u8` as the array child type.
                            );
                    } else @compileError("`argz` does not support parsing arrays of slices of `" ++ @typeName(ptr.child) ++ "`");
                },
                else => @compileError("`argz` does not support parsing arrays of `" ++ @typeName(arr.child) ++ "`"),
            }
        },
        else => @compileError("`argz` does not support parsing options with values of type `" ++ @typeName(T) ++ "`"),
    }
}

pub fn nameForType(comptime T: type) [:0]const u8 {
    if (comptime isCounter(T))
        return "counter";
    if (comptime isMulti(T))
        return "(repeatable) " ++ comptime nameForType(getMultiChildType(T));

    return switch (@typeInfo(T)) {
        .Array => |arr| comptime nameForType(arr.child) ++ std.fmt.comptimePrint(" [{d}]", .{arr.len}),
        .Int => |int| if (int.signedness == .signed) "int" else "uint",
        .Float => "float",
        .Optional => |opt| comptime nameForType(opt.child),
        .Pointer => |ptr| if (T == []const u8) "string" else comptime nameForType(ptr.child) ++ "...",
        .Bool => "bool",
        else => internalTypeError(T),
    };
}

pub fn nameForTypeAdvanced(comptime T: type) [:0]const u8 {
    if (isCounter(T))
        return "counter";
    if (isMulti(T))
        return "repeatable";

    return switch (@typeInfo(T)) {
        .Array => |arr| comptime nameForTypeAdvanced(arr.child) ++ std.fmt.comptimePrint(" [{d}]", .{arr.len}),
        .Int, .Float => @typeName(T),
        .Optional => |opt| @typeName(opt),
        .Pointer => |ptr| @typeName(ptr.child) ++ "...",
        .Bool => "bool",
        else => comptime internalTypeError(T),
    };
}

/// TODO use this for other nameForType functions.
pub fn suffixForType(comptime T: type) [:0]const u8 {
    return switch (@typeInfo(T)) {
        .Array => |arr| std.fmt.comptimePrint(" [{d}]", .{arr.len}),
        .Pointer => if (T == []const u8) "" else "...",
        else => "",
    };
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

pub fn isToggle(comptime T: type) bool {
    return @typeInfo(T) == .Struct and @hasField(T, "__argz_toggle_tag") and @hasField(T, "__argz_counter_type") and @TypeOf(T.__argz_counter_value) == bool;
}

pub fn getToggleValue(comptime T: type) bool {
    if (!isToggle(T))
        internalTypeError(T);
    return @as(*const bool, @ptrCast(@alignCast(std.meta.fieldInfo(T, .__argz_toggle_value).default_value))).*;
}

pub fn isCounter(comptime T: type) bool {
    if (@typeInfo(T) == .Struct) {
        return if (@hasField(T, "__argz_counter_tag") and @hasField(T, "__argz_counter_type"))
            switch (@typeInfo(@as(*const type, @ptrCast(@alignCast(std.meta.fieldInfo(T, .__argz_counter_type).default_value))).*)) {
                .Int => |int| int.bits != 0,
                else => false,
            }
        else
            false;
    } else return false;
}

pub fn isMulti(comptime T: type) bool {
    return @typeInfo(T) == .Struct and @hasField(T, "__argz_multi_tag") and @hasField(T, "__argz_multi_type") and @hasField(T, "__argz_multi_child_type") and @hasField(T, "__argz_multi_stack_len");
}

pub fn getCounterType(comptime T: type) type {
    if (!isCounter(T))
        internalTypeError(T);
    return @as(*const type, @ptrCast(@alignCast(std.meta.fieldInfo(T, .__argz_counter_type).default_value))).*;
}

pub fn getMultiType(comptime T: type) type {
    if (!isMulti(T))
        internalTypeError(T);
    return @as(*const type, @ptrCast(@alignCast(std.meta.fieldInfo(T, .__argz_multi_type).default_value))).*;
}

pub fn getMultiChildType(comptime T: type) type {
    if (!isMulti(T))
        internalTypeError(T);
    return @as(*const type, @ptrCast(@alignCast(std.meta.fieldInfo(T, .__argz_multi_child_type).default_value))).*;
}

pub fn getMultiStackLen(comptime T: type) ?usize {
    if (!isMulti(T))
        internalTypeError(T);

    return @as(*const ?usize, @ptrCast(@alignCast(std.meta.fieldInfo(T, .__argz_multi_stack_len).default_value))).*;
}
