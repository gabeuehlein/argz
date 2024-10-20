const std = @import("std");
const util = @import("util.zig");
const argz = @import("argz.zig");
const assert = std.debug.assert;

const bool_table = std.StaticStringMap(bool).initComptime(.{
    .{ "true", true },
    .{ "false", false },
});

pub fn ParseStaticValueReturnType(comptime T: type) type {
    return if (util.isPair(T))
        ParsePairReturnType(T)
    else
        T;
}

pub fn ParseDynamicValueReturnType(comptime T: type) type {
    return if (comptime util.isPair(T)) blk: {
        const Result = @as(T, .{}).__argz_pair_result;
        break :blk struct { Result[0], Result[1] };
    } else T;
}

pub fn parseStaticValue(comptime T: type, string: []const u8) !T {
    if (comptime util.isPair(T))
        return parsePair(T, string);

    return switch (@typeInfo(T)) {
        .optional => |opt| if (@typeInfo(opt.child) == .optional)
            @compileError("nested optional value '" ++ @typeName(T) ++ "' is not an allowed type")
        else
            try parseStaticValue(opt.child, string),
        .int => std.fmt.parseInt(T, string, 0),
        .float => std.fmt.parseFloat(T, string),
        .bool => bool_table.get(string) orelse error.InvalidBool,
        .array => |arr| parseArray(arr.child, arr.len, string),
        .@"enum" => inline for (std.meta.fields(T)) |field| {
            if (std.mem.eql(u8, string, field.name))
                break @as(T, @enumFromInt(field.value));
        } else error.InvalidEnumField,
        .pointer => if (T != []const u8)
            @compileError("type '" ++ @typeName(T) ++ "' cannot be parsed without a memory allocator")
        else
            string,
        else => @compileError("parsing type '" ++ @typeName(T) ++ "' is either not supported or cannot be performed without a memory allocator"),
    };
}

pub fn parseArray(comptime Child: type, comptime len: usize, string: []const u8) ![len]Child {
    const is_optional = @typeInfo(Child) == .optional;
    var result: [len]Child = if (is_optional)
        null ** len
    else
        undefined;
    var i: usize = 0;

    const Inner = switch (@typeInfo(Child)) {
        .optional => |opt| opt.child,
        else => Child,
    };

    var split = std.mem.splitScalar(u8, string, ',');
    while (split.next()) |elem| : (i += 1) {
        if (i == len)
            return error.TooManyElements;
        result[i] = try switch (@typeInfo(Inner)) {
            .int => std.fmt.parseInt(Inner, elem, 0),
            .float => std.fmt.parseFloat(Inner, elem),
            .bool => bool_table.get(string) orelse return error.InvalidBool,
            .@"enum" => inline for (std.meta.fields(Child)) |field| {
                if (std.mem.eql(u8, string, field.name))
                    break @as(Child, @enumFromInt(field.value));
            } else return error.InvalidEnumField,
            .pointer => if (Inner != []const u8)
                @compileError("invalid array child type: '" ++ @typeName(Child) ++ "'")
            else
                elem,
            else => @compileError("invalid array child type: '" ++ @typeName(Child) ++ "'"),
        };
    }

    if (!is_optional)
        if (i != len)
            return error.TooFewElements;

    return result;
}

pub fn parseDynamicValue(comptime T: type, allocator: std.mem.Allocator, string: []const u8, comptime escape_commas_in_strings: bool) !ParseDynamicValueReturnType(T) {
    if (comptime util.isPair(T))
        return try parsePairAlloc(T, allocator, string, escape_commas_in_strings)
    else if (T == []const u8)
        return string
    else
        return switch (@typeInfo(T)) {
            .optional => |opt| if (@typeInfo(opt.child) == .optional)
                @compileError("nested optional value '" ++ @typeName(T) ++ "' is not an allowed type")
            else
                try parseDynamicValue(opt.child, allocator, string, escape_commas_in_strings),
            .pointer => |ptr| if (ptr.size != .Slice)
                @compileError("pointer size must be 'Slice', not '" ++ @tagName(ptr.size) ++ "'")
            else blk: {
                switch (@typeInfo(ptr.child)) {
                    .optional => @compileError("optional values are not supported when parsing slices"),
                    .array => @compileError("arrays cannot be the child element of a slice"),
                    .pointer => if (ptr.child != []const u8) @compileError("parsing nested slices is not supported"),
                    else => {}, // parseStaticValue will take care of emitting the other errors
                }
                var buf = std.ArrayListUnmanaged(ptr.child).empty;
                errdefer {
                    if (escape_commas_in_strings and ptr.child == []const u8) {
                        for (buf.items) |itm| {
                            allocator.free(itm);
                        }
                    }
                    buf.deinit(allocator);
                }
                if (escape_commas_in_strings and ptr.child == []const u8) {
                    var tmp = std.ArrayListUnmanaged(u8).empty;
                    errdefer tmp.deinit(allocator);

                    var i = @as(usize, 0);
                    while (i < string.len) : (i += 1) {
                        if (string[i] == '\\')
                            if (i + 1 < string.len and string[i + 1] == ',') {
                                try tmp.append(allocator, ',');
                                i += 1;
                            } else try tmp.append(allocator, '\\')
                        else if (string[i] == ',') {
                            try buf.append(allocator, try tmp.toOwnedSlice(allocator));
                        } else try tmp.append(allocator, string[i]);
                    }
                    try buf.append(allocator, try tmp.toOwnedSlice(allocator));
                } else {
                    var split = std.mem.splitScalar(u8, string, ',');
                    while (split.next()) |elem| {
                        try buf.append(allocator, try parseStaticValue(ptr.child, elem));
                    }
                }
                break :blk try buf.toOwnedSlice(allocator);
            },

            else => parseStaticValue(T, string),
        };
}

pub fn ParsePairReturnType(comptime T: type) type {
    if (!util.isPair(T))
        @compileError("expected a 'Pair', but found a '" ++ @typeName(T) ++ "'");

    const V = @as(T, .{}).__argz_pair_result;
    if (@typeInfo(V[0]) == .optional)
        @compileError("the first part of a 'Pair' may not be an optional type");
    // do basic type checking to make sure the user didn't pass in bogus arguments
    // (optional second type with invalid child, void, etc.)
    _, _ = .{ ParseStaticValueReturnType(V[0]), ParseStaticValueReturnType(V[1]) };
    return struct { V[0], V[1] };
}

pub fn parsePair(comptime T: type, string: []const u8) !ParsePairReturnType(T) {
    const V = @as(T, .{}).__argz_pair_result;
    const First, const Second = .{ V[0], V[1] };
    const sep = @as(T, .{}).__argz_pair_separator;
    const bytes = std.unicode.utf8EncodeComptime(sep);
    if (std.mem.indexOf(u8, string, &bytes)) |sep_idx| {
        if (sep_idx + bytes.len == string.len)
            return error.NothingAfterSeparator;
        const first_val = string[0..sep_idx];
        const second_val = string[sep_idx + bytes.len ..];
        return .{ try parseStaticValue(First, first_val), try parseStaticValue(Second, second_val) };
    } else if (@typeInfo(Second) != .optional) {
        return error.MissingSeparator;
    } else {
        return .{ try parseStaticValue(First, string), null };
    }
}

pub fn parsePairAlloc(comptime T: type, allocator: std.mem.Allocator, string: []const u8, comptime escape_commas_in_strings: bool) !ParseDynamicValueReturnType(T) {
    const V = @as(T, .{}).__argz_pair_result;
    const First, const Second = .{ V[0], V[1] };
    const sep = @as(T, .{}).__argz_pair_separator;
    const bytes = std.unicode.utf8EncodeComptime(sep);
    if (std.mem.indexOf(u8, string, &bytes)) |sep_idx| {
        if (sep_idx + bytes.len == string.len)
            return error.NothingAfterSeparator;
        const first_val = string[0..sep_idx];
        const second_val = string[sep_idx + bytes.len ..];
        const first_data = try parseDynamicValue(First, allocator, first_val, escape_commas_in_strings);
        errdefer freeDynamicValue(First, first_data, allocator, escape_commas_in_strings);
        const second_data = try parseDynamicValue(Second, allocator, second_val, escape_commas_in_strings);
        return .{ first_data, second_data };
    } else if (@typeInfo(Second) != .optional) {
        return error.MissingSeparator;
    } else {
        return .{ try parseDynamicValue(First, allocator, string, escape_commas_in_strings), null };
    }
}

pub fn freeDynamicValue(comptime InternalType: type, value: anytype, allocator: std.mem.Allocator, comptime escape_commas_in_strings: bool) void {
    if (comptime util.isBoundedMulti(InternalType)) {
        const Child = @as(InternalType, .{}).__argz_bmulti_child;
        if (Child == []const u8) {
            for (value.items) |string| {
                allocator.free(string);
            }
        } else if (comptime util.typeHasDynamicValue(Child)) {
            for (value.constSlice()) |elem| {
                freeDynamicValue(@TypeOf(elem), elem, allocator, escape_commas_in_strings);
            }
        }
    } else if (comptime util.isDynamicMulti(InternalType)) {
        const Child = @as(InternalType, .{}).__argz_dmulti_child;
        if (escape_commas_in_strings and Child == []const u8) {
            for (value.items) |string| {
                allocator.free(string);
            }
        } else if (comptime util.typeHasDynamicValue(Child)) {
            for (value.items) |elem| {
                freeDynamicValue(@TypeOf(elem), elem, allocator, escape_commas_in_strings);
            }
        }
        value.deinit(allocator);
    } else if (comptime util.isPair(InternalType)) {
        const PairResult = @as(InternalType, .{}).__argz_pair_result;
        if (comptime util.typeHasDynamicValue(PairResult[0])) {
            freeDynamicValue(@TypeOf(value[0]), value[0], allocator, escape_commas_in_strings);
        }
        if (comptime util.typeHasDynamicValue(PairResult[1])) {
            freeDynamicValue(@TypeOf(value[1]), value[1], allocator, escape_commas_in_strings);
        }
    } else switch (@typeInfo(InternalType)) {
        .pointer => |ptr| if (InternalType != []const u8) {
            if (ptr.child == []const u8 and escape_commas_in_strings) {
                for (value) |string| {
                    allocator.free(string);
                }
            }
            allocator.free(value);
        },
        .optional => |opt| {
            comptime assert(@typeInfo(opt.child != .optional));
            if (value) |val| {
                freeDynamicValue(opt.child, val, allocator, escape_commas_in_strings);
            }
        },
        else => {},
    }
}
