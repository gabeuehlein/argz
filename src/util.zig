const std = @import("std");
const builtin = @import("builtin");
const argz = @import("argz.zig");

const assert = std.debug.assert;
pub const ParseValueError = error{ IntegerOverflow, NotEnoughArguments, TooManyArguments, InvalidBool, InvalidEnumField } || std.fmt.ParseIntError || std.fmt.ParseFloatError;

pub const FlagType = enum { long, short };

const bool_table = std.StaticStringMap(bool).initComptime(.{ .{ "true", true }, .{ "false", false } });

pub const ArgzType = union(enum) {
    multi: struct {
        storage: union(enum) {
            bounded: struct { max_elems: usize },
            dynamic,
        },
        child: type,
    },
    counter: struct {
        backing_int: type,
    },
    pair: struct {
        lhs_type: type,
        rhs_type: type,
        separator: u21,
    },
    flag_help,
    trailing,
    zig_primitive: type,

    pub fn fromZigType(comptime T: type) ArgzType {
        comptime {
            return if (isCounter(T))
                .{ .counter = .{ .backing_int = @as(T, .{}).__argz_counter_type } }
            else if (isBoundedMulti(T))
                .{ .multi = .{ .storage = .{
                    .bounded = .{
                        .max_elems = @as(T, .{}).__argz_bmulti_len,
                    },
                }, .child = @as(T, .{}).__argz_bmulti_child } }
            else if (isDynamicMulti(T))
                .{ .multi = .{ .storage = .dynamic, .child = @as(T, .{}).__argz_dmulti_child } }
            else if (isPair(T)) blk: {
                const val = @as(T, .{});
                const res = val.__argz_pair_result;
                break :blk .{ .pair = .{ .lhs_type = res[0], .rhs_type = res[1], .separator = val.__argz_pair_separator } };
            } else if (T == argz.FlagHelp)
                .flag_help
            else if (T == argz.Trailing)
                .trailing
            else switch (@typeInfo(T)) {
                .int, .float, .bool, .void, .array, .pointer, .optional => .{ .zig_primitive = T },
                else => @compileError("internal argz error: found unexpected type '" ++ @typeName(T) ++ "'"),
            };
        }
    }

    /// Returns `true` if the type represented by `ty` requires a memory allocator to parse.
    pub fn requiresAllocator(comptime ty: ArgzType) bool {
        return switch (ty) {
            .flag_help, .trailing_positionals, .counter => false,
            .multi => |m| m.storage == .dynamic,
            .pair => |p| ArgzType.fromZigType(p.lhs_type).requiresAllocator() or ArgzType.fromZigType(p.rhs_type).requiresAllocator(),
            .zig_primitive => |prim| switch (@typeInfo(prim)) {
                .pointer => prim != []const u8,
                .optional => |info| info.child != []const u8,
                else => false,
            },
        };
    }
};

pub const ValidationPurpose = enum {
    flag,
    pair_in_flag,
    positional,
    positional_in_flag,
};

pub fn validateType(
    comptime T: type,
    comptime purpose: ValidationPurpose,
    comptime listlike_parent: bool,
    comptime pair_lhs: bool,
    comptime support_allocation: bool,
) void {
    const info = ArgzType.fromZigType(T);
    switch (info) {
        .pair => |p| switch (purpose) {
            .flag, .positional => {
                const new_purpose = switch (purpose) {
                    .flag => .pair_in_flag,
                    .positional => .positional_in_flag,
                    else => unreachable,
                };
                validateType(p.lhs_type, new_purpose, listlike_parent, false, support_allocation);
                validateType(p.rhs_type, new_purpose, listlike_parent, false, support_allocation);
            },
            else => @compileError("invalid type: '" ++ @typeName(T) ++ "'"),
        },
        .counter => {
            if (purpose != .flag)
                @compileError("type '" ++ @typeName(T) ++ "' can only be the type of a flag");
        },
        .multi => |multi| {
            if (purpose != .flag)
                @compileError("type '" ++ @typeName(T) ++ "' can only be the type of a flag");
            if (listlike_parent)
                @compileError("invalid type: '" ++ @typeName(T) ++ "'");
            switch (multi.storage) {
                .bounded => |size| if (size == 0)
                    @compileError("repeatable flag cannot have an upper occurance limit of 0"),
                .dynamic => if (!support_allocation)
                    @compileError("repeatable flag cannot have a variable occurance limit without an allocator"),
            }
            switch (ArgzType.fromZigType(multi.child)) {
                .pair => |pair| {
                    validateType(pair.lhs_type, .pair_in_flag, true, true, support_allocation);
                    validateType(pair.rhs_type, .pair_in_flag, true, false, support_allocation);
                },
                .zig_primitive => validateType(multi.child, .flag, true, support_allocation),
                else => @compileError("invalid type: '" ++ @typeName(T) ++ "'"),
            }
        },
        .zig_primitive => |prim| switch (@typeInfo(prim)) {
            .pointer => |ptr| if (ptr.size != .Slice)
                @compileError("pointer type must be '.Slice'")
            else if (!support_allocation and (!(ptr.child == u8 and ptr.is_const) and ptr.sentinel != null))
                @compileError("type '" ++ @typeName(T) ++ "' is not supported without an allocator")
            else if (listlike_parent and !(ptr.child == u8 and ptr.is_const))
                @compileError("type '" ++ @typeName(T) ++ "' cannot be the child of a list-like type"),
            // This case is to prevent needing to add another parameter to validateType
            .array => |arr| if (listlike_parent)
                @compileError("type '" ++ @typeName(T) ++ "' cannot be the child of a list-like type")
            else switch (@typeInfo(arr)) {
                .pointer => |ptr| if (!(ptr.child == u8 and ptr.is_const))
                    @compileError("invalid type: '" ++ @typeName(T) ++ "'"),
                else => validateType(arr.child, purpose, true, support_allocation),
            },
            .int, .float, .bool, .void => {},
            .optional => |opt| if (pair_lhs)
                @compileError("optional type cannot be the LHS of a pair")
            else if (@typeInfo(opt.child) == .optional)
                @compileError("optional types cannot be nested")
            else switch (purpose) {
                .flag, .positional => validateType(opt.child, purpose, true, false, support_allocation),
                .pair_in_flag, .pair_in_positional => @compileError("pairs of values cannot be nested"),
            },
            else => @compileError("invalid type: '" ++ @typeName(T) ++ "'"),
        },
        .flag_help => if (purpose != .flag)
            @compileError("type '" ++ @typeName(T) ++ "' can only be the type of a flag"),
        .trailing_positionals => if (purpose != .positional)
            @compileError("type '" ++ @typeName(T) ++ "' can only be the type of a positional argument"),
    }
}

pub fn isCounter(comptime T: type) bool {
    return @typeInfo(T) == .@"struct" and @hasField(T, "__argz_counter_type") and argz.Counter(@as(T, .{}).__argz_counter_type) == T;
}

pub fn isBoundedMulti(comptime T: type) bool {
    return @typeInfo(T) == .@"struct" and @hasField(T, "__argz_bmulti_child") and @hasField(T, "__argz_bmulti_len") and blk: {
        const init = @as(T, .{});
        break :blk argz.BoundedMulti(init.__argz_bmulti_child, init.__argz_bmulti_len) == T;
    };
}

pub fn isDynamicMulti(comptime T: type) bool {
    if (!(@typeInfo(T) == .@"struct" and @hasField(T, "__argz_dmulti_child") and @hasField(T, "__argz_dmulti_backing_type"))) {
        return false;
    } else {
        const init = @as(T, .{});
        return argz.DynamicMulti(init.__argz_dmulti_child) == T;
    }
}

pub fn isPair(comptime T: type) bool {
    return @typeInfo(T) == .@"struct" and @hasField(T, "__argz_pair_tag") and @hasField(T, "__argz_pair_result") and @hasField(T, "__argz_pair_separator") and blk: {
        const init = @as(T, .{});
        const P = init.__argz_pair_result;
        break :blk argz.Pair(P[0], P[1], init.__argz_pair_separator) == T;
    };
}
