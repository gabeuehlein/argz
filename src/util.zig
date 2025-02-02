const std = @import("std");
const builtin = @import("builtin");
const argz = @import("argz.zig");

const assert = std.debug.assert;
pub const ParseValueError = error{
    IntegerOverflow,
    NotEnoughArguments,
    TooManyArguments,
    InvalidBool,
    InvalidEnumField,
} || std.fmt.ParseIntError || std.fmt.ParseFloatError;

pub const FlagType = enum { long, short };

const bool_table = std.StaticStringMap(bool).initComptime(.{ .{ "true", true }, .{ "false", false } });

pub const ArgzType = union(enum) {
    multi: struct {
        storage: union(enum) {
            bounded: usize,
            dynamic,
        },
        Child: type,
    },
    counter: struct {
        backing_int: type,
    },
    pair: struct {
        Lhs: type,
        Rhs: type,
        separator: u21,
    },
    flag_help,
    trailing,
    zig_primitive: type,

    pub inline fn fromZigType(comptime T: type) ArgzType {
        return comptime if (isCounter(T))
            .{ .counter = .{ .backing_int = T.argz_counter_int } }
        else if (isPair(T))
            .{ .pair = .{ .Lhs = T.Lhs, .Rhs = T.Rhs, .separator = T.separator } }
        else if (T == argz.FlagHelp)
            .flag_help
        else if (T == argz.Trailing)
            .trailing
        else if (isMulti(T))
            .{ .multi = .{ .Child = T.Child, .storage = switch (T.storage) {
                .bounded => |sz| .{ .bounded = sz },
                .dynamic => .dynamic,
            } } }
        else switch (@typeInfo(T)) {
            .int, .float, .bool, .void, .array, .pointer, .optional, .@"enum" => .{ .zig_primitive = T },
            else => @compileError("invalid type provided: '" ++ @typeName(T) ++ "'"),
        };
    }

    /// Returns `true` if the type represented by `ty` requires a memory allocator to parse.
    pub inline fn requiresAllocator(comptime ty: ArgzType) bool {
        return switch (ty) {
            .flag_help, .trailing, .counter => false,
            .multi => |m| m.storage == .dynamic,
            .pair => |p| ArgzType.fromZigType(p.Lhs).requiresAllocator() or ArgzType.fromZigType(p.Rhs).requiresAllocator(),
            .zig_primitive => |prim| switch (@typeInfo(prim)) {
                .pointer => prim != []const u8,
                .optional => |info| ArgzType.fromZigType(info.child).requiresAllocator(),
                else => false,
            },
        };
    }

    pub const ResolveMode = enum {
        struct_field,
        parse_value,
    };

    pub inline fn ResolveType(comptime ty: ArgzType, comptime mode: ResolveMode) type {
        return switch (mode) {
            .struct_field => switch (ty) {
                .trailing => argz.TrailingPositionals,
                .pair => |p| struct { p.Lhs, p.Rhs },
                .counter => |c| c.backing_int,
                .flag_help => void,
                .multi => |m| switch (m.storage) {
                    .bounded => |sz| std.BoundedArray(m.Child, sz),
                    .dynamic => std.ArrayListUnmanaged(m.Child),
                },
                .zig_primitive => |prim| if (prim == void) bool else prim,
            },
            .parse_value => switch (ty) {
                .trailing, .counter, .flag_help => comptime unreachable,
                .multi => |m| m.child,
                .zig_primitive => |prim| if (prim == void)
                    comptime unreachable
                else
                    prim,
                .pair => |p| argz.Pair(p.Lhs, p.Rhs, p.separator),
            },
        };
    }

    pub const ValidationPurpose2 = enum {
        flag,
        pair_in_flag,
        pair_in_positional,
        positional,
    };

    pub inline fn validateType(comptime ty: ArgzType, purpose: ValidationPurpose, support_allocation: void) void {
        ty.validateTypeInner(purpose, support_allocation, false, false);
    }

    inline fn validateTypeInner(
        comptime ty: ArgzType,
        purpose: ValidationPurpose,
        support_allocation: bool,
        listlike_parent: bool,
        is_pair_lhs: bool,
    ) void {
        switch (ty) {
            .pair => |p| {
                const new_purpose = switch (purpose) {
                    .flag => .pair_in_flag,
                    .positional => .pair_in_positional,
                    .pair_in_flag => if (is_pair_lhs)
                        @compileError("nested pairs must be the RHS of a pair")
                    else
                        .pair_in_flag,
                    else => @compileError("invalid type"), // TODO improve this error message
                };
                if (!std.unicode.utf8ValidCodepoint(p.separator))
                    @compileError("decimal value '" ++ std.fmt.comptimePrint("{}", .{p.separator}) ++ "' is not a valid UTF-8 codepoint");
                ArgzType.fromZigType(p.Lhs).validateTypeInner(new_purpose, support_allocation, listlike_parent, true);
                ArgzType.fromZigType(p.Rhs).validateTypeInner(new_purpose, support_allocation, listlike_parent, false);
            },
            .trailing => switch (purpose) {
                .positional => {},
                else => @compileError("Trailing must be the type of a positional"),
            },
            .flag_help => switch (purpose) {
                .flag => {},
                else => @compileError("FlagHelp must be the top-level type of a flag"),
            },
            .zig_primitive => |prim| switch (@typeInfo(prim)) {
                .array => |arr| if (listlike_parent)
                    @compileError("nested arrays are not supported")
                else
                    ArgzType.fromZigType(arr.child).validateTypeInner(purpose, support_allocation, true, is_pair_lhs),
                .pointer => |ptr| if (ptr.size != .slice)
                    @compileError("pointer types must be slices")
                else if (prim != []const u8 and !support_allocation)
                    @compileError("type '" ++ @typeName(prim) ++ "' is not supported without a memory allocator")
                else
                    ArgzType.fromZigType(ptr.child).validateTypeInner(purpose, support_allocation, true, is_pair_lhs),

                else => {},
            },
        }
    }
};

pub const ValidationPurpose = enum {
    flag,
    pair_in_flag,
    pair_in_positional,
    positional,
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
                    validateType(pair.Lhs, .pair_in_flag, true, true, support_allocation);
                    validateType(pair.Rhs, .pair_in_flag, true, false, support_allocation);
                },
                .zig_primitive => validateType(multi.child, .flag, true, support_allocation),
                else => @compileError("invalid type: '" ++ @typeName(T) ++ "'"),
            }
        },
        .zig_primitive => |prim| switch (@typeInfo(prim)) {
            .pointer => |ptr| if (ptr.size != .slice)
                @compileError("pointer type must be '.Slice'")
            else if (!support_allocation and (!(ptr.child == u8 and ptr.is_const) and ptr.sentinel != null))
                @compileError("type '" ++ @typeName(T) ++ "' is not supported without an allocator")
            else if (listlike_parent and !(ptr.child == u8 and ptr.is_const))
                @compileError("type '" ++ @typeName(T) ++ "' cannot be the child of a list-like type"),
            // This case is to prevent needing to add another parameter to validateType
            .array => |arr| if (listlike_parent)
                @compileError("type '" ++ @typeName(T) ++ "' cannot be the child of a list-like type")
            else switch (@typeInfo(arr.child)) {
                .pointer => |ptr| if (!(ptr.child == u8 and ptr.is_const))
                    @compileError("invalid type: '" ++ @typeName(T) ++ "'"),
                else => validateType(arr.child, purpose, true, false, support_allocation),
            },
            .int, .float, .bool, .void, .@"enum" => {},
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
        .trailing => if (purpose != .positional)
            @compileError("type '" ++ @typeName(T) ++ "' can only be the type of a positional argument"),
    }
}

pub fn isCounter(comptime T: type) bool {
    return @typeInfo(T) == .@"struct" and @hasDecl(T, "argz_counter_int") and @TypeOf(T.argz_counter_int) == type and switch (@typeInfo(T.argz_counter_int)) {
        .int => |info| info.bits != 0,
        else => false,
    };
}

pub fn isMulti(comptime T: type) bool {
    return @typeInfo(T) == .@"struct" and @hasDecl(T, "argz_multi_tag") and
        @hasDecl(T, "Child") and @TypeOf(T.Child) == type and
        @hasDecl(T, "storage") and @TypeOf(T.storage) == argz.MultiStorage;
}

pub fn isPair(comptime T: type) bool {
    return @typeInfo(T) == .@"struct" and @hasDecl(T, "argz_pair_tag") and
        @hasDecl(T, "Lhs") and @TypeOf(T.Lhs) == type and
        @hasDecl(T, "Rhs") and @TypeOf(T.Rhs) == type and
        @hasDecl(T, "separator") and @TypeOf(T.separator) == u21;
}
