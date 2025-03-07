const std = @import("std");
const builtin = @import("builtin");
const argz = @import("argz.zig");
const Type = std.builtin.Type;

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
    sequence: []const type,
    flag_help,
    trailing,
    zig_primitive: type,

    pub inline fn fromZigType(comptime T: type) ArgzType {
        return comptime if (isCounter(T))
            .{ .counter = .{ .backing_int = T.argz_counter_int } }
        else if (isPair(T))
            .{ .pair = .{ .lhs_type = T.lhs_ty, .rhs_type = T.rhs_ty, .separator = T.separator } }
        else if (isSequence(T))
            .{ .sequence = T.items }
        else if (T == argz.FlagHelp)
            .flag_help
        else if (T == argz.Trailing)
            .trailing
        else if (isMulti(T))
            .{ .multi = .{ .child = T.child, .storage = switch (T.storage) {
                .bounded => |sz| .{ .bounded = sz },
                .dynamic => .dynamic,
            } } }
        else if (isSequence(T))
            .{ .items = .{T.items} }
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
            .pair => |p| ArgzType.fromZigType(p.lhs_type).requiresAllocator() or ArgzType.fromZigType(p.rhs_type).requiresAllocator(),
            .sequence => |s| for (s) |Ty| {
                if (ArgzType.fromZigType(Ty).requiresAllocator())
                    break true;
            } else false,
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

    pub const ResolveFor = enum {
        flag,
        positional,
    };

    // TODO this needs to be audited for correctness. It has some issues right now.
    pub inline fn Resolve(
        comptime ty: ArgzType,
        comptime mode: ResolveMode,
        comptime resolve_for: ResolveFor,
    ) type {
        comptime switch (ty) {
            .flag_help, .trailing, .counter => if (mode == .parse_value)
                @compileError("type '" ++ std.fmt.comptimePrint("{any}", .{ty}) ++ "' should be handled specially in Parser")
            else
                return if (ty == .trailing) argz.TrailingPositionals else void,
            .multi => |m| switch (resolve_for) {
                .flag => switch (mode) {
                    .struct_field => return switch (m.storage) {
                        .bounded => |n| std.BoundedArray(Resolve(.fromZigType(m.child), mode, resolve_for), n),
                        .dynamic => std.ArrayListUnmanaged(Resolve(.fromZigType(m.child), mode, resolve_for)),
                    },
                    .parse_value => return m.child,
                },
                .positional => @compileError("type '" ++ std.fmt.comptimePrint("{any}", .{ty}) ++ "' cannot be the type of a positional"),
            },
            .pair => |p| {
                if (mode == .parse_value)
                    return argz.Pair(p.lhs_ty, p.rhs_ty, p.separator);
                const lhs = ArgzType.fromZigType(p.lhs_type);
                const rhs = ArgzType.fromZigType(p.rhs_type);
                for (.{ lhs, rhs }) |side| {
                    switch (side) {
                        .flag_help, .counter, .trailing, .multi => @compileError("type '" ++ std.fmt.comptimePrint("{any}", .{ty}) ++ "' is not legal"),
                        else => {},
                    }
                }
                return struct { lhs.Resolve(mode, resolve_for), rhs.Resolve(mode, resolve_for) };
            },
            .zig_primitive => |prim| switch (prim) {
                void => switch (resolve_for) {
                    .positional => @compileError("void cannot be the type of a positional"),
                    .flag => return bool,
                },
                else => {
                    var listlike_parent = false;
                    var unwrap_optional = false;
                    state: switch (@typeInfo(prim)) {
                        .int, .float, .bool, .@"enum" => return if (unwrap_optional) @typeInfo(prim).optional.child else prim,
                        .array => |a| {
                            if (listlike_parent)
                                @compileError("type " ++ @typeName(prim) ++ " is not valid");
                            if (a.len == 0)
                                @compileError("array type cannot have a length of zero");
                            listlike_parent = true;
                            continue :state @typeInfo(a.child);
                        },
                        .void => if (listlike_parent)
                            @compileError("type '" ++ @typeName(prim) ++ "' is not valid"),
                        .optional => |o| if (@typeInfo(o.child) == .optional)
                            @compileError("type '" ++ @typeName(prim) ++ "' has a nested optional type")
                        else if (o.child == void)
                            @compileError("type '" ++ @typeName(prim) ++ "' has an optional 'void' type")
                        else if (mode == .struct_field) {
                            _ = ArgzType.fromZigType(o.child).Resolve(mode, resolve_for);
                            return prim;
                        } else {
                            if (@Type(.{ .optional = o }) == prim)
                                unwrap_optional = true;
                            continue :state @typeInfo(o.child);
                        },
                        .pointer => |p| {
                            if (p.size != .slice)
                                @compileError("type '" ++ @typeName(prim) ++ "' contains a non-slice pointer");
                            if (p.is_const and p.child == u8)
                                return if (unwrap_optional) @typeInfo(prim).optional.child else prim;
                            if (listlike_parent)
                                @compileError("type " ++ @typeName(prim) ++ " is not valid");
                            if (mode == .struct_field)
                                // make sure the child is valid
                                continue :state @typeInfo(p.child);
                            listlike_parent = true;
                            return ArgzType.fromZigType(p.child).Resolve(mode, resolve_for);
                        },
                        else => @compileError("type '" ++ @typeName(prim) ++ "' is not valid"),
                    }
                },
            },
            .sequence => |s| {
                var fields: [s.len]Type.StructField = undefined;
                for (s, 0..) |Ty, i| {
                    fields[i] = Type.StructField{
                        .type = ArgzType.fromZigType(Ty).Resolve(mode, resolve_for),
                        .name = std.fmt.comptimePrint("{d}", .{i}),
                        .default_value_ptr = null,
                        .alignment = 0,
                        .is_comptime = false,
                    };
                }
                return @Type(.{ .@"struct" = .{
                    .fields = &fields,
                    .decls = &.{},
                    .is_tuple = true,
                    .layout = .auto,
                } });
            },
        };
    }

    pub const ValidationPurpose = enum {
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
                    @compileError("separator '" ++ std.fmt.comptimePrint("{}", .{p.separator}) ++ "' is not a valid UTF-8 codepoint");
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
            .sequence => |s| switch (purpose) {
                .flag => for (s) |Ty| {
                    ArgzType.validateTypeInner(.fromZigType(Ty), .flag, true, false);
                },
                else => @compileError("sequences must be the top-level type of a flag"),
            },
        }
    }
};

pub fn isCounter(comptime T: type) bool {
    return @typeInfo(T) == .@"struct" and @hasDecl(T, "argz_counter_int") and @TypeOf(T.argz_counter_int) == type and switch (@typeInfo(T.argz_counter_int)) {
        .int => |info| info.bits != 0,
        else => false,
    };
}

pub fn isMulti(comptime T: type) bool {
    return @typeInfo(T) == .@"struct" and @hasDecl(T, "argz_multi_tag") and
        @hasDecl(T, "child") and @TypeOf(T.child) == type and
        @hasDecl(T, "storage") and @TypeOf(T.storage) == argz.MultiStorage;
}

pub fn isPair(comptime T: type) bool {
    return @typeInfo(T) == .@"struct" and @hasDecl(T, "argz_pair_tag") and
        @hasDecl(T, "lhs_ty") and @TypeOf(T.lhs_ty) == type and
        @hasDecl(T, "rhs_ty") and @TypeOf(T.rhs_ty) == type and
        @hasDecl(T, "separator") and @TypeOf(T.separator) == u21;
}

pub fn isSequence(comptime T: type) bool {
    return @typeInfo(T) == .@"struct" and @hasDecl(T, "argz_sequence_tag") and @hasDecl(T, "items") and @TypeOf(T.items) == []const type;
}

pub fn StructFromFlags(comptime flags: []const argz.Flag) type {
    comptime {
        var fields: [flags.len]Type.StructField = undefined;
        for (flags, 0..) |flag, i| {
            const Resolved = ArgzType.fromZigType(flag.type).Resolve(.struct_field, .flag);
            fields[i] = Type.StructField{
                .type = ArgzType.fromZigType(flag.type).Resolve(.struct_field, .flag),
                .name = flag.fieldName(),
                .alignment = 0,
                .is_comptime = false,
                .default_value_ptr = flag.default_value_ptr orelse if (flag.type == void) &false else if (isMulti(flag.type)) blk: {
                    const info = ArgzType.fromZigType(flag.type).multi;
                    break :blk @as(*const Resolved, &switch (info.storage) {
                        .bounded => Resolved.init(0) catch unreachable,
                        .dynamic => Resolved.empty,
                    });
                } else if (@typeInfo(Resolved) == .optional) &@as(Resolved, null) else &@as(Resolved, undefined),
            };
        }
        return @Type(.{ .@"struct" = .{
            .fields = &fields,
            .layout = .auto,
            .decls = &.{},
            .is_tuple = false,
        } });
    }
}

comptime {
    // change false with true to see a number of tested types
    if (false) {
        const from = ArgzType.fromZigType;
        @compileLog(from(u64).Resolve(.struct_field, .flag));
        @compileLog(from(u64).Resolve(.struct_field, .positional));
        @compileLog(from([]const u8).Resolve(.parse_value, .flag));
        @compileLog(from(argz.Pair([]const u8, []u16, '=')).Resolve(.struct_field, .flag));
        @compileLog(from(argz.Pair([]const u8, []u16, '=')).Resolve(.parse_value, .flag));
    }
}
