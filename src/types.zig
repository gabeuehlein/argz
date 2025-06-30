const std = @import("std");
const assert = std.debug.assert;

const Args = @import("args.zig").Args;
const argz = @import("argz.zig");
const Parser = @import("Parser.zig");
const values = @import("Parser/values.zig");
const CustomTypeMetadata = @import("CustomTypeMetadata.zig");

pub inline fn isCustomType(comptime T: type) bool {
    return @typeInfo(T) == .@"struct" and @hasDecl(T, "argz_custom_type_data") and @TypeOf(T.argz_custom_type_data) == CustomTypeMetadata;
}

pub inline fn customTypeData(comptime T: type) CustomTypeMetadata {
    comptime assert(isCustomType(T));
    return T.argz_custom_type_data;
}

pub inline fn supportsLeadingDash(comptime T: type, comptime context: Parser.Context.Tag) bool {
    if (isCustomType(T)) {
        const data = customTypeData(T);
        return data.allowsLeadingDash(context);
    }
    return switch (@typeInfo(T)) {
        .int => |i| i.signedness == .signed,
        .float => true,
        .pointer => |p| (p.is_const and p.child == u8) or supportsLeadingDash(p.child),
        else => false,
    };
}

pub fn ResolveType(comptime T: type, context: Parser.Context.Tag) type {
    if (comptime isCustomType(T)) {
        const data = customTypeData(T);
        return data.Resolve(context) orelse @compileError("type '" ++ @typeName(T) ++ "' cannot exist in a struct in a context of '" ++ @tagName(context) ++ "'");
    } else switch (@typeInfo(T)) {
        .int,
        .float,
        .bool,
        => return T,
        .array => |info| {
            if (info.len == 0)
                @compileError("arrays may not have a length of 0");
            state: switch (@typeInfo(info.child)) {
                .int, .bool, .float => {},
                .array => |child_info| {
                    if (child_info.child != u8)
                        @compileError("arrays of u8 (i.e. fixed-length strings) are the only arrays allowed as the child element of arrays");
                },
                .optional => |child_info| {
                    if (@typeInfo(child_info.child) == .optional)
                        @compileError("nested optionals are not allowed");
                    continue :state @typeInfo(child_info.child);
                },
                .@"enum",
                .error_set,
                => {},
                else => @compileError("type '" ++ @typeName(info.child) ++ "' is not allowed as an array's child type"),
            }
            return T;
        },
        // the Zig compiler checks vector types to make sure they're sane, so nothing
        // we need to do here.
        .vector => return T,
        .optional => |info| {
            if (@typeInfo(info.child) == .optional)
                @compileError("nested optionals are not allowed");
            return ?ResolveType(info.child, context);
        },
        .pointer => |info| {
            if (info.size != .slice)
                @compileError("non-slice pointer types are not legal in argz");
            if (info.is_const and info.child == u8)
                // strings are always allowed
                return T;
            switch (@typeInfo(info.child)) {
                .int, .bool, .float => return T,
                .array => |child_info| {
                    if (child_info.child != u8)
                        @compileError("arrays of u8 (i.e. fixed-length strings) are the only arrays allowed as the child element of slices");
                },
                .pointer => |child_info| {
                    if (!(child_info.is_const and child_info.child == u8))
                        @compileError("slices of u8 (i.e. strings) are the only pointer types allowed as the child element of slices");
                },
                else => @compileError("type '" ++ @typeName(info.child) ++ "' is not allowed as a slice's child type"),
            }
            return T;
        },
        // void is for flags that take no argument (i.e. the only data they convey
        // is whether they were found in an argument list).
        .void => {
            if (context != .flag)
                @compileError("type 'void' is only legal in the context of a flag");
            return void;
        },
        .@"enum" => return T,
        else => @compileError("type '" ++ @typeName(T) ++ "' is not legal in argz"),
    }
}

pub inline fn requiresAllocator(comptime T: type, comptime context: Parser.Context.Tag) bool {
    if (isCustomType(T)) {
        const data = customTypeData(T);
        return data.requiresAllocator(context);
    }
    switch (@typeInfo(T)) {
        .int, .float, .bool => return false,
        .array => |info| return requiresAllocator(info.child, context),
        .optional => |info| return requiresAllocator(info.child, context),
        .pointer => return T != []const u8,
        .@"union" => |info| {
            for (info.fields) |field| {
                if (requiresAllocator(field.type, context))
                    return true;
            }
            return false;
        },
        else => return false,
    }
}

pub inline fn typeName(comptime T: type, comptime context: Parser.Context.Tag, comptime depth: u32) ?[:0]const u8 {
    if (isCustomType(T)) {
        const data = customTypeData(T);
        return data.defaultTypeName(context, depth);
    }
    return switch (@typeInfo(T)) {
        .int => "integer",
        .float => "number",
        .bool => "true | false",
        .pointer => |ptr_info| if (ptr_info.child == u8 and ptr_info.is_const)
            "string"
        else
            typeName(ptr_info.child, context, depth + 1) ++ "...",
        .array => |array_info| (typeName(array_info.child, context, depth + 1) orelse return null) ++ "[" ++ comptime std.fmt.comptimePrint("{d}", .{array_info.len}) ++ "]",
        .void => null,
        .@"enum" => |info| blk: {
            assert(info.fields.len != 0);
            if (info.fields.len > 5)
                break :blk @typeName(T);
            comptime var string: [:0]const u8 = info.fields[0].name;
            inline for (info.fields[1..]) |field| {
                string = string ++ " | " ++ field.name;
            }
            if (depth != 0)
                string = "(" ++ string ++ ")";
            break :blk string;
        },
        .optional => |info| "?" ++ (typeName(info.child, context, depth + 1) orelse return null),
        else => blk: {
            break :blk @typeName(T);
        },
    };
}

pub fn StructFromFlags(comptime flags: []const argz.Flag) type {
    comptime var fields: [flags.len]std.builtin.Type.StructField = undefined;
    inline for (flags, 0..) |flag, i| {
        fields[i] = std.builtin.Type.StructField{
            .type = ResolveType(flag.type, .flag),
            .alignment = 0,
            .default_value_ptr = flag.default_value_ptr,
            .is_comptime = false,
            .name = flag.fieldName(),
        };
    }

    return @Type(.{ .@"struct" = .{
        .decls = &.{},
        .fields = &fields,
        .is_tuple = false,
        .layout = .auto,
    } });
}
