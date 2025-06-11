
const std = @import("std");
const assert = std.debug.assert;

const Args = @import("args.zig").Args;
const argz = @import("argz.zig");
const Parser = @import("Parser.zig");
const values = @import("Parser/values.zig");
const CustomTypeData = @import("CustomTypeData.zig");

pub inline fn isCustomType(comptime T: type) bool {
    return @typeInfo(T) == .@"struct" and @hasDecl(T, "argz_custom_type_data") and @TypeOf(T.argz_custom_type_data) == CustomTypeData;
}

pub inline fn customTypeData(comptime T: type) CustomTypeData {
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

pub fn StructField(comptime T: type, context: Parser.Context.Tag) type {
    if (comptime isCustomType(T)) {
        const data = customTypeData(T);
        return data.ResolveType(context) orelse @compileError("type '" ++ @typeName(T) ++ "' cannot exist in a struct in a context of '" ++ @tagName(context) ++ "'");
    }
    else switch (@typeInfo(T)) {
        .int, .float, .bool, => return T,
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
                .@"enum", .error_set, => {},
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
            return ?StructField(info.child, context); 
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
                else => @compileError("type '" ++ @typeName(info.child) ++ "' is not allowed as a slice's child type")
            }
            return T;
        },
        // void is for flags that take no argument (i.e. the only data they convey
        // is whether they were found in an argument list).
        .void => {
            if (context != .flag)
                @compileError("type 'void' is only legal in the context of a flag");
            return bool;
        },
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
        .pointer => |ptr_info| 
            if (ptr_info.child == u8 and ptr_info.is_const)
                "string"
            else 
                typeName(ptr_info.child, context, depth + 1) ++ "...",
        .array => |array_info| (typeName(array_info.child, context, depth + 1) orelse return null) ++ "[" ++ comptime std.fmt.comptimePrint("{d}", .{array_info.len}) ++ "]",
        .void => null,
        .@"enum" => |info| blk: {
            assert(info.fields.len != 0);
            if (info.fields.len > 5)
                break :blk @typeName(T);
            comptime var string: []const u8 = info.fields[0].name;
        inline for (info.fields[1..]) |field| {
                string = string ++ " | " ++ field.name;
            }
            if (depth != 0)
                string = "(" ++ string ++ ")";
            break :blk string;
        },
        .optional => |info| (typeName(info.child, context, depth + 1) orelse return null),
        else => blk: {
            break :blk @typeName(T);
        },
    };
}


pub fn StructFromFlags(comptime flags: []const argz.Flag) type {
    comptime var fields: [flags.len]std.builtin.Type.StructField = undefined;
    inline for (flags, 0..) |flag, i| {
        fields[i] = std.builtin.Type.StructField{
            .type = StructField(flag.type, .flag),
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

pub fn TypeFromMode(comptime mode: argz.Mode) type {
    const Type = std.builtin.Type;
    switch (mode) {
        .commands => |commands| {
            comptime var union_fields: [commands.len]Type.UnionField = undefined;
            inline for (commands, 0..) |cmd, i| {
                union_fields[i] = cmd.ToType();
            }
            return 
                @Type(.{ .@"union" = .{
                    .layout = .auto,
                    .fields = &union_fields,
                    .decls = &.{},
                } });
        },
        .positionals => |positionals| {
            comptime var struct_fields: [positionals.len]Type.StructField = undefined;
            inline for (positionals, 0..) |positional, i| {
                struct_fields[i] = StructField(positional.type, .positional);
            }
            return 
                @Type(.{ .@"struct" = .{
                    .layout = .auto,
                    .fields = &struct_fields,
                    .decls = &.{},
                    .is_tuple = false,
                } });
        },
    }
}

pub fn WrapModeAndFlags(comptime mode: argz.Mode, comptime flags: []const argz.Flag) type {
    const FlagsType = StructFromFlags(flags);
    switch (mode) {
        .commands => |commands| {
            comptime var union_fields: [commands.len]std.builtin.Type.UnionField = undefined;
            comptime var enum_fields: [commands.len]std.builtin.Type.EnumField = undefined;
            inline for (commands, 0..) |cmd, i| {
                union_fields[i] = std.builtin.Type.UnionField{
                    .type = cmd.ToType(),
                    .name = cmd.fieldName(),
                    .alignment = 0,
                };
                enum_fields[i] = .{ .name = cmd.fieldName(), .value = i };
            }
            const as_const = union_fields;
            const enum_fields_as_const = enum_fields;
            return struct {
                command: @Type(.{ .@"union" = .{
                    .layout = .auto,
                    .fields = &as_const,
                    .decls = &.{},
                    .tag_type = @Type(.{ .@"enum" = std.builtin.Type.Enum{
                        .fields = &enum_fields_as_const,
                        .tag_type = std.math.IntFittingRange(0, commands.len),
                        .decls = &.{},
                        .is_exhaustive = true,
                    } }),
                } }),
                flags: FlagsType,
            };
        },
        .positionals => |positionals| {
            comptime var struct_fields: [positionals.len]std.builtin.Type.StructField = undefined;
            inline for (positionals, 0..) |positional, i| {
                struct_fields[i] = std.builtin.Type.StructField{
                    .type = StructField(positional.type, .positional),
                    .default_value_ptr = null,
                    .alignment = 0,
                    .is_comptime = false,
                    .name = positional.fieldName(),
                };
            }
            const as_const = struct_fields;
            return struct {
                positionals: @Type(.{ .@"struct" = .{
                    .layout = .auto,
                    .fields = &as_const,
                    .decls = &.{},
                    .is_tuple = false,
                } }),
                flags: FlagsType,
            };
        },
    }
}

