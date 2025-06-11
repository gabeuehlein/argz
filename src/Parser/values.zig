const std = @import("std");
const types = @import("../types.zig");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const Parser = @import("../Parser.zig");

/// This is the method that should be used by default for parsing values. It will automatically
/// use either data from the argument provided or will allocate the result on the heap when needed. This logic
/// is consistent with [Parser.deinit]; if you are straying from this logic, do not use [Parser.deinit] or
/// this function.
pub fn parseValueAuto(comptime T: type, data_ptr: anytype, parser: *Parser, opt_string: ?[]const u8, comptime context: Parser.Context, comptime depth: u32) !void {
    if (types.isCustomType(T)) {
        const data = types.customTypeData(T);
        try data.parseWithContext(context, parser, data_ptr, depth);
        return;
    }

    switch (@typeInfo(T)) {
        .optional => |opt_info| {
            if (opt_string == null and parser.lexer.maybe(&.{ .flag_eq }) == null) {
                data_ptr.* = null;
                return;
            }

            if (types.isCustomType(opt_info.child)) {
                if (opt_string) |extra| {
                    const log = std.log.scoped(.argz);
                    log.warn("Received extra string '{s}' that will be unused for parsing type '{s}'. This is likely a bug and may result in unexpected behavior.", .{
                        extra,
                        @typeName(T),
                    });
                }
                const data = types.customTypeData(T);
                var space: data.ResolveType(context) = undefined;
                try data.parseWithContext(context, parser, &space, depth);
                data_ptr.* = space;
                return;
            }
        },
        else => {},
    }

    const string = opt_string orelse try parser.lexer.argument(types.supportsLeadingDash(T, context));
    if (types.requiresAllocator(T, context)) {
        const allocator = parser.allocator orelse @panic("A memory allocator is required to parse '" ++ @typeName(T) ++ "' but one was not provided. This is a bug");
        data_ptr.* = parseValueAlloc(T, allocator, context, parser, string, depth) catch |e| {
            if (e == error.OutOfMemory)
                return e;

            switch (context) {
                .flag => |context_info| {
                    return parser.fail(.{ .invalid_arg_for_flag = .{
                        .arg_string = string,
                        .flag_string = context_info.flag_string,
                        .arg_ty_string = context_info.flag_ty_string,
                    } });
                },
                .positional => |context_info| {
                    return parser.fail(.{ .invalid_positional = .{
                        .arg_string = string,
                        .arg_ty_string = context_info.positional_ty_string,
                        .positional_display_name = context_info.positional_display,
                    } });
                }
            }
        };
    } else {
        data_ptr.* = parseValueNoAlloc(T, context, parser, string, depth) catch |e| switch (e) {
            error.ParseError => return e,
        };
    }
}

/// T must not be a custom type.
pub fn parseValueNoAlloc(comptime T: type, comptime context: Parser.Context, parser: *Parser, string: []const u8, comptime depth: u32) !T {
    switch (@typeInfo(T)) {
        .bool => if (std.mem.eql(u8, string, "true"))
            return true
        else if (std.mem.eql(u8, string, "false"))
            return false
        else return genericInvalidArgError(context, parser, string),
        .int => return std.fmt.parseInt(T, string, 0) catch return genericInvalidArgError(context, parser, string),
        .float => return std.fmt.parseFloat(T, string) catch return genericInvalidArgError(context, parser, string),
        .pointer => {
            comptime assert(T == []const u8);
            return string;
        },
        .array => |info| {
            var result: T = undefined;
            var split = std.mem.splitScalar(u8, string, ',');
            var i: usize = 0;
            while (split.next()) |elem| : (i += 1) {
                if (i >= info.len)
                    return genericInvalidArgError(context, parser, string);
                result[i] = parseValueNoAlloc(info.child, context, parser, elem, depth + 1) catch return genericInvalidArgError(context, parser, string);
            }
            if (i != info.len) {
                if (@typeInfo(info.child) == .optional) {
                    @memset(result[i..], null);
                } else {
                    return genericInvalidArgError(context, parser, string);
                }
            }
            return result;
        },
        .optional => |info| return try parseValueNoAlloc(info.child, context, parser, string, depth + 1),
        .error_set => |info| {
            const errors = info orelse @compileError("inferred error sets cannot be parsed");
            for (errors) |err| {
                if (std.mem.eql(u8, string, err.name))
                    return @field(T, err.name);
            }
            return genericInvalidArgError(context, parser, string);
        },
        .@"enum" => {
            if (context == .flag)
                _ = parser.lexer.maybe(&.{.flag_eq});
            return std.meta.stringToEnum(T, string) orelse error.UnknownEnumValue;
        },
        .vector => |info| {
            var result: T = undefined;
            var split = std.mem.splitScalar(u8, string, ',');
            var i: usize = 0;
            while (split.next()) |elem| : (i += 1) {
                if (i >= info.len)
                    return genericInvalidArgError(context, parser, string);
                result[i] = parseValueNoAlloc(info.child, context, parser, elem, depth + 1) catch return genericInvalidArgError(context, parser, string);
            }
            if (i != info.len)
                return genericInvalidArgError(context, parser, string);
            return result;
        },
        // comptime-only types
        .type,
        .comptime_int,
        .comptime_float,
        .@"fn",
        .enum_literal,
        // types without a concrete definition
        .undefined,
        .null,
        .frame,
        .@"anyframe",
        .@"opaque",
        // types that would be nonsensical to parse/would not
        // have a reasonable way to do so in a user-friendly way.
        .@"union",
        .@"struct",
        .noreturn,
        .void,
        .error_union,
        => @compileError("cannot parse type '" ++ @typeName(T) ++ "'"),
    }
}

/// T must not be a custom type. Will not accept types that do not need an allocator to parse.
pub fn parseValueAlloc(comptime T: type, gpa: Allocator, context: Parser.Context, parser: *Parser, string: []const u8, comptime depth: u32) !T {
    return switch (@typeInfo(T)) {
        .pointer => |info| {
            comptime assert(T != []const u8);
            if (comptime info.sentinel_ptr != null and info.child == u8 and info.is_const) {
                const result = try gpa.allocSentinel(u8, string.len, info.sentinel().?);
                @memcpy(result, string);
                return result;
            }

            var list: std.ArrayListUnmanaged(info.child) = .empty;
            errdefer {
                if (types.requiresAllocator(info.child)) {
                    for (list.items) |item| {
                        gpa.free(item);
                    }
                }
            }

            var split = std.mem.splitScalar(u8, string, ',');
            while (split.next()) |elem| {
                var item: info.child = undefined;
                try parseValueAuto(info.child, &item, parser, elem, context, depth + 1);
                try list.append(item);
            }
        },
        else => comptime unreachable,
    };
}

pub inline fn deinitValueAuto(comptime T: type, comptime context: Parser.Context, parser: *Parser, val: *types.StructField(T, context)) void {
    if (types.isCustomType(T)) {
        const data = types.customTypeData(T);
        data.deinitWithContext(context, parser, val);
    } else if (comptime types.requiresAllocator(T, context)) {
        parser.allocator.?.free(val.*);
    }
}

inline fn genericInvalidArgError(comptime context: Parser.Context, parser: *Parser, string: []const u8) error{ParseError} {
    return switch (context) {
        .flag => |info| parser.fail(.{
            .invalid_arg_for_flag = .{
                .arg_string = string,
                .arg_ty_string = info.flag_ty_string,
                .flag_string = info.flag_string,
            },
        }),
        .positional => |info| parser.fail(.{
            .invalid_positional = .{
                .arg_string = string,
                .arg_ty_string = info.positional_ty_string,
                .positional_display_name = info.positional_display,
            },
        }),
    };
}
