const std = @import("std");
const argz= @import("argz.zig");
const assert = std.debug.assert;
const Parser = @import("Parser.zig");

pub const Context = union(enum) {
    positional: argz.Positional,
    option: argz.Option,

    fn toBadArgError(comptime ctx: Context, arg: []const u8) Parser.Error {
        return switch (ctx) {
            .positional => |pos| .{ .invalid_positional = .{
                .positional = comptime &pos.toRuntime(),
                .arg_repr = arg,
            } },
            .option => |opt| .{ .invalid_arg_for_option = .{
                .option = comptime &opt.toRuntime(),
                .arg_repr = arg,
            } },
        };
    }
};

fn ContextChildType(comptime context: Context) type {
    return switch (context) {
        .positional => |pos| pos.type,
        .option => |opt| opt.type,
    };
}

pub fn parseValue(p: *Parser, value: []const u8, comptime context: Context) error{ParseError}!ContextChildType(context) {
    return parseValueExplicitType(p, value, context, ContextChildType(context));
}

pub fn parseValueExplicitType(p: *Parser, value: []const u8, comptime context: Context, comptime T: type) error{ParseError}!ContextChildType(context) {
    const ToParse = comptime state: switch (T) {
        else => |U| {
            switch (@typeInfo(U)) {
                .optional => |info| continue :state info.child,
                else => break :state U,
            }
        },
    };

    switch (@typeInfo(ToParse)) {
        .int => return std.fmt.parseInt(ToParse, value, 0) catch return p.fail(context.toBadArgError(value)),
        .float => return std.fmt.parseFloat(ToParse, value) catch return p.fail(context.toBadArgError(value)),
        .bool => {
            const map: std.StaticStringMap(bool) = .initComptime(.{
                .{ "true", true },
                .{ "false", false },
            });

            return map.get(value) orelse return p.fail(context.toBadArgError(value));
        },
        .array => |info| {
            var result: ToParse = undefined;
            var remaining: []const u8 = value;
            var i: usize = 0;

            while (remaining.len != 0) : (i += 1) {
                if (i >= info.len)
                    return p.fail(context.toBadArgError(value));

                const comma_index = std.mem.indexOfScalar(u8, remaining, ',') orelse remaining.len;
                result[i] = try parseValueExplicitType(p, remaining[0..comma_index], context, info.child);
                remaining = remaining[comma_index..];
            }

            return result;
        },
        .pointer => |info| {
            comptime assert(info.size == .slice);

            if (info.child == u8 and info.is_const) {
                if (info.sentinel_ptr != null)
                    @compileError("TODO: allocated values");

                return value;
            } else {
                @compileError("TODO: allocated values");
            }
        },
        else => @compileError("TODO"),
    }
}
