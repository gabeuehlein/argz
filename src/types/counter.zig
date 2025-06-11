pub inline fn Counter(comptime T: type) type {
    if (@typeInfo(T) != .int)
        @compileError("only runtime ints are supported by Counter");
    return struct {
        t: T,
        pub const argz_custom_type_data: CustomTypeData = .{
            .ResolveType = struct {
                inline fn func(comptime context: Parser.Context.Tag) ?type {
                    return switch (context) {
                        .flag => T,
                        .positional => null,
                    };
                }
            }.func,

            .parseWithContext = struct {
                fn func(comptime context: Parser.Context, _: *Parser, data_ptr: anytype, comptime depth: u32) anyerror!void {
                    comptime assert(context == .flag);
                    comptime assert(depth == 0); // only legal in top-level flags
                    comptime assert(@TypeOf(data_ptr) == *T);

                    data_ptr.* +|= 1;
                }
            }.func, 

            .requiresAllocator = CustomTypeData.always(false),
            .repeatable = CustomTypeData.always(true),
            .allowsLeadingDash = struct {
                inline fn func(comptime context: Parser.Context.Tag) bool {
                    comptime assert(context == .flag);
                    return @typeInfo(T).int.signedness == .signed;
                }
            }.func,
        };
    };
}

test Counter {
    const args = @import("../args.zig");
    const values = @import("../Parser/values.zig");

    const context: Parser.Context = .{ .flag = .{
        .flag_string = "--test",
        .flag_ty_string = "counter",
    } };
    {
        inline for (.{ i32, i9, i64, i12, u8, u16, u99 }) |IntTy| {
            const CounterTy = Counter(IntTy);
            const repeats = 30;
            const argv = .{ "program" } ++ [1][:0]const u8{"--test"} ** repeats;
            var arguments: args.OwnedArgs = .init(&argv);
            var p: Parser = try .init(arguments.args(), .{});
            var n: IntTy = 0;
            while (p.lexer.nextToken()) |_| {
                try values.parseValueAuto(CounterTy, &n, &p, null, context, 0);
            }
            try std.testing.expectEqual(n, repeats);
        }
    }

    {
            const CounterTy = Counter(i2);
            const argv = .{ "program" } ++ [1][:0]const u8{"--test"} ** 8;
            var arguments: args.OwnedArgs = .init(&argv);
            var p: Parser = try .init(arguments.args(), .{});
            var n: i2 = 0;
            while (p.lexer.nextToken()) |_| {
                try values.parseValueAuto(CounterTy, &n, &p, null, context, 0);
            }
            try std.testing.expectEqual(n, std.math.maxInt(i2));
    }

    {
        const CounterTy = Counter(u2);
        const argv = .{ "program" } ++ [1][:0]const u8{"--test"} ** 8;
        var arguments: args.OwnedArgs = .init(&argv);
        var p: Parser = try .init(arguments.args(), .{});
        var n: u2 = 0;
        while (p.lexer.nextToken()) |_| {
            try values.parseValueAuto(CounterTy, &n, &p, null, context, 0);
        }
        try std.testing.expectEqual(n, std.math.maxInt(u2));
    }
}

const std = @import("std");
const Parser = @import("../Parser.zig");
const CustomTypeData = @import("../CustomTypeData.zig");
const types = @import("../types.zig");
const assert = std.debug.assert;
