pub const argz_custom_type_data: CustomTypeMetadata = .{
    .ResolveType = struct {
        inline fn func(comptime context: Parser.Context.Tag) ?type {
            return switch (context) {
                .flag => null,
                .positional => Iterator,
            };
        }
    }.func,

    .parseWithContext = struct {
        fn func(comptime context: Parser.Context, parser: *Parser, data_ptr: anytype, comptime depth: u32) anyerror!void {
            comptime assert(context == .positional);
            comptime assert(depth == 0); // only legal in top-level positionals
            comptime assert(@TypeOf(data_ptr) == *Iterator);
            
            if (!parser.lexer.found_force_stop)
                return parser.fail(.{ .too_many_positionals = parser.lexer.argument(true) catch unreachable });

            data_ptr.* = Iterator{
                .args = parser.lexer.args,
                .index = parser.lexer.argi,
            };

            parser.lexer.argi = parser.lexer.args.len;
        }
    }.func, 

    .initWithDefaultValue = struct {
        inline fn func(comptime context: Parser.Context.Tag, data_ptr: anytype) bool {
            comptime assert(context == .positional);

            data_ptr.* = Iterator.empty;
            return true;
        }
    }.func,
};

pub const Iterator = struct {
    args: Args,
    index: usize,

    pub const empty: Iterator = .{
        .args = .empty,
        .index = 0,
    };

    pub fn next(it: *Iterator) ?[]const u8 {
        if (it.index >= it.args.len)
            return null;
        const arg = it.args.get(it.index);
        it.index += 1;
        return arg;
    }

    pub fn init(args: Args, index: usize) @This() {
        return .{ .args = args, .index = index };
    }

    pub fn iterator(self: @This()) Iterator {
        return .{ .args = self.args, .index = self.index };
    }
};

test TrailingPositionals {
    const args = @import("../args.zig");

    const context: Parser.Context = .{ .positional = .{
        .positional_display = "TEST",
        .positional_ty_string = "trailing-positionals",
    } };
    const argv: []const [:0]const u8 = &.{
        "program",
        "99",
        "255",
        "blah",
        "false",
    };
    var arguments: args.OwnedArgs = .init(argv);
    var p: Parser = try .init(arguments.args(), .{});
    p.lexer.found_force_stop = true;
    var space: TrailingPositionals.argz_custom_type_data.ResolveType(.positional).? = undefined;
    try values.parseValueAuto(TrailingPositionals, &space, &p, null, context, 0);
    var i: usize = 1;
    while (space.next()) |string| : (i += 1) {
        try std.testing.expectEqualStrings(string, argv[i]);
    }
}

const Parser = @import("../Parser.zig");
const CustomTypeMetadata = @import("../CustomTypeMetadata.zig");
const TrailingPositionals = @This();
const std = @import("std");
const types = @import("../types.zig");
const assert = std.debug.assert;
const values = @import("../Parser/values.zig");
const Args = @import("../args.zig").Args;
