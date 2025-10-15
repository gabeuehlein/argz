const std = @import("std");
const argz = @import("../argz.zig");
const Allocator = std.mem.Allocator;
const Parser = @import("../Parser.zig");
const Lexer = @import("../Lexer.zig");

const Default = @This();

pub const Options = struct {
};

const vtable: Parser.Interface.VTable = .{
    .next = vNext,
    .argument = vArgument,
    .handle_error = undefined,
    .format_help = undefined,

};

fn vNext(context: *anyopaque) ?Parser.Token {
    const me: *Default = @ptrCast(@alignCast(context));
    const token = me.lexer.nextToken(me.found_force_stop, me.allow_empty_word, me.no_force_stop);
    if (token != null and token.? == .force_stop)
        me.found_force_stop = true;
    return token;
}

fn vArgument(context: *anyopaque, allow_leading_dash: bool) ?[]const u8 {
    const me: *Default = @ptrCast(@alignCast(context));
    return me.lexer.argument(allow_leading_dash);
}

lexer: Lexer,
found_force_stop: bool = false,
no_force_stop: bool = false,
allow_empty_word: bool = true,

pub fn init(args: argz.args.Args) error{NoArguments,InvalidUtf8}!Default {
   return .{
       .lexer = try .init(args),
   };
}

pub fn interface(dfl: *Default) Parser.Interface {
    return .{
        .vtable = &vtable,
        .context = dfl,
    };
}
