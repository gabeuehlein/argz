//! A basic [Parser] implementation that should fit most use cases.
//!
//! Tokenization is done using the [Lexer] `struct`. This handles both
//! short-style (POSIX) and long-style (GNU) named options and positional
//! arguments.

const std = @import("std");
const argz = @import("../argz.zig");
const fmt = @import("../fmt.zig");
const Allocator = std.mem.Allocator;
const Parser = @import("../Parser.zig");
const Lexer = @import("../Lexer.zig");
const Writer = std.Io.Writer;

const Default = @This();

const vtable: Parser.Interface.VTable = .{
    .next = next,
    .argument = argument,
    .handle_error = handleError,
    .format_help = formatHelp,
};

fn next(context: *anyopaque) ?Parser.Token {
    const me: *Default = @ptrCast(@alignCast(context));
    return me.lexer.next(me.allow_empty_word);
}

fn argument(context: *anyopaque, allow_leading_dash: bool) ?[]const u8 {
    const me: *Default = @ptrCast(@alignCast(context));
    return me.lexer.argument(allow_leading_dash);
}

fn handleError(
    context: *anyopaque,
    p: *const Parser,
    err: *const Parser.Error,
    writer: *Writer,
) error{HandlingFailed}!void {
    _ = context;

    const formatter: fmt.BasicErrorFormatter = .{
        .parser = p,
        .err = err,
    };

    formatter.format(writer) catch return error.HandlingFailed; 
}

fn formatHelp(
    context: *anyopaque,
    parser: *const Parser,
    options: []const argz.Option.Runtime,
    positionals: []const argz.Positional.Runtime,
    writer: *Writer,
) Writer.Error!void {
    _ = context;

    const formatter: fmt.BasicHelpFormatter = .{
        .parser = parser,
        .options = options,
        .positionals = positionals,
    };

    try formatter.format(writer);
}

lexer: Lexer,
allow_empty_word: bool = true,

pub fn init(args: argz.Args) error{NoArguments,InvalidUtf8}!Default {
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
