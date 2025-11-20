//! A basic [Parser] implementation that should fit most use cases.
//!
//! Tokenization is done using the [Lexer] `struct`. This handles both
//! short-style (POSIX) and long-style (GNU) named options and positional
//! arguments.

const std = @import("std");
const argz = @import("../argz.zig");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Parser = @import("../Parser.zig");
const Lexer = @import("../Lexer.zig");
const Io = std.Io;

const Option = argz.Option;
const Positional = argz.Positional;

const Default = @This();

pub const Options = struct {
};

const vtable: Parser.Interface.VTable = .{
    .next = vNext,
    .argument = vArgument,
    .handle_error = undefined,
    .format_help = vFormatHelp,
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

fn vFormatHelp(
    context: *anyopaque,
    p: *const Parser,
    options: []const Option.Runtime,
    positionals: []const Positional.Runtime,
    option_descriptions: std.StaticStringMap([:0]const u8),
    writer: *Io.Writer,
) Io.Writer.Error!void {
    const me: *Default = @ptrCast(context);

    const tty_conf = p.stdout_config;

    const prog_name: []const u8 = p.program_name orelse if (me.lexer.args.len != 0)
        me.lexer.args.get(0)
    else
        "[error: zero-length argv]";

    if (p.program_description) |desc|
        try writer.print("{s} - {s}\n", .{ prog_name, desc });

    try writer.writeAll("Usage: ");

    {
        defer tty_conf.setColor(writer, .reset);
        try tty_conf.setColor(writer, .bright_cyan);
        try writer.writeAll(prog_name);
    }

    if (options.len != 0) {
        try writer.writeByte(' ');
        defer tty_conf.setColor(writer, .reset);
        try tty_conf.setColor(writer, .bright_green);
        try writer.writeAll("[OPTIONS]");
    }

    for (positionals) |positional| {
        try writer.writeByte(' ');
        defer tty_conf.setColor(writer, .reset);
        try tty_conf.setColor(writer, .bright_green);
        try writer.writeAll(positional.display);
    }

    try writer.writeByte('\n');
    
    if (options.len != 0)
        try writeOptions(writer, tty_conf, options, option_descriptions);

}

fn writeOptions(
    writer: *Io.Writer,
    tty_conf: Io.tty.Config,
    options: []const Option.Runtime,
    descriptions: std.StaticStringMap([:0]const u8),
) Io.Writer.Error!void {
    assert(options.len != 0);

    const repr_description_padding: usize = blk: {
        var max: usize = 0;

        for (options) |opt| {
            if (descriptions.get(opt.field_name) == null)
                continue;

            var tmp: usize = 0;

            if (opt.long) |l| {
                tmp += std.unicode.utf8CountCodepoints(l) catch unreachable;
                if (opt.short != null)
                    tmp += ", ".len;
            }
            
            if (opt.short) |s|
                tmp += std.unicode.utf8CodepointSequenceLength(s) catch unreachable;

            if (opt.type_name.len != 0) 
                tmp += 1 + "[]".len + std.unicode.utf8CountCodepoints(opt.type_name) catch unreachable;

            max = @max(max, tmp);
        }

        break :blk max;
    };

    {
        defer tty_conf.setColor(writer, .reset);
        try tty_conf.setColor(writer, .bright_green);
        try writer.writeAll("OPTIONS:\n");
    }

    for (options) |opt| {
        var written: usize = 0;

        if (opt.short) |s| {
            defer tty_conf.setColor(writer, .reset);
            try tty_conf.setColor(writer, .bold);
            try tty_conf.setColor(writer, .green);
            try writer.print("-{u}", .{s});
            written += std.unicode.utf8CodepointSequenceLength(s) catch unreachable;
        }

        if (opt.long) |l| {
            if (opt.short != null) {
                try writer.print(", ");
                written += 2;
            }

            defer tty_conf.setColor(writer, .reset);
            try tty_conf.setColor(writer, .bold);
            try tty_conf.setColor(writer, .green);
            try writer.print("--{s}", .{l});
        }

        if (opt.type_name.len != 0) {
            try writer.writeByte(' ');

            {
                defer tty_conf.setColor(writer, .reset);
                try tty_conf.setColor(writer, .bold);
                try tty_conf.setColor(writer, .bright_blue);
                try writer.print("[{s}]", .{opt.type_name});
                written += 2 + std.unicode.utf8CountCodepoints(opt.type_name) catch unreachable;
            }
        }

        if (descriptions.get(opt.field_name)) |desc| {
            try writer.splatByte(' ', repr_description_padding - written);
            try writer.writeAll(desc);
            try writer.writeByte('\n');
        }
    }
}

lexer: Lexer,
found_force_stop: bool = false,
no_force_stop: bool = false,
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

