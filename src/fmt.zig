const std = @import("std");
const argz = @import("argz.zig");
const types = @import("types.zig");
const values = @import("values.zig");
const Io = std.Io;
const Parser = argz.Parser;
const Error = Parser.Error;

const assert = std.debug.assert;

pub const BasicErrorFormatter = struct {
    parser: *const Parser,
    err: *const Error,

    pub fn format(data: BasicErrorFormatter, writer: *Io.Writer) Io.Writer.Error!void {
        const err, const p = .{ data.err, data.parser };

        if (data.err.* != .raw)
            writeColoredError(p.stderr_config, writer) catch return error.WriteFailed;

        switch (err.*) {
            .unexpected_arg_for_option => |info| {
                const use_long = info.option.long != null;
                if (use_long)
                    assert(info.option.short != null);

                try writer.print("found unexpected argument '{s}' for option '{f}'\n", .{
                    info.arg_string,
                    info.option,
                });
            },
            .expected_arg_for_option => |opt| {
                if (opt.type_name) |name|
                    try writer.print("expected argument of type '{s}' for option '{f}'\n", .{name, opt})
                else
                    try writer.print("missing argument for option '{f}'\n", .{opt});
            },
            .invalid_arg_for_option => |info| {
                try writer.print("invalid argument '{s}' for option '{f}'\n", .{ info.arg_repr, info.option });
            },
            .unknown_long_option => |info| {
                try writer.print("unknown option '--{s}'\n", .{info.found});
            },
            .unknown_short_option => |info| {
                try writer.print("unknown option '-{u}'\n", .{info.found});
            },
            .invalid_positional => |info| {
                try writer.print("invalid argument '{s}' for positional '{s}'\n", .{
                    info.arg_repr,
                    info.positional.display,
                });
            },
            .too_many_positionals => |word| {
                try writer.print("found extra positional '{s}'\n", .{word});
            },
            .missing_positionals => |missing| {
                try writer.print("missing positional '{s}'\n", .{missing[0].display});
            },
            .missing_required_option => |missing| {
                try writer.print("missing required option '{f}'\n", .{missing});
            },
            .duplicate_short_option => |dupe| {
                try writer.print("option '-{u}' found multiple times", .{dupe});
            },
            .duplicate_long_option => |dupe| {
                try writer.print("option '--{s}' found multiple times", .{dupe});
            },
            .custom, .raw => |msg| {
                try writer.writeAll(msg);
                try writer.writeByte('\n');
            },
        }
    }
};

pub const BasicHelpFormatter = struct {
    parser: *const Parser,
    options: []const argz.Option.Runtime,
    positionals: []const argz.Positional.Runtime,

    pub fn format(data: *const BasicHelpFormatter, writer: *Io.Writer) Io.Writer.Error!void {
        const p, const opts, const poss = .{ data.parser, data.options, data.positionals };

        try writer.writeAll(p.program_name);
        if (p.program_description) |desc|
            try writer.print(" - {s}", .{desc});

        try writer.splatByteAll('\n', 2);

        try writer.print("usage: {s}", .{p.program_name});

        if (opts.len != 0)
            try writer.writeAll(" [options]");

        for (poss) |pos|
            try writer.print(" {s}", .{pos.display});

        try writer.writeByte('\n');

        if (opts.len != 0) {
            const opt_description_padding: usize = blk: {
                var max: usize = 0;

                for (opts) |opt| {
                    var current: usize = 0;

                    if (opt.long) |long|
                        current += 2 + (std.unicode.utf8CountCodepoints(long) catch unreachable);

                    if (opt.short != null) {
                        current += 2;
                        if (opt.long != null)
                            current += ", ".len;
                    }

                    if (opt.type_name) |name|
                        current += 3 + (std.unicode.utf8CountCodepoints(name) catch unreachable);

                    max = @max(max, current);
                }

                break :blk max;
            };

            try writer.writeAll("options:\n");

            for (opts) |opt| {
                try writer.splatByteAll(' ', 4);
                var written: usize = 0;

                if (opt.short) |s| {
                    try writer.print("-{u}", .{s});
                    written += 2;

                    if (opt.long != null) {
                        try writer.writeAll(", ");
                        written += 2;
                    }
                }

                if (opt.long) |long| {
                    try writer.print("--{s}", .{long}); 
                    written += 2 + (std.unicode.utf8CountCodepoints(long) catch unreachable);
                }

                if (opt.type_name) |name| {
                    try writer.writeAll(" [");
                    try writer.print("{s}]", .{name});
                    written += 3 + (std.unicode.utf8CountCodepoints(name) catch unreachable);
                }

                if (opt.info) |info| {
                    try writer.splatByteAll(' ', opt_description_padding - written + 1);
                    try writer.writeAll(info);
                }

                if (opt.default_value_repr) |dvr| {
                    try writer.print(" (default {s})", .{dvr});
                }

                try writer.writeByte('\n');
            }
        }

        writer.flush() catch {};
    }
};

fn writeColoredError(tty_config: std.Io.tty.Config, writer: *Io.Writer) (Io.Writer.Error || error{Unexpected})!void {
    try tty_config.setColor(writer, .bold);
    try tty_config.setColor(writer, .red);
    try writer.writeAll("error:");
    try tty_config.setColor(writer, .reset);
    try writer.writeByte(' ');
}

