//! A general-purpose error type based on a fixed-size buffer, with support for error notes.
//! Failure to add a full error message or note is ignored and a message that the error or note
//! was truncated is appended to the end.
//!
//! As this is intended to only be used in error conditions, performance is not a primary concern. That
//! said, all functions used in this `struct` use at most O(n) time complexity and O(1) space.

buf: Buffer,
/// Includes all null terminators.
bytes_used: usize,
num_notes: usize,

pub const Buffer = [512]u8;

const truncated_msg = "<truncated>...";

pub fn init(comptime format: []const u8, args: anytype) Error {
    var ret: Error = undefined;
    ret.setFormat(format, args);
    return ret;
}

pub fn setFormat(err: *Error, comptime format: []const u8, args: anytype) void {
    const e = std.fmt.bufPrintZ(&err.buf, format, args) catch {
        const bytes_to_copy =
            if (err.buf.len > truncated_msg.len)
                truncated_msg.len - 1
            else
                err.buf.len - 1;
        @memcpy(err.buf[err.buf.len -| truncated_msg.len..], truncated_msg[0 .. bytes_to_copy + 1]);
        err.buf[err.buf.len - 1] = 0;
        err.bytes_used = err.buf.len;
        err.num_notes = 0;
        return;
    };

    err.bytes_used = e.len + 1;
    err.num_notes = 0;
}

pub fn addNote(err: *Error, comptime format: []const u8, args: anytype) void {
    const remaining = err.remainingBuffer();
    if (remaining.len == 0)
        return;

    const note = std.fmt.bufPrintZ(remaining, format, args) catch {
        const bytes_to_copy =
            if (remaining.len > truncated_msg.len)
                truncated_msg.len
            else
                remaining.len - 1;
        @memcpy(remaining[remaining.len - bytes_to_copy - 1 ..][0..bytes_to_copy], truncated_msg[0..bytes_to_copy]);
        remaining[remaining.len - 1] = 0;
        err.bytes_used = err.buf.len;
        err.num_notes += 1;
        return;
    };

    err.bytes_used += note.len + 1;
    err.num_notes += 1;
}

pub fn remainingBuffer(err: *Error) []u8 {
    return err.buf[err.bytes_used..];
}

pub fn emit(err: *const Error, writer: anytype, color_conf: TtyConfig) void {
    if (@sizeOf(Buffer) == 0)
        return;
    var remaining: []const u8 = &err.buf;
    {
        color_conf.setColor(writer, .bright_red) catch return;
        color_conf.setColor(writer, .bold) catch return;
        writer.writeAll("error:") catch return;
        color_conf.setColor(writer, .reset) catch return;
        writer.writeByte(' ') catch return;
        const index = std.mem.indexOfScalar(u8, remaining, 0).?;
        writer.writeAll(remaining[0..index]) catch return;
        writer.writeByte('\n') catch return;
        remaining = remaining[index + 1 ..];
    }
    for (0..err.num_notes) |_| {
        color_conf.setColor(writer, .blue) catch return;
        color_conf.setColor(writer, .bold) catch return;
        writer.writeAll("note:") catch return;
        color_conf.setColor(writer, .reset) catch return;
        writer.writeByte(' ') catch return;
        const index = std.mem.indexOfScalar(u8, remaining, 0).?;
        writer.writeAll(remaining[0..index]) catch return;
        writer.writeByte('\n') catch return;
        remaining = remaining[index + 1 ..];
    }
}

pub fn main() !void {
    var e = Error.init("this is an error", .{});
    e.addNote("this is a note", .{});
    e.addNote("this is a note with a format argument: {d}", .{50});
    e.addNote("this is an excessively long error note, that will truncate after a bit; " ++ ("A" ** 500), .{});
    e.emit(std.io.getStdErr().writer(), .escape_codes);
}

const Error = @This();
const std = @import("std");
const Allocator = std.mem.Allocator;
const TtyConfig = std.io.tty.Config;
