//! Just a small set of functions to convert `type`s into `[]const u8`
//! with decent clarity and accuracy.

const std = @import("std");
const types = @import("types.zig");
const assert = std.debug.assert;
const builtin = @import("builtin");
const options = @import("options");

pub const Audience = enum {
    /// Produces strings tailored to users. For instance, passing a `u32`
    /// argument to [typeName] will return `"integer"`.
    user,
    /// Produces strings tailored to developers of software using `argz`.
    /// For instance, [typeName] will use `@typeName` to produce a precise type name
    /// to more accurately convey the source type provided to it. "Fancy" formatting
    /// like whitespace padding is still generated where applicable to aid with readability.
    developer,
    /// Performs no work prettifying strings. This is similar to the `.developer`
    /// audience, but with less fancy things. For instance, padding will not be added in certain areas
    /// where it would help humans reading the strings; only one space will be emitted to separate them
    /// regardless of other settings.
    computer,


    pub const default: Audience = if (options.audience_override) |audience|
        audience
    else switch (builtin.mode) {
        .Debug => .developer,
        .ReleaseFast, .ReleaseSafe => .user,
        .ReleaseSmall => .computer,
    };
};

/// Internal function to check that `T` is valid in a particular context
fn validateType(comptime T: type) void {
}

/// Returns an appropriate representation of the name of `T` pertaining to the target
/// `audience`. For instance, `typeName([2]u32, .user)` will return `"integer,integer"`,
/// whereas `typeName([2]u32, .developer)` or `typeName([2]u32, .computer)` will return
/// `@typeName([2]u32)`, which is precisely `"[2]u32"`.
///
/// This is `inline` so a `comptime`-known `audience` will force a single string
/// to be generated instead of a bunch of `switch` conditions.
pub inline fn typeName(comptime T: type, audience: Audience) ?[:0]const u8 {
    return switch (audience) {
        .developer, .computer => @typeName(T),
        .user => switch (@typeInfo(T)) {
            .void => null,
            .bool => "true | false",
            .int => "integer",
            .float => "number",
            .array => |arr| blk: {
                comptime var space: []const u8 = "";
                const child_name = typeName(arr.child, audience);
                inline for (0..arr.len) |i| {
                    space = space ++ child_name;
                    if (i + 1 < space.len)
                        space = space ++ ",";
                }
                break :blk space;
            },
            .pointer => |ptr| blk: {
                comptime assert(ptr.size == .slice);
                if (ptr.child == u8 and ptr.is_const)
                    break :blk "string";
                const child_name = typeName(ptr.child, audience);
                break :blk child_name ++ "[," ++ child_name ++ "...]";
            },
            else => {
                if (types.custom.isCustomType(T)) {
                }
            },
        },
    };
}
