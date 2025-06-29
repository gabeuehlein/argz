//! Container for all functions pertaining to the information [Parser] needs to properly
//! handle custom types. For the parser to recognize that a type is a custom one, this data
//! **must** be set in the public declaration (n.b. 'pub const', *not* `field: type`) with
//! the name of `argz_custom_type_data`.

/// The type of the `struct` field (if applicable) of the custom type in the
/// provided `context`. If the custom type should not appear in a particular
/// context, returns `null`.
///
/// Every other function in [CustomTypeMetadata] that takes a `data_ptr: anytype` parameter
/// should have a type of `*ResolveType(context).?` for a given context. This is assumed to
/// be true by [Parser].
Resolve: fn(comptime Parser.Context.Tag) callconv(.@"inline") ?type,

parseWithContext: fn(comptime Parser.Context, *Parser, data_ptr: anytype, arg: []const u8, comptime depth: u32) error{ParseError,OutOfMemory}!void,

defaultTypeName: fn(comptime Parser.Context.Tag, comptime depth: u32) callconv(.@"inline") ?[:0]const u8 = nullDefaultTypeName,

defaultValueString: fn(comptime Parser.Context.Tag) callconv(.@"inline") ?[:0]const u8 = always(@as(?[:0]const u8, null)), 

pub fn always(comptime val: anytype) fn(comptime Parser.Context.Tag) callconv(.@"inline") @TypeOf(val) {
    return struct {
        inline fn func(comptime _: Parser.Context.Tag) @TypeOf(val) {
            return val;
        }
    }.func;
}

inline fn nullDefaultTypeName(comptime _: Parser.Context.Tag, comptime _: u32) ?[:0]const u8 {
    return null;
}

const std = @import("std");
const Parser = @import("Parser.zig");

// For autodoc resolution.
const Counter = @import("types/counter.zig");

comptime {
    std.testing.refAllDeclsRecursive(@This());
}
