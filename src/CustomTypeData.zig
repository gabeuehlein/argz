//! Container for all functions pertaining to the information [Parser] needs to properly
//! handle custom types. For the parser to recognize that a type is a custom one, this data
//! **must** be set in the public declaration (n.b. 'pub const', *not* `field: type`) with
//! the name of `argz_custom_type_data`.

/// The type of the `struct` field (if applicable) of the custom type in the
/// provided `context`. If the custom type should not appear in a particular
/// context, returns `null`.
///
/// Every other function in [CustomTypeData] that takes a `data_ptr: anytype` parameter
/// should have a type of `*ResolveType(context).?` for a given context. This is assumed to
/// be true by [Parser].
ResolveType: fn(comptime Parser.Context.Tag) callconv(.@"inline") ?type,

/// If a default value is to be 
initWithDefaultValue: fn(comptime Parser.Context.Tag, data_ptr: anytype) callconv(.@"inline") bool = noDefaultValue,

parseWithContext: fn(comptime Parser.Context, *Parser, data_ptr: anytype, comptime depth: u32) anyerror!void,
deinitWithContext: fn(comptime Parser.Context, *Parser, data_ptr: anytype) void = noopDeinit, 

requiresAllocator: fn(comptime Parser.Context.Tag) callconv(.@"inline") bool,
repeatable: fn(comptime Parser.Context.Tag) callconv(.@"inline") bool = always(false),
allowsLeadingDash: fn (comptime Parser.Context.Tag) callconv(.@"inline") bool = always(false),

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

inline fn noDefaultValue(comptime _: Parser.Context.Tag, _: anytype) bool {
    return false;
}

fn noopDeinit(comptime _: Parser.Context, _: *Parser,_: anytype) void {}

const std = @import("std");
const Parser = @import("Parser.zig");

// For autodoc resolution.
const Counter = @import("types/counter.zig");

comptime {
    std.testing.refAllDeclsRecursive(@This());
}
