const argz = @import("argz.zig");

pub inline fn name(comptime T: type) [:0]const u8 {
    return @typeName(T);
}

pub inline fn mayHaveLeadingDash(comptime T: type) bool {
    return comptime state: switch (@typeInfo(T)) {
        .optional => |opt| continue :state opt.child,
        .int => |info| info.signedness == .signed,
        .float => true,
        .pointer => |info| {
            if (info.child == u8 and info.is_const)
                break :state true
            else
                continue :state info.child;
        },
        else => false,
    };
}
