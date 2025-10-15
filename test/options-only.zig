// args: --foo 29 --quux -b593  --zag=42

const std = @import("std");
const argz = @import("argz");
const assert = std.debug.assert;
const Parser = argz.Parser;
const Default = argz.Parser.Interface.Default;

const MyCli = struct {
    options: struct {
        foo: u32,
        bar: u8 = 9,
        baz: []const u8,
        quux: bool,
        zag: u99 = 12, 
    },

    pub const config = struct {
        pub const short_mappings = struct {
            pub const baz = 'b';
        };
    };
};

pub fn main() !void {
    var dfl: Default = try .init(argz.args.system());
    var p: argz.Parser = .init(dfl.interface(), .{});
    const opts: MyCli = try p.parse(MyCli);

    assert(opts.options.foo == 29);
    assert(opts.options.bar == 9);
    assert(std.mem.eql(u8, "593", opts.options.baz));
    assert(opts.options.quux);
    assert(opts.options.zag == 42);
}
