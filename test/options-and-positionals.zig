// args: --foo 29 30 --quux -b593 AAAA --zag=42 false

const std = @import("std");
const argz = @import("argz");
const assert = std.debug.assert;
const Parser = argz.Parser;
const Simple = argz.Parser.Interface.Simple;

const MyCli = struct {
    options: struct {
        foo: u32,
        bar: u8 = 9,
        baz: []const u8,
        quux: bool,
        zag: u99 = 12, 
    },

    positionals: struct {
        a: u32,
        b: []const u8,
        c: bool,
    },
};

pub const config = struct {
    pub const options = struct {
        pub const short_mappings = struct {
            pub const baz = 'b';
        };
    };
};

pub fn main() !void {
    var dfl: Simple = try .init(.system());
    var p: argz.Parser = .init(dfl.interface(), .{});
    const opts: MyCli = try p.parse(MyCli, config);

    assert(opts.options.foo == 29);
    assert(opts.options.bar == 9);
    assert(std.mem.eql(u8, "593", opts.options.baz));
    assert(opts.options.quux);
    assert(opts.options.zag == 42);

    assert(opts.positionals.a == 30);
    assert(std.mem.eql(u8, "AAAA", opts.positionals.b));
    assert(!opts.positionals.c);
}
