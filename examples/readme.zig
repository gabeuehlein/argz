const std = @import("std");
const argz = @import("argz");

const config: argz.Config = .{
    // Specifies the flags for the application
    .top_level_flags = &.{
        .help,
        // `void` represents a flag that takes no value.
        // The corresponding field is set to either `true` or `false`
        // depending on whether the flag was found or not.
        //
        // Note: the parameters to `Flag.init` are (1) the flag's type, (2) the flag's short
        // representation (if applicable), (3) the flag's long representation (if applicable),
        // (4) the default value for this flag if it isn't provided (`null` indicates that the
        // flag is mandatory), (5) a brief message describing the flag's usage, and (6) extra
        // data describing various additional properties of the flag that are less common.
        .init(void, 'f', "flag", null, "an example flag", .{}),
        // Optional parameters are supported as well. In this case,
        // a correct usage of this flag would be `-j` or `-j=<u32>`.
        // In the former case, the value corresponding to this value
        // would be `null`.
        .init(?u32, 'j', null, 1, "number of jobs to use", .{
            // This overrides the field name of the flag in the resulting struct.
            // The priority for the field name is detailed below:
            //   1. `.field_name` in the extra data passed to `Flag.init`
            //   2. `flag.long`
            //   3. `flag.short`
            .field_name = "jobs",
        }),
    },
    .mode = .{
        .positionals = &.{
            .init([]const u8, "FILE", "the file to print", .{
                .field_name = "file",
            }),
        },
    },
    .support_allocation = false,
};

pub fn main() !void {
    // This is a wrapper around `std.os.argv`, meaining that it won't work on Windows or WASI.
    // `argz.OwnedArgs` must be used in this case.
    const argv: argz.SystemArgs = .init();
    var p: argz.Parser = try .init(argv.args(), .{
        .program_name = "demo",
        .program_description = "a small demo program",
    });

    const opts = try p.parse(config);

    const jobs: u32 = opts.flags.jobs orelse 18;
    if (jobs == 0)
        p.fatal("jobs must not be zero", .{});

    if (opts.flags.flag) {
        std.debug.print("accelerating computation using the mysteries of the universe...\n", .{});
    }

    if (jobs != 1) {
        std.debug.print("making your computation {d} times faster...\n", .{jobs});
    }
    // make the user happy by making it look like we're doing something
    std.time.sleep(std.time.ns_per_s * 3 / jobs);

    std.debug.print("Printing {s}...\n", .{opts.positionals.file});
    var f = try std.fs.cwd().openFile(opts.positionals.file, .{});
    defer f.close();
    var buf: [4096]u8 = undefined;
    while (true) {
        const n = try f.read(&buf);
        if (n == 0) break;
        std.debug.print("{s}", .{buf[0..n]});
    }

    // No allocations, no cleanup!
}
