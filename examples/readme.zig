const std = @import("std");
const argz = @import("argz");

const config: argz.Config = .{
    // Specifies the options for the application
    .top_level_options = &.{
        .help,
        // `void` represents an option that takes no value.
        // The corresponding field is set to either `true` or `false`
        // depending on whether the option was found or not.
        //
        // Note: the parameters to `Option.init` are (1) the option's type, (2) the option's short
        // representation (if applicable), (3) the option's long representation (if applicable),
        // (4) the default value for this option if it isn't provided (`null` indicates that the
        // option is mandatory), (5) a brief message describing the option's usage, and (6) extra
        // data describing various additional properties of the option that are less common.
        .init(void, 'f', "option", null, "an example option", .{}),
        // Optional parameters are supported as well. In this case,
        // a correct usage of this option would be `-j` or `-j=<u32>`.
        // In the former case, the value corresponding to this option
        // would be `null`.
        .init(?u32, 'j', null, 1, "number of jobs to use", .{
            // This overrides the field name of the option in the resulting struct.
            // The priority for the field name is detailed below:
            //   1. `.field_name` in the extra data passed to `Option.init`
            //   2. `option.long`
            //   3. `option.short`
            .field_name = "jobs",
        }),
    },
    .mode = .{
        .positionals = &.{
            .init([]const u8, "FILE", "the file to print", .{
                .field_name = "file",
            }),
            .init(argz.Trailing, "NICE WORDS", "nice words to make the computation go faster", .{
                .field_name = "nice_words",
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

    const jobs: u32 = opts.options.jobs orelse 18;
    if (jobs == 0)
        p.fatal("jobs must not be zero", .{});

    if (opts.options.option) {
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

    var nice_words = opts.positionals.nice_words;
    var it = nice_words.iterator();
    var arg = it.next() orelse return;
    std.debug.print("The program thanks you for saying the following words of encouragement:\n", .{});
    while (it.next()) |next_arg| : (arg = next_arg) {
        std.debug.print("{s}\n", .{arg});
    }
    // print the last argument too
    std.debug.print("{s}\n", .{arg});

    // No allocations, no cleanup!
}
