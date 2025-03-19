## argz

> [!NOTE]
> `argz` is still in early development. Expect bugs and edge cases if you are using this library for your CLI.

`argz` is a Zig library that provides utilities for parsing and handling command line arguments.
It is based on `comptime` generation of a parser for a given configuration, making a quick and easy CLI.

## Features

- Full support for parsing long flags, short flags, and chains of short flags that may optionally
  take an argument.
- Support for positional-based CLIs and command-based CLIs.
- `comptime` generation and verification of a type that represents your CLI.
- Uses Zig's type system to provide an ergonomic API for generating CLI configurations.
- Allocators are optional — you can forego an allocator completely if needed.
- Automatic generation of help/synopsis messages with support for ANSI escape sequences
  for colored output.
- (soon) Automatic generation of manual pages and completions for various shells.

## Examples

There are a number of examples that can be found in the [examples](examples/) subdirectory at the root
of this project. You can run them locally by passing `run-example -Dexample=<example>` to a `zig build`
invocation.

For a quick taste of how CLIs are written using `argz`, see the additional example below:
```zig
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
        // In the former case, the value corresponding to this flag
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
```
