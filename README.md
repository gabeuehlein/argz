## argz

> [!NOTE]
> While `argz` will generally function for some CLIs, note that
> it is not well tested and as such more esoteric configurations
> may not function properly.

`argz` is a Zig library that provides utilities for parsing command line arguments.
It is based on `comptime` generation of a parser for a given configuration, which means that parsing your arguments using `argz` does not require an allocator.
However parsing dynamically-sized values (e.g. slices) with an allocator *is* supported if required.

## Features

`argz` currently supports the following:
  - Basic flag parsing, including flags that take no arguments, an optional argument, or a mandatory argument
    - Chaining short flags (e.g. `-abc value`) is supported.
    - Flags which may occur multiple times are supported (e.g. `-afoo -abar -abaz`), but are not currently well-tested.
  - Parsing of positional arguments (including variadic positionals at the end of a positional list)
    - Note that optional positionals are not implemented yet, but are a planned feature.
  - Automatic generation of informational help strings based on a provided configuration
  - Automatic emission of error messages for insufficient or invalid arguments
    - Currently, these aren't very descriptive. Fix suggestions and improved error messages are planned, though.
  - Support for commands with their own set of flags and subcommands/positionals
  - ANSI escape code support for emitting colorful output

## Configuration

The configuration for an `argz`-based command line argument parser is a statically-typed `Config` struct. For many CLI applications, most important parts of this `Config` are the `mode` and the `flags`. The mode must be one of the following:
  - `.standard` -- This is the `Mode` that most simple projects should use. A `standard` `Mode` allows you to decribe what kinds of positional arguments your CLI expects users to pass to it.
  - `.commands` -- This `Mode` may be used for advanced projects to group related flags, positionals or subcommands together. Commands are like a dumbed-down `Config` --- they have their own set of flags and their own `mode`, allowing for deep nesting of commands or ending a (potentially nested) command with a set of positionals.
