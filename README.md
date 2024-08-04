# Disclaimer

The API for `argz` is not yet stable. Additionally, There are still quite a few warts and/or missing features that you would expect
from a typical argument parser. For example, dynamic arguments (i.e. those requiring heap allocation) have limited support, variadic
positional arguments aren't supported, and proper cleanup is not performed when errors are encountered. At the moment, usage should
only be for testing purposes and not for use when writing your CLI program.

I'm already aware of most of the problems that currently exist. You can expect the vast majority of them to get fixed in the coming months.

## argz

`argz` is an (in progress) command line argument parser for Zig that utilizes `comptime`.
