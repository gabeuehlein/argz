# test/

This is the directory where files for `argz`'s test suite go. Each test file is a standalone
Zig file that contains additional data that is preprocessed by a build step in `../build.zig`
that tells the Zig build system what arguments to pass to the test executable and what the
expected output from `stdout`/`stderr` should be (if any).

It is not necessary to modify `../build.zig` to add a test; any `.zig` files in this directory
will be treated as an individual test and will be run automatically when running tests.

# Test Format

**TODO: Describe the format for providing arguments/stating expected stdout/stderr contents**
