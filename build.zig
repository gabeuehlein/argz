const std = @import("std");
const argz = @import("src/argz.zig");
const Allocator = std.mem.Allocator;

const Example = enum {
    echo,
    git,
    @"print-env",
    @"math-test",
    random,
    readme,
};

const Audience = enum {
    user,
    developer,
    computer,
};

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    var io_instance: std.Io.Threaded = .init_single_threaded;
    const io = io_instance.io();

    const build_options = b.addOptions();
    const mod = b.addModule("argz", .{
        .root_source_file = b.path("src/argz.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
        },
    });
    mod.addOptions("build_options", build_options);
    build_options.addOption(
        Audience,
        "target_audience",
        b.option(Audience, "target_audience", "manual override for who will be reading messages") orelse switch (optimize) {
            .Debug => .developer,
            .ReleaseFast, .ReleaseSafe => .user,
            .ReleaseSmall => .computer,
        },
    );

    const example = b.option([]const u8, "example", "the example to run");

    const run_example = b.step("run-example", "run an example");
    if (example) |example_str| {
        const e = std.meta.stringToEnum(Example, example_str) orelse @panic("invalid example provided");
        const exe = switch (e) {
            inline else => |ex| blk: {
                const example_mod = b.createModule(.{
                    .root_source_file = b.path("examples/" ++ @tagName(ex) ++ ".zig"),
                    .target = target,
                    .optimize = optimize,
                    .imports = &.{ .{ .name = "argz", .module = mod } },
                });
                break :blk b.addExecutable(.{
                    .name = @tagName(ex) ++ "-demo",
                    .root_module = example_mod,
                });
            },
        };
        const artifact = b.addRunArtifact(exe);
        if (b.args) |args|
            artifact.addArgs(args);
        run_example.dependOn(&artifact.step);
    }

    const test_step = b.step("test", "run tests");
    createTests(b, io, test_step, target, optimize, mod) catch |e| std.debug.panic("running tests failed: {s}", .{@errorName(e)});
}

fn createTests(b: *std.Build, io: std.Io, step: *std.Build.Step, target: std.Build.ResolvedTarget, optimize: std.builtin.OptimizeMode, argz_module: *std.Build.Module) !void {
    const test_dir = b.path("test").getPath(b);
    var dir = try std.fs.cwd().openDir(test_dir, .{ .iterate = true });
    defer dir.close();
    var it = dir.iterate();
    while (try it.next()) |entry| {
        if (entry.kind != .file) continue;
        const file = b.path("test").path(b, entry.name);
        if (std.mem.eql(u8, std.fs.path.extension(entry.name), ".zig")) {
            buildTest(b, io, step, target, optimize, argz_module, file) catch |e| std.debug.panic("test '{s}' failed: {s}", .{ file.getDisplayName(), @errorName(e) });
        }
    }
}

fn buildTest(b: *std.Build, io: std.Io, step: *std.Build.Step, target: std.Build.ResolvedTarget, optimize: std.builtin.OptimizeMode, argz_module: *std.Build.Module, file: std.Build.LazyPath) !void {
    const gpa = b.allocator;

    var arena_allocator: std.heap.ArenaAllocator = .init(gpa);
    defer arena_allocator.deinit();

    var f = try std.fs.cwd().openFile(file.getPath(b), .{});
    defer f.close();

    var buf: [4096]u8 = undefined;
    var f_reader = f.reader(io, &buf);
    const reader = &f_reader.interface;

    var line_buf: std.Io.Writer.Allocating = .init(gpa);
    defer line_buf.deinit();

    var stdout_expected_string: std.ArrayList(u8) = .empty;
    defer stdout_expected_string.deinit(gpa);

    var stderr_expected_string: std.ArrayList(u8) = .empty;
    defer stderr_expected_string.deinit(gpa);

    var expected_exit_code: u8 = 0;

    const test_exe = b.addExecutable(.{
        .name = std.fs.path.basename(file.getDisplayName()),
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
            .root_source_file = file,
        }),
    });
    test_exe.root_module.addImport("argz", argz_module);
    const exe = b.addRunArtifact(test_exe);

    while (true) {
        defer line_buf.clearRetainingCapacity();
        _ = try reader.streamDelimiter(&line_buf.writer, '\n');
        const line = line_buf.written();

        if (std.mem.startsWith(u8, line, "// args:")) {
            const rest = line["// args:".len..];
            try parseArgs(gpa, rest, file, exe);
        } else if (std.mem.startsWith(u8, line, "// expected(stdout):")) {
            const rest = line["// expected(stdout):".len..];
            try tokenizeExpectedString(gpa, rest, file, &stdout_expected_string);
        } else if (std.mem.startsWith(u8, line, "// expected(stderr):")) {
            const rest = line["// expected(stderr):".len..];
            try tokenizeExpectedString(gpa, rest, file, &stderr_expected_string);
        } else if (std.mem.startsWith(u8, line, "// expect-exit-code:")) {
            const rest = line["// expect-exit-code:".len..];
            expected_exit_code = try std.fmt.parseInt(u8, std.mem.trim(u8, rest, &std.ascii.whitespace), 0);
        } else break;
    }
    exe.expectExitCode(expected_exit_code);
    step.dependOn(&exe.step);
    // While it would be nice to test color output, I don't feel like
    // hand-writing all of the ANSI escape sequences.
    exe.setEnvironmentVariable("NO_COLOR", "1");
    exe.expectStdOutEqual(stdout_expected_string.items);
    exe.expectStdErrEqual(stderr_expected_string.items);
}

// Arbitrary escape sequences may be embedded in `string`, e.g. `foo bar\x30 baz` is the same as `foo bar0 baz`. The newline is
// included, unless the next line contains only the string `IGNORE-LAST-NEWLINE`.
fn tokenizeExpectedString(gpa: Allocator, string: []const u8, path: std.Build.LazyPath, array_list: *std.ArrayList(u8)) !void {
    const trimmed = blk: {
        var trimmed_end = std.mem.trimRight(u8, string, &std.ascii.whitespace);
        if (trimmed_end.len != 0 and trimmed_end[0] == ' ') {
            trimmed_end.ptr += 1;
            trimmed_end.len -= 1;
        }
        break :blk trimmed_end;
    };
    if (std.mem.eql(u8, trimmed, "IGNORE-LAST-NEWLINE")) {
        if (array_list.pop()) |chr| {
            if (chr != '\n')
                std.debug.panic("path {s} has IGNORE-LAST-NEWLINE when previous character was not a newline, it is '{c}'", .{ path.getDisplayName(), chr });
        } else std.debug.panic("path {s} contains IGNORE-LAST-NEWLINE as the first expected line", .{path.getDisplayName()});
        return;
    }
    var i: usize = 0;
    while (i < trimmed.len) : (i += 1) {
        if (trimmed[i] == '\\') {
            const char_literal = std.zig.string_literal.parseEscapeSequence(string, &i);
            switch (char_literal) {
                .success => |codepoint| {
                    var buf: [4]u8 = undefined;
                    try array_list.appendSlice(gpa, buf[0..try std.unicode.utf8Encode(codepoint, &buf)]);
                },
                .failure => std.debug.panic("path {s} contains an invalid escape sequence in an expected string", .{path.getDisplayName()}),
            }
        } else {
            try array_list.append(gpa, trimmed[i]);
        }
    }
    try array_list.append(gpa, '\n');
}

fn parseArgs(gpa: std.mem.Allocator, string: []const u8, path: std.Build.LazyPath, run_step: *std.Build.Step.Run) !void {
    const trimmed = std.mem.trim(u8, string, &std.ascii.whitespace);
    var i: usize = 0;
    while (i < trimmed.len) : (i += 1) {
        switch (trimmed[i]) {
            ' ' => continue,
            '"' => {
                const start_idx = i;
                i += 1;
                const end_idx = while (i < trimmed.len) : (i += 1) {
                    if (trimmed[i] == '"' and !(trimmed[i - 1] == '\\' and trimmed[i - 2] == '\\')) {
                        i += 1;
                        break i;
                    }
                } else std.debug.panic("path {s} has invalid arguments", .{path.getDisplayName()});

                const parsed = try std.zig.string_literal.parseAlloc(gpa, trimmed[start_idx..end_idx]);
                defer gpa.free(parsed);
                run_step.addArg(parsed);
            },
            else => {
                const start_idx = i;
                i += 1;
                const end_idx = while (i < trimmed.len) : (i += 1) {
                    if (trimmed[i] == ' ')
                        break i;
                } else trimmed.len;
                run_step.addArg(trimmed[start_idx..end_idx]);
            },
        }
    }
}
