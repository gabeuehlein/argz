const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addStaticLibrary(.{
        .name = "argz",
        .root_source_file = b.path("src/argz.zig"),
        .target = target,
        .optimize = optimize,
    });

    const install_docs = b.addInstallDirectory(.{
        .source_dir = lib.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "doc/argz",
    });

    _ = b.addModule("argz", .{ .root_source_file = b.path("src/argz.zig"), .optimize = optimize, .target = target });

    const Example = enum {
        echo,
        git,
        help,
        @"math-test",
        random,
    };
    const example = b.option([]const u8, "example", "the example to run");

    const run_example = b.step("run-example", "run an example");
    if (example) |example_str| {
        const mod = b.addModule("argz", .{ .root_source_file = b.path("src/argz.zig"), .target = target, .optimize = optimize });
        const e = std.meta.stringToEnum(Example, example_str) orelse @panic("invalid example provided");
        const exe = switch (e) {
            .echo => b.addExecutable(.{ .name = "echo-demo", .root_source_file = b.path("examples/echo.zig"), .target = target, .optimize = optimize }),
            .git => b.addExecutable(.{ .name = "git-demo", .root_source_file = b.path("examples/git.zig"), .target = target, .optimize = optimize }),
            .help => b.addExecutable(.{ .name = "help-demo", .root_source_file = b.path("examples/help.zig"), .target = target, .optimize = optimize }),
            .@"math-test" => b.addExecutable(.{ .name = "math-test-demo", .root_source_file = b.path("examples/math-test.zig"), .target = target, .optimize = optimize }),
            .random => b.addExecutable(.{ .name = "random-demo", .root_source_file = b.path("examples/random.zig"), .target = target, .optimize = optimize }),
        };
        exe.root_module.addImport("argz", mod);
        const artifact = b.addRunArtifact(exe);
        if (b.args) |args| {
            artifact.addArgs(args);
        }
        run_example.dependOn(&artifact.step);
    }

    b.getInstallStep().dependOn(&install_docs.step);
}
