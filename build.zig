const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addStaticLibrary(.{
        .name = "argz",
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const install_docs = b.addInstallDirectory(.{
        .source_dir = lib.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "doc/argz",
    });

    const Example = enum { echo, git, help };
    const example = b.option([]const u8, "example", "the example to run");

    const run_example = b.step("run-example", "run an example");
    if (example) |example_str| {
        const mod = b.addModule("argz", .{ .root_source_file = b.path("src/root.zig"), .target = target, .optimize = optimize });
        const e = std.meta.stringToEnum(Example, example_str) orelse @panic("invalid example provided");
        const exe = switch (e) {
            .echo => b.addExecutable(.{ .name = "echo-demo", .root_source_file = b.path("examples/echo.zig"), .target = target, .optimize = optimize }),
            .git => b.addExecutable(.{ .name = "git-demo", .root_source_file = b.path("examples/git.zig"), .target = target, .optimize = optimize }),
            .help => b.addExecutable(.{ .name = "help-demo", .root_source_file = b.path("examples/help.zig"), .target = target, .optimize = optimize }),
        };
        exe.root_module.addImport("argz", mod);
        const artifact = b.addRunArtifact(exe);
        run_example.dependOn(&artifact.step);
    }

    b.getInstallStep().dependOn(&install_docs.step);

    const check_lib = b.addStaticLibrary(.{
        .name = "argz",
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const check_step = b.step("check", "perform a more in-depth check");
    check_step.dependOn(&check_lib.step);
}
