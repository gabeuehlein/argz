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

    b.getInstallStep().dependOn(&install_docs.step);

    const example = b.step("run-example", "run an example");
    const demo = b.option([]const u8, "example", "the example to run");
    const mod = b.addModule("argz", .{ .root_source_file = b.path("src/root.zig"), .target = target, .optimize = optimize });
    if (demo) |d| {
        if (std.mem.eql(u8, d, "git")) {
            const git_demo = b.addExecutable(.{
                .name = "git-demo",
                .root_source_file = b.path("examples/git.zig"),
                .target = target,
                .optimize = optimize,
            });
            git_demo.root_module.addImport("argz", mod);
            example.dependOn(&git_demo.step);
            const run_example = b.addRunArtifact(git_demo);
            if (b.args) |args| run_example.addArgs(args);
            example.dependOn(&run_example.step);
        }
    }

    const check_lib = b.addStaticLibrary(.{
        .name = "argz",
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const check_step = b.step("check", "perform a more in-depth check");
    check_step.dependOn(&check_lib.step);
}
