const std = @import("std");
const argz = @import("argz");

const Operator = enum {
    add,
    sub,
    mul,
    div,
};

const config: argz.Config = .{
    .mode = .{ .positionals = &.{} },
    .top_level_flags = &.{
        .help,
        .init([4]?Operator, null, "operators", .{ .add, .sub, .mul, .div }, "a list of arithmetic operators to make questions with", .{ .alt_type_name = "OPERATOR" }),
        .init(i32, null, "min", -10, "the smallest number that a question can have", .{}),
        .init(i32, null, "max", 10, "the largest number that a question can have", .{}),
        .init(u32, 'n', "num-questions", 20, "the number of questions to ask", .{}),
    },
    .program_name = "math-test",
    .program_description = "a small program that tests your ability to do arithmetic",
    .support_allocation = false,
};

pub fn main() !void {
    var sys_args = argz.SystemArgs.init();
    var p = try argz.argParser(config, sys_args.args(), null);
    const args = try p.parse();

    if (args.flags.min > args.flags.max)
        p.fatal("minimum value must be greater than maximum value", .{});

    const ops = args.flags.operators;
    const null_index = std.mem.indexOfScalar(?Operator, &ops, null) orelse 3;
    if (null_index == 0)
        p.fatal("at least one operator must be provided", .{});

    var stdout = std.io.getStdOut();
    const out = stdout.writer();
    var stdin = std.io.getStdIn();
    const in = stdin.reader();

    var xrng = std.Random.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    const rng = xrng.random();

    var question_no: u32 = 1;
    var question_input: [4096]u8 = undefined;
    while (question_no <= args.flags.@"num-questions") : (question_no += 1) {
        const index = rng.intRangeAtMost(usize, 0, null_index - 1);
        const op = ops[index].?;
        const a = rng.intRangeAtMost(i32, args.flags.min, args.flags.max);
        const b = rng.intRangeAtMost(i32, args.flags.min, args.flags.max);
        try out.print("Question {d}: {d} {s} {d} = ", .{ question_no, a, switch (op) {
            .add => "+",
            .sub => "-",
            .mul => "*",
            .div => "/",
        }, b });
        const answer = in.readUntilDelimiterOrEof(&question_input, '\n') catch |e| p.fatal("couldn't read answer: {s}\n", .{@errorName(e)}) orelse break;
        if (b == 0 and op == .div) {
            if (std.ascii.eqlIgnoreCase(answer, "undefined"))
                try out.writeAll("Correct!\n")
            else
                try out.writeAll("Incorrect. Correct answer: undefined\n");
        } else {
            const correct_answer = switch (op) {
                .add => a +% b,
                .sub => a -% b,
                .mul => a *% b,
                .div => @divTrunc(a, b),
            };
            const answer_int = std.fmt.parseInt(i32, answer, 10) catch {
                try out.print("Incorrect. Correct answer: {d}\n", .{correct_answer});
                continue;
            };
            if (answer_int != correct_answer) {
                try out.print("Incorrect. Correct answer: {d}\n", .{correct_answer});
            } else {
                try out.writeAll("Correct!\n");
            }
        }
    }
}
