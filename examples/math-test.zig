const std = @import("std");
const argz = @import("argz");

const Operator = enum {
    add,
    sub,
    mul,
    div,
};

const cli = struct {
    pub const flags = [_]argz.Flag{
        .init(i32, "max", .{ .long = "max", .help_msg = "the largest number that a question can have", .required = false }),
        .init(i32, "min", .{ .long = "min", .help_msg = "the smallest number that a question can have", .required = false }),
        .init(u32, "num_questions", .{
            .short = 'n',
            .long = "num_questions",
            .help_msg = "the number of questions",
            .required = false,
        }),
        .init(Operator, "operator", .{
            .short = 'o',
            .long = "operator",
            .help_msg = "add a possible operator to be used in a question",
            .required = false,
            .repeatable = true,
        }),
    };

    pub const positionals = [0]argz.Positional{};

    pub const Context = argz.Parser.ParseContext(&flags, &positionals);
};

pub fn main() !void {
    var max: i32 = 10;
    var min: i32 = -10;
    var num_questions: u32 = 20;
    var operators: std.BoundedArray(Operator, std.meta.fields(Operator).len) = .{ .buffer = undefined, .len = 0 };

    var p = try argz.Parser.init(argz.SystemArgs.init(), .{
        .program_name = "math-test",
        .program_description = "example program offering arithmetic quizzes",
    });
    var context = cli.Context{};
    while (try p.nextArg(&cli.flags, &cli.positionals, &context)) |arg| {
        switch (arg) {
            .flag => |flag| switch (flag) {
                .max => |m| max = m,
                .min => |m| min = m,
                .num_questions => |n| num_questions = n,
                .operator => |o| {
                    if (std.mem.indexOfScalar(Operator, operators.constSlice(), o) != null)
                        p.fatal("cannot add operator '{s}' multiple times", .{@tagName(o)});
                    operators.appendAssumeCapacity(o);
                },
            },
            .positional => unreachable,
        }
    }
    try context.checkRequirements(&p);

    if (operators.len == 0)
        operators = @TypeOf(operators).fromSlice(&.{ .add, .sub, .mul, .div }) catch unreachable;

    if (min > max)
        p.fatal("minimum value ({d}) must be greater than maximum value ({d})", .{ min, max });

    var stdout = std.io.getStdOut();
    const out = stdout.writer();
    var stdin = std.io.getStdIn();
    const in = stdin.reader();

    var xrng = std.Random.DefaultPrng.init(@bitCast(std.time.microTimestamp()));
    const rng = xrng.random();

    var question_no: u32 = 1;
    var question_input: [4096]u8 = undefined;
    while (question_no <= num_questions) : (question_no += 1) {
        const index = rng.intRangeAtMost(usize, 0, operators.len - 1);
        const op = operators.buffer[index];
        const a = rng.intRangeAtMost(i32, min, max);
        const b = rng.intRangeAtMost(i32, min, max);
        try out.print("Question {d}: {d} {s} {d} = ", .{ question_no, a, switch (op) {
            .add => "+",
            .sub => "-",
            .mul => "*",
            .div => "/",
        }, b });
        const answer = (in.readUntilDelimiterOrEof(&question_input, '\n') catch |e| p.fatal("couldn't read answer: {s}\n", .{@errorName(e)})) orelse break;
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
