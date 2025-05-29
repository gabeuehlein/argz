//! Dependency evaluator.

const std = @import("std");
const argz = @import("../argz.zig");
const format = @import("../format.zig");
const values = @import("values.zig");
const Parser = @import("../Parser.zig");

pub fn ensureFlagDependenciesSatisfied(p: *Parser, comptime flags: []const argz.Flag, flag_data: anytype, flags_set: anytype) !void {
    @setEvalBranchQuota(50000000);
    const max_errors = 5;
    var errors_found: usize = 0;
    inline for (flags) |flag| {
        const context: Parser.Context = comptime .{ .flag = .{
            .flag_ty_string = format.typeString(flag.type, .flag),
            .flag_string = if (flag.long) |long| "--" ++ long else flag.flagString(.short)
        } };
        for (flag.dependencies) |dep| {
            evalDependency(p, context, flags, flag_data, flags_set, dep) catch |e| {
                errors_found += 1;
                if (errors_found == max_errors)
                    return e;
            };
        }
    }
    if (errors_found != 0)
        return error.ParseError;
}

fn evalDependency(p: *Parser, comptime context: Parser.Context, comptime target_flag: argz.Flag, comptime target_flag_index: usize, comptime flags: []const argz.Flag, flag_data: anytype, flags_set: anytype, comptime dep: argz.Flag.Dependency) !void {
    const dep_index, const dep_flag_string: usize = comptime switch (dep.flag) {
        .short => |sf| blk: {
            for (flags, 0..) |flag, i| {
                if (target_flag_index == i) continue;
                if (flag.short == sf) {
                    break :blk .{ i, std.fmt.comptimePrint("-{u}", .{sf}) };
                }
            }
            unreachable;
        },
        .long => |lf| blk: {
            for (flags, 0..) |flag, i| {
                if (target_flag_index == i) continue;
                if (std.mem.eql(u8, lf, flag.long orelse continue)) {
                    break :blk .{ i, std.fmt.comptimePrint("--{s}", .{flag.long.?}) };
                }
            }
            unreachable;
        }
    };
    if (!depSatisfied(p, context, dep_index, flags, flag_data, flags_set, dep)) {
        switch (dep.pred) {
            .required_present => return p.fail(.fmt(context, "flag '{s}' requires flag '-{u}' to be set", .{target_flag.flagString(.auto), dep_flag_string})),
            .exclusive => return p.fail(.fmt(context, "flag '{s}' is mutually exclusive with flag '-{u}'", .{target_flag.flagString(.auto), dep_flag_string})),
            .implies => unreachable,
            .requires => return p.fail(.fmt(context, "unsatisfied dependencies (TODO: improve this error mesasge)", .{})),
        }
    }
}

fn depSatisfied(p: *Parser, comptime context: Parser.Context, comptime dep_index: usize, comptime flags: []const argz.Flag, flag_data: anytype, flags_set: anytype, comptime dep: argz.Flag.Dependency) !void {
    switch (dep.pred) {
        .required_present => {
            if (!flags_set.isSet(dep_index))
                return false;
        },
        .exclusive => {
            if (!flags_set.isSet(dep_index))
                return false;
        },
        .implies => |data| {
            const dep_flag = flags[dep_index];
            if (data.override or !flags_set.isSet(dep_index)) {
                values.deinitValueAuto(dep_flag.type, .flag, p, &@field(flag_data, dep_flag.fieldName()));
            }
            @field(flag_data, dep_flag.fieldName()) = @as(*const dep_flag.StructField(), @alignCast(data.value)).*;
        },
        .requires => |data| {
            switch (data) {
                .@"and" => |preds| {
                    inline for (preds) |subpred| {
                        if (!depSatisfied(p, context, flags, flag_data, flags_set, subpred))
                            return false;
                    }
                },
                .@"or" => |preds| blk: {
                    inline for (preds) |subpred| {
                        if (!depSatisfied(p, context, flags, flag_data, flags_set, subpred))
                            break :blk;
                    }
                    return false;
                },
                .eq => |eq| {
                    _ = eq;
                    @compileError("TODO");
                },
            }
        }
    }
    return true;
}
