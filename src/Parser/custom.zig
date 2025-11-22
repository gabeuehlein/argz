//! A mix-and-match implementation of the [Parser] interface. A custom tokenizer may be
//! used, as well as allowing for specialized error reporting using `comptime` .

const std = @import("std");
const argz = @import("../argz.zig");
const Io = std.Io;
const Parser = @import("../Parser.zig");
const Option = argz.Option;
const Positional = argz.Positional;

pub fn TokenizerImpl(
    comptime Context: type,
    next: fn(*Context) ?Parser.Token,
    argument: fn(*Context, bool) ?[]const u8,
) type {
    return struct {
        ctx: Context,


    };
}

/// An alternative implementation of [Parser.Interface] that allows for greater
/// specialization of what and how tokens are generated and errors are handled.
///
/// `tokenizer` must be a `struct` with at least the following fields, and with `T` referring to the concrete
/// type of the tokenizer's state and `U` referring to a type satisfying the implementation
/// of [Parser.Detached]:
/// ```
/// struct {
///     next: fn(*T) ?Parser.Token,
///     argument: fn(*T, bool) ?[]const u8,
///     detach: fn(T) error{NotSupported}!U,
/// }
/// ```
/// `T` and `U` are inferred from the function types; there is no need to expose
/// additional fields to explicitly declare them.
///
/// With certain configurations, this `struct` can mimic the CLI provided by
/// legacy versions of `argz`. Premade functions supporting this are exposed
/// in [args.fmt.help] and [args.fmt.errors].
pub fn Advanced(
    comptime Context: type,
    comptime Tokenizer: type,
    help_fn: fn(*Context, []const argz.Option.Runtime, []const argz.Positional.Runtime, *Io.Writer) anyerror!void,
    handle_error: fn(*Context, Parser.Error, *Io.Writer) Io.Writer.Error!void,
) type {
    return struct {
        custom_ctx: Context,
        tokenizer: Tokenizer,

        /// The VTable for this parser configuration.
        pub const vtable: Parser.Interface.VTable = .{
            .next = next,
            .detach = detach,
            .argument = argument,
            .handle_error = handleError,
            .format_help = formatHelp,
        };

        pub fn interface(p: *@This()) Parser.Interface {
            return .{
                .context = p,
                .vtable = &vtable,
            };
        }

        fn formatHelp(
            context: *anyopaque,
            options: []const Option.Runtime,
            positionals: []const Positional.Runtime,
            writer: *Io.Writer,
        ) Io.Writer.Error!void {
            const me: @This() = @ptrCast(@alignCast(context));

            return help_fn(me.custom_ctx, options, positionals, writer);
        }

        fn next(context: *anyopaque) ?Parser.Token {
            const me: @This() = @ptrCast(@alignCast(context));
            
            return me.tokenizer.next();
        }

        
        fn argument(context: *anyopaque, allow_leading_dash: bool) ?[]const u8 {
            const me: @This() = @ptrCast(@alignCast(context));

            return me.tokenizer.argument(allow_leading_dash);
        }

        fn detach(context: *anyopaque) error{Unsupported}!Parser.Detached {
            const me: @This() = @ptrCast(@alignCast(context));

            return me.tokenizer.detach();
        }

        fn handleError(context: *anyopaque, err: Parser.Error, writer: *Io.Writer) Io.Writer.Error!void {
            const me: @This() = @ptrCast(@alignCast(context));
            
            return handle_error(&me.custom_ctx, err, writer);
        }
    };
}
