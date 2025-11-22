//! A mix-and-match implementation of the [Parser] interface. A custom tokenizer may be
//! used, as well as allowing for specialized error reporting using `comptime` .

const std = @import("std");
const argz = @import("../argz.zig");
const Io = std.Io;
const Parser = @import("../Parser.zig");
const Option = argz.Option;
const Positional = argz.Positional;

/// An alternative implementation of [Parser.Interface] that allows for greater
/// specialization of what and how tokens are generated and errors are handled.
///
/// `Tokenizer` is a container that has at least the following functions:
/// ```
/// fn next(Tokenizer) ?Parser.Token;
/// fn argument(Tokenizer, bool) ?[]const u8;
/// fn detach(Tokenizer) error{Unsupported,DetachFailed}!Tokenizer.Detached
/// ```
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

        fn handleError(context: *anyopaque, p: *Parser, err: Parser.Error) Io.Writer.Error!void {
            const me: @This() = @ptrCast(@alignCast(context));
            
            return handle_error(&me.custom_ctx, p, err);
        }
    };
}
