// Emacs just can't handle this syntax, fucks up the tabbing of everything after it so I put it in a separate file.

macro_rules! command_log {
    ($(#[$m:meta])* fn $fname:ident($ctx_arg:ident) $b:block) => {
        command_log!($(#[$m])* fn $fname($ctx_arg, _msg, _args) $b);
    };
    ($(#[$m:meta])* fn $fname:ident($ctx_arg:ident, $msg_arg:ident) $b:block) => {
        command_log!($(#[$m])* fn $fname($ctx_arg, $msg_arg, _args) $b);
    };
    ($(#[$m:meta])* fn $fname:ident($ctx_arg:ident, $msg_arg:ident, $args_arg:ident) $b:block) => {
        
        #[command]
        $(#[$m])*
        fn $fname($ctx_arg: &mut Context, $msg_arg: &Message, #[allow(unused_mut)] mut $args_arg: Args) -> CommandResult {
            let res:Result<(), CetrizineError> = $b;
            log_any_error!(res);
            Ok(())
        }
    };
}
