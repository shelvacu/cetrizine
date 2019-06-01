use crate::{
    USER_ID,
    ContextExt,
    CetrizineError,
    PoolArcKey,
    db_types::SmartHax,
    r2d2,
    r2d2_postgres,
};
use serenity::{
    model::prelude::*,
    prelude::*,
    framework::standard::StandardFramework,
};

use std::sync::Arc;
use std::sync::atomic::Ordering;


trait BoolExt {
    fn into_u8(self) -> u8;
}

impl BoolExt for bool {
    #[inline]
    fn into_u8(self) -> u8 {if self {1} else {0}}
}

//https://stackoverflow.com/questions/38406793/why-is-capitalizing-the-first-letter-of-a-string-so-convoluted-in-rust
fn some_kind_of_uppercase_first_letter(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

macro_rules! command_log {
    ($fname:ident($($arg:ident),+) $b:block) => {
        command!($fname($($arg),+) {
            let res:Result<(), CetrizineError> = $b;
            log_any_error!(res);
        });
    };
}

pub fn cetrizine_framework() -> StandardFramework {
    StandardFramework::new()
        .configure(
            |c| c
                .dynamic_prefix(move |context, message| {
                    log_any_error!(inner_dynamic_prefix(context, message)).flatten()
                })
                .allow_whitespace(true)
                .on_mention(true)
                .no_dm_prefix(true)
        )
        .cmd("ping", ping)
        .cmd("setprefix", set_prefix)
        .cmd("set prefix", set_prefix)
        .cmd("set_prefix", set_prefix)
        .cmd("sp", set_prefix)
        .cmd("prefix", get_prefix)
        .cmd("get_prefix", get_prefix)
        .cmd("get prefix", get_prefix)
        .cmd("getprefix", get_prefix)
        .cmd("p", get_prefix)
        .cmd("info", help_info)
        .cmd("help", help_info)
        .cmd("binary", binary)
}

command_log!(binary(context, message, args){
    let res = decode_binary(context, message, args.rest().to_string())?;
    message.reply(&res)?;
    Ok(())
});

fn decode_binary(
    _context: &Context,
    _message: &Message,
    arg: String,
) -> Result<String,CetrizineError> {
    let mut result_text = String::from("\n");
    let mut bits:Vec<bool> = Vec::new();
    let mut weird_char = None;
    for c in arg.chars() {
        match c {
            '0' => bits.push(false),
            '1' => bits.push(true),
            ' '|','|':'|'\t' => (),
            c => weird_char = Some(c),
        }
    }
    let bits = bits;
    if let Some(c) = weird_char {
        result_text.push_str(&format!("WARN: Found character {:?}, sure doesn't look like binary\n", c));
    }
    if bits.len() % 8 != 0 {
        result_text.push_str(&format!("WARN: Number of bits({}) is not a multiple of 8\n", bits.len()));
    }
    let mut bytes:Vec<u8> = Vec::new();
    for chunk in bits.chunks_exact(8usize) {
        let byte:u8 =
            chunk[0].into_u8() << 7
            | chunk[1].into_u8() << 6
            | chunk[2].into_u8() << 5
            | chunk[3].into_u8() << 4
            | chunk[4].into_u8() << 3
            | chunk[5].into_u8() << 2
            | chunk[6].into_u8() << 1
            | chunk[7].into_u8();
        bytes.push(byte);
    }
    let bytes = bytes;
    let conversion_result = String::from_utf8_lossy(&bytes);
    result_text.push_str(&format!("Binary decoded:\n{}", conversion_result));
    Ok(result_text)
}

fn get_guild_prefix(
    pool: &Arc<r2d2::Pool<r2d2_postgres::PostgresConnectionManager>>,
    guild_id: GuildId
) -> Result<Option<String>,CetrizineError> {
    let conn = pool.get()?;
    let rows = conn.query(
        "SELECT command_prefix FROM guild_prefixes WHERE guild_id = $1",
        &[&i64::from(guild_id)]
    )?;
    if rows.len() == 0 {
        return Ok(None);
    } else if rows.len() == 1 {
        let prefix:String = rows.get(0).get(0);
        return Ok(Some(prefix));
    } else { unreachable!() }
}

fn inner_dynamic_prefix(
    context: &Context,
    message: &Message,
) -> Result<Option<String>, CetrizineError> {
    if let Some(guild_id) = message.guild_id {
        // I'd like to put this in a cache, but premature
        // optimization is the root of all evil.
        Ok(get_guild_prefix(&context.get_pool_arc(), guild_id)?)
    } else {
        Ok(None)
    }
}

command_log!(get_prefix(context, message) {
    if let Some(guild_id) = message.guild_id {
        // I'd like to put this in a cache, but premature
        // optimization is the root of all evil.
        match get_guild_prefix(&Arc::clone(context.data.lock().get::<PoolArcKey>().unwrap()), guild_id)? {
            Some(res) => message.reply(&format!("Prefix is currently {:?}", res))?,
            None => message.reply("Prefix is currently unset")?,
        };
    } else {
        message.reply("Prefix can only be set in guilds.")?;
    }
    Ok(())
});

command_log!(help_info(context, message) {
    let reply_text = format!(
        "
{0} {1}
Commands:
{2}ping - Play table tennis
{2}setprefix - Change the prefix that bot will use. (shortcut: sp)
{2}getprefix - Display the current prefix (shortcut: p)
{2}help - Display this message (also: info)

Note: You can always perform commands by pinging this bot at the beginning, eg:
<@{3}> command",
        some_kind_of_uppercase_first_letter(env!("CARGO_PKG_NAME")),
        env!("CARGO_PKG_VERSION"),
        inner_dynamic_prefix(&context, message)?.unwrap_or(String::from("")),
        USER_ID.load(Ordering::Relaxed),
    );
    message.reply(&reply_text)?;
    Ok(())
});

command_log!(ping(_context, message) {
    message.reply("Pong!")?;
    Ok(())
});

command_log!(set_prefix(context, message, args) {
    let guild_id = match message.guild_id {
        None => {
            message.reply("Cannot set prefix in private messages. No prefix needed!")?;
            return Ok(())
        },
        Some(id) => id
    };
    let new_prefix:String = args.rest().into();
    let conn = Arc::clone(context.data.lock().get::<PoolArcKey>().unwrap()).get()?;
    if new_prefix.len() == 0 {
        conn.execute(
            "DELETE FROM guild_prefixes WHERE guild_id = $1",
            &[&i64::from(guild_id)]
        )?;
        message.reply("Prefix unset, use at-mention to execute commands")?;
    } else {
        conn.execute(
            "INSERT INTO guild_prefixes (guild_id, command_prefix) VALUES ($1, $2) ON CONFLICT (guild_id) DO UPDATE SET command_prefix = $2",
            &[
                &SmartHax(guild_id),
                &new_prefix,
            ],
        )?;
        message.reply(&format!("Prefix set to {:?}.",new_prefix))?;
    }
    Ok(())
});
