use crate::{
    USER_ID,
    DO_RE_EXEC,
    ContextExt,
    CetrizineError,
    PoolArcKey,
    ShardManagerArcKey,
    db_types::SmartHax,
    r2d2,
    r2d2_postgres,
};
use serenity::{
    model::prelude::*,
    prelude::*,
    framework::standard::{
        StandardFramework,
        Args,
        //CommandOptions,
    }
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

pub fn cetrizine_framework() -> StandardFramework {
    let owners = vec![
        #[allow(clippy::unreadable_literal)] //This literal isn't meant to be read, it's an ID
        UserId(165858230327574528)
    ].into_iter().collect();
    StandardFramework::new()
        .configure(
            |c| c
                .dynamic_prefix(move |context, message| {
                    log_any_error!(inner_dynamic_prefix(context, message)).flatten()
                })
                .allow_whitespace(true)
                .on_mention(true)
                .no_dm_prefix(true)
                .owners(owners)
        )
        .on_dispatch_error(|_, msg, error| {
            debug!("Dispatch error. msg: {:?}, error: {:?}", msg, error);
        })
        .unrecognised_command(|_, msg, name| {
            debug!("Unrecognized command {:?} {:?}", msg, name);
        })
        .cmd("ping", ping)
        .cmd("Ping!", ping)
        .cmd("🏓", ping) //table tennis paddle
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
        .cmd("\u{2139}", help_info) //information
        .cmd("\u{2139}\u{fe0f}", help_info) //information
        .cmd("binary", binary)
        .cmd("b", binary)
        .cmd("binarydecode", binary_de)
        .cmd("bd", binary_de)
        .cmd("binaryencode", binary_en)
        .cmd("be", binary_en)
        .cmd("pony", pony)
        .cmd("horse", horse)
        .cmd("invite url", invite_url)
        .cmd("invite_url", invite_url)
        .cmd("invite", invite_url)
        .cmd("inv", invite_url)
        .command(
            "r&r",
            |c| c
                .owners_only(true)
                .cmd(recompile_and_run)
        )
        .command(
            "restartshard",
            |c| c
                .owners_only(true)
                .cmd(restart_shard)
        )
        .command(
            "sendraw",
            |c| c
                .owners_only(true)
                .cmd(send_raw)
        )
        .command(
            "restart",
            |c| c
                .owners_only(true)
                .cmd(restart_bot)
        )
}

macro_rules! command_log {
    ($fname:ident($($arg:ident),+) $b:block) => {
        command!($fname($($arg),+) {
            let res:Result<(), CetrizineError> = $b;
            log_any_error!(res);
        });
    };
}

command_log!(invite_url(_ctx, message) {
    let user_id = USER_ID.load(Ordering::Relaxed);
    let perms = Permissions::READ_MESSAGES | Permissions::SEND_MESSAGES | Permissions::EMBED_LINKS | Permissions::ATTACH_FILES | Permissions::READ_MESSAGE_HISTORY;
    let url = format!(
        "<https://discordapp.com/api/oauth2/authorize?client_id={}&scope=bot&permissions={}>",
        user_id,
        perms.bits(),
    );
    message.reply(&url)?;
    Ok(())
});

command_log!(send_raw(_context, message, args) {
    let channel_id:u64 = args.single()?;
    let message_to_send_content = args.rest();

    ChannelId(channel_id).send_message(|m| m.content(message_to_send_content))?;
    message.reply("Message sent")?;
    Ok(())
});

command_log!(restart_shard(context) {
    context.shard.shutdown_clean();
    Ok(())
});

command_log!(restart_bot(context, message) {
    warn!("Restart command called");
    message.reply("About to restart")?;
    {
        let mut do_re_exec = DO_RE_EXEC.lock().unwrap();
        *do_re_exec = Some(message.channel_id.0);
    }
    context.data.lock().get::<ShardManagerArcKey>().unwrap().lock().shutdown_all();
    Ok(())
});

command_log!(recompile_and_run(context, message) {
    warn!("Recompile command called");
    let mut messages_to_show = Vec::new();
    let analyzer_res = coral::Analyzer::with_args(coral::Checker::Build, &["--release"]);
    let analyzer = match analyzer_res {
        Ok(anal) => anal,
        Err(why) => {
            message.reply(&format!("Error running cargo: {:?}", why))?;
            return Ok(())
        }
    };
    message.reply("Starting compilation")?;
    for entry in analyzer {
        if let Some(cargo_msg) = entry.message {
            if cargo_msg.is_warning() || cargo_msg.is_error() {
                messages_to_show.push(cargo_msg);
            }
        }
    }
    if !messages_to_show.is_empty() {
        let message_reports:Vec<String> = messages_to_show
            .into_iter()
            .map(|m| m.report(false, 80usize).unwrap_or_else(String::new))
            .collect();

        message.reply(&format!("Errors or warnings found, not running:\n\n```{}```", message_reports.join("\n\n")))?;
    } else {
        message.reply("About to restart into new code.")?;
        {
            let mut do_re_exec = DO_RE_EXEC.lock().unwrap();
            *do_re_exec = Some(message.channel_id.0);
        }
        context.data.lock().get::<ShardManagerArcKey>().unwrap().lock().shutdown_all();
    }
    Ok(())
});

command_log!(horse(_context, message){
    message.channel_id.send_files(vec!["996592.png"], |m| m)?;
    Ok(())
});

command_log!(pony(_context, message){
    message.channel_id.send_files(vec!["1955980.png"], |m| m)?;
    Ok(())
});

command_log!(binary_de(context, message, args){
    message.reply(&decode_binary(context, message, args.rest().to_string())?)?;
    Ok(())
});   

command_log!(binary_en(context, message, args){
    message.reply(&encode_binary(context, message, args.rest().to_string())?)?;
    Ok(())
});   

command_log!(binary(context, message, args){
    //let res = decode_binary(context, message, args.rest().to_string())?;
    let res = guess_binary(context, message, args)?;
    message.reply(&res)?;
    Ok(())
});

fn max(a:f32,b:f32)->f32{if a<b { b } else { a }}

fn guess_binary(
    context: &Context,
    message: &Message,
    args: Args,
) -> Result<String, CetrizineError> {
    let input = args.rest().to_string();
    let mut binaryish_chars = 0f32;
    let mut _indifferent_chars = 0f32;
    let mut textual_chars = 0f32;
    for c in input.chars() {
        match c {
            '0'|'1' => binaryish_chars += 1f32,
            ' '|'\t'|'\n' => _indifferent_chars += 1f32,
            _ => textual_chars += 1f32,
        }
    }
    let nonindifferent_chars = binaryish_chars + textual_chars;
    //let ratio = binaryish_chars/nonindifferent_chars;
    let encode;
    if nonindifferent_chars < 16f32 && (nonindifferent_chars as u32) % 8 != 0 {
        encode = true;
    } else if ( (binaryish_chars as u32) % 8 == 0 && textual_chars < 2f32 ) ||
        ( nonindifferent_chars < 30f32 && (binaryish_chars/(binaryish_chars+max(textual_chars-2f32,0f32))) > 0.95 ) ||
        ( (binaryish_chars/(binaryish_chars+max(textual_chars-4f32,0f32))) > 0.95 )
    {
        encode = false;
    } else {
        encode = true;
    }
    
    if encode {
        Ok(encode_binary(context, message, input)?)
    } else {
        Ok(decode_binary(context, message, input)?)
    }
}

fn encode_binary(
    _context: &Context,
    _message: &Message,
    arg: String,
) -> Result<String, CetrizineError> {
    let mut bit_string = String::with_capacity(arg.len()*9);
    for byt in arg.as_bytes() {
        for z in 0..=7 {
            let i = 7-z;
            if byt & (1 << i) != 0 {
                bit_string.push('1');
            } else {
                bit_string.push('0');
            }
        }
        bit_string.push(' ');
    }

    Ok(bit_string)
}

fn decode_binary(
    _context: &Context,
    _message: &Message,
    arg: String,
) -> Result<String, CetrizineError> {
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
    info!("bits {:?}", &bits);
    let mut bytes:Vec<u8> = Vec::new();
    for chunk in bits.chunks_exact(8usize) {
        info!("chunk: {:?}", &chunk);
        let byte:u8 =
            chunk[0].into_u8() << 7
            | chunk[1].into_u8() << 6
            | chunk[2].into_u8() << 5
            | chunk[3].into_u8() << 4
            | chunk[4].into_u8() << 3
            | chunk[5].into_u8() << 2
            | chunk[6].into_u8() << 1
            | chunk[7].into_u8();
        info!("byte: {:?}", &byte);
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
    if rows.is_empty() {
        Ok(None)
    } else if rows.len() == 1 {
        let prefix:String = rows.get(0).get(0);
        Ok(Some(prefix))
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
{2}setprefix - Change the prefix that bot will use (shortcut: sp)
{2}getprefix - Display the current prefix (shortcut: p)
{2}binary - Convert between binary and text. Will try to \"guess\" \"\"intelligently\"\" what direction you're trying to go (shortcut: b)
{2}binaryencode - Convert from text to binary (shortcut: be)
{2}binarydecode - Convert from binary to text (shortcut: bd)
{2}help - Display this message (also: info)

Note: You can always perform commands by pinging this bot at the beginning, eg:
<@{3}> command

App icon based on <https://icons8.com/icon/114217/floppy-disk>

This {0} has Super Pony Powers",
        some_kind_of_uppercase_first_letter(env!("CARGO_PKG_NAME")),
        env!("CARGO_PKG_VERSION"),
        inner_dynamic_prefix(&context, message)?.unwrap_or_else(String::new),
        USER_ID.load(Ordering::Relaxed),
    );
    message.channel_id.send_message(|m| m.content(&reply_text))?;
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
    let conn = context.get_pool_arc().get()?;
    if new_prefix.is_empty() {
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