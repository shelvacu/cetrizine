use crate::{
    USER_ID,
    DO_RE_EXEC,
    ContextExt,
    CetrizineError,
    PoolArcKey,
    ShardManagerArcKey,
    db_types::{SmartHax, SmartHaxO},
    r2d2,
    diesel::prelude::*,
    ArcPool,
    schema,
};
use serenity::{
    model::prelude::*,
    prelude::*,
    framework::standard::{
        StandardFramework,
        CommandResult,
        Args,
        macros::{command, group},
    }
};

use regex::Regex;
use std::sync::Arc;
use std::sync::atomic::Ordering;
use std::convert::TryInto;


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

fn get_guild_prefix(
    pool: &ArcPool,
    guild_id: GuildId
) -> Result<Option<String>, CetrizineError> {
    use crate::schema::guild_prefixes::dsl;
    let conn = pool.get()?;
    dsl::guild_prefixes
        .filter(dsl::guild_id.eq(SmartHax(guild_id)))
        .select(dsl::command_prefix)
        .get_result(&conn)
        .optional()
        .map_err(From::from)
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

pub fn cetrizine_framework(my_id: UserId) -> StandardFramework {
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
                .by_space(false)
                .on_mention(Some(my_id))
                .no_dm_prefix(true)
                .owners(owners)
        )
        .unrecognised_command(|_ctx, msg, unrecognised_command_name| {
            info!("A user named {:?} tried to execute an unknown command: {}", msg.author.name, unrecognised_command_name);
        })
        .group(&DEFAULT_GROUP)
        .group(&GUILD_ADMIN_GROUP)
        .group(&OWNER_GROUP)
}

group!({
    name: "default",
    options: {},
    commands: [
        pony,
        ping,
        set_prefix,
        get_prefix,
        help_info,
        binary,
        binary_en,
        binary_de,
        horse,
        invite_url,
        test,
        rps_start,
    ],
});

group!({
    name: "guild_admin",
    options: {
        only_in: "guilds",
        required_permissions: [ADMINISTRATOR],
    },
    commands: [
        archive_channel,
        unarchive_channel,
    ],
});

group!({
    name: "owner",
    options: {
        owners_only: true,
    },
    commands: [
        recompile_and_run,
        raw_message,
        restart_bot,
    ],
});

#[command]
fn test(ctx: &mut Context, msg: &Message, mut args: Args) -> CommandResult {
    //let val:u64 = inner_test();
    let mut reply = String::new();
    reply.push_str(&format!("Args are {:?}\n", args));
    reply.push_str(&format!("quoted current {:?}\n", args.quoted().current()));
    reply.push_str(&format!("trimmed current {:?}\n", args.trimmed().current()));
    reply.push_str(&format!("quoted trimmed current {:?}\n", args.trimmed().quoted().current()));

    args.restore();
    reply.push_str(&format!("rest {:?}\n", args.rest()));

    args.restore();
    reply.push_str(&format!("message {:?}\n", args.message()));

    args.restore();
    for arg in args.iter::<String>() {
        reply.push_str(&format!("arg {:?}\n", arg));
    }
    msg.reply(ctx, &reply)?;
    Ok(())
}

trait FromCommandArgs : Sized {
    fn from_command_args(ctx: &Context, msg: &Message, arg: &str) -> Result<Self, &'static str>;
}

impl FromCommandArgs for UserId {
    fn from_command_args(ctx: &Context, msg: &Message, arg: &str) -> Result<Self, &'static str> {
        lazy_static! {
            static ref USER_PING_RE: Regex = Regex::new(r"^\s*<@!?(\d+)>\s*$").unwrap();
        }
        if arg == "." || arg == "self" {
            return Ok(msg.author.id);
        }
        // if arg == "last" || arg == "him" || arg == "her" || arg == "them" {
        //     //TODO: find message before the current one that isn't from author in the same channel, and return that UserId
        // }
        if let Ok(raw_id) = arg.parse():Result<u64,_> {
            return Ok(UserId::from(raw_id));
        }

        if let Some(ma) = USER_PING_RE.captures(arg) {
            if let Ok(raw_id) = ma.get(1).unwrap().as_str().parse():Result<u64,_> {
                return Ok(UserId::from(raw_id));
            }
        }

        if arg.contains('#') {
            let pieces = arg.rsplitn(2,'#').collect():Vec<&str>;
            if let Ok(discriminator) = pieces[0].parse():Result<u16, _> {
                if discriminator <= 9999 {
                    let name = pieces[1];
                    let cache = ctx.cache().read();
                    let maybe_user = cache
                        .users
                        .values()
                        .find(|user_lock| {
                            let user = user_lock.read();
                            user.discriminator == discriminator && user.name.to_ascii_uppercase() == name.to_ascii_uppercase()
                        });
                    if let Some(user_lock) = maybe_user {
                        return Ok(user_lock.read().id);
                    }
                }
            }
        }

        if let Some(guild_lock) = msg.guild(&ctx) {
            let guild = guild_lock.read();
            for member in guild.members.values() {
                if let Some(nick) = member.nick.as_ref() {
                    if nick.to_ascii_uppercase() == arg.to_ascii_uppercase() {
                        return Ok(member.user.read().id);
                    }
                }
                let user = member.user.read();
                if user.name.to_ascii_uppercase() == arg.to_ascii_uppercase() {
                    return Ok(user.id);
                }
            }
        }
        Err("Could not find any User.")
    }
}

#[command]
#[aliases("rpschallenge","rpsstart","rps_challenge","rps_start","rps challenge","rps start","\u{1F5FF}\u{1F4F0}\u{2702}","rps")]
fn rps_start(ctx: &mut Context, msg: &Message, args: Args) -> CommandResult {

    let maybe_receiver_id = UserId::from_command_args(ctx, msg, args.rest());
    match maybe_receiver_id {
        Ok(receiver_id) => {
            crate::rps::start_game(&ctx, msg.author.id, receiver_id, msg.channel_id, msg.id.0)?;
        },
        Err(err_msg) => {
            msg.reply(ctx, err_msg)?;
            return Ok(());
        }
    } 
    Ok(())
}

//#[command]
command_log!(
    #[aliases("unarchivechan","unarchive channel","unarchivechannel")]
//    fn unarchive_channel(ctx: &mut Context, msg: &Message, args: Args) -> CommandResult {
    fn unarchive_channel(ctx, msg, args) {
        use schema::chan_archival::dsl;
        use crate::db_types::Snowflake;
        #[derive(Queryable,Debug)]
        struct ChanArchival {
            rowid: i64,
            from_cat_id: Option<Snowflake>,
            to_cat_id: Option<Snowflake>,
            moved_chan_id: Snowflake,
            dir_is_archiving: bool,
            command_msg_id: Snowflake,
            done: bool,
            failed: bool,
        }
        if args.is_empty() || args.rest() == "." {
            let conn = ctx.get_pool_arc().get()?;
            let curr_chan_cat = msg
                .channel_id
                .to_channel_cached(&ctx)
                .ok_or("Expected channel to be cached".to_string())?
                .guild().ok_or("Expected channel to be a GuildChannel".to_string())?
                .read()
                .category_id;
            // let maybe_archive_channel_id:Option<serenity::model::id::ChannelId> = ctx
            //     .cache()
            //     .read()
            //     .guilds[&msg.guild_id.unwrap()]
            //     .read()
            //     .channels
            //     .values()
            //     .find(|&chan_lock| {
            //         let chan = chan_lock.read();
            //         chan.kind == serenity::model::channel::ChannelType::Category && chan.name.to_ascii_uppercase() == "ARCHIVED"
            //     })
            //     .map(|chan_lock| chan_lock.read().id);
            let maybe_ca:Option<ChanArchival> = dsl::chan_archival.filter(
                (
                    dsl::moved_chan_id.eq(SmartHax(msg.channel_id))
                ).and(
                    diesel::dsl::not(dsl::failed)
                ).and(
                    dsl::dir_is_archiving
                )
            ).order(dsl::command_msg_id.desc()).limit(1).get_result(&conn).optional()?;
            if let Some(ca) = maybe_ca {
                if curr_chan_cat != ca.to_cat_id.map(ChannelId::from) {
                    msg.reply(&ctx, "Channel has moved since being archived, cannot determine where to un-archive to.")?;
                }else{
                    let to_cat = ca.from_cat_id.map(ChannelId::from);
                    let ca_id = pg_insert_helper!(
                        &conn, chan_archival,
                        from_cat_id => SmartHaxO(curr_chan_cat),
                        to_cat_id => SmartHaxO(to_cat),
                        moved_chan_id => SmartHax(msg.channel_id),
                        dir_is_archiving => false,
                        command_msg_id => SmartHax(msg.id),
                        done => false,
                        failed => false,
                    )?;
                    msg.channel_id.edit(&ctx, |ec| ec.category(to_cat)).map_err(|e| {
                        //We don't actually change the error, we just use this to do something only if an error occurs, after the error occurs, before returning.
                        log_any_error!(diesel::update(dsl::chan_archival.filter(dsl::rowid.eq(ca_id))).set(dsl::failed.eq(true)).execute(&conn));
                        e
                    })?;
                    diesel::update(dsl::chan_archival.filter(dsl::rowid.eq(ca_id))).set(dsl::done.eq(true)).execute(&conn)?;
                    msg.reply(&ctx, "Channel un-archived")?;
                }
            } else {
                msg.reply(&ctx, "Cannot find any archivals of this channel.")?;
            }
        } else {
            msg.reply(&ctx, "This command does not (yet) work on anything but the channel the command is sent in.")?;
        }
        Ok(())
    }
);

command_log!(
    #[aliases("archivechannel","archive channel","archivechan")]
    fn archive_channel(ctx, msg, args) {
//    #[command]
//    fn archive_channel(ctx: &mut Context, msg: &Message, args: Args) -> CommandResult {
        use schema::chan_archival::dsl;
        if args.is_empty() || args.rest() == "." {
            let curr_chan_cat = msg
                .channel_id
                .to_channel_cached(&ctx)
                .ok_or("Expected channel to be cached".to_string())?
                .guild().ok_or("Expected channel to be a GuildChannel".to_string())?
                .read()
                .category_id;
            let maybe_archive_channel_id:Option<serenity::model::id::ChannelId> = ctx
                .cache()
                .read()
                .guilds[&msg.guild_id.unwrap()]
                .read()
                .channels
                .values()
                .find(|&chan_lock| {
                    let chan = chan_lock.read();
                    let uppername = chan.name.to_ascii_uppercase();
                    chan.kind == serenity::model::channel::ChannelType::Category && (
                        uppername == "ARCHIVED" || uppername == "ARCHIVE"
                    )
                })
                .map(|chan_lock| chan_lock.read().id);
            if let Some(archive_channel_id) = maybe_archive_channel_id {
                if Some(archive_channel_id) != curr_chan_cat {
                    let conn = ctx.get_pool_arc().get()?;
                    let ca_id = pg_insert_helper!(
                        &conn, chan_archival,
                        from_cat_id => curr_chan_cat.map(SmartHax),
                        to_cat_id => SmartHax(archive_channel_id),
                        moved_chan_id => SmartHax(msg.channel_id),
                        dir_is_archiving => true,
                        command_msg_id => SmartHax(msg.id),
                        done => false,
                        failed => false,
                    )?;
                    msg.channel_id.edit(&ctx, |ec| ec.category(archive_channel_id)).map_err(|e| {
                        //We don't actually change the error, we just use this to do something only if an error occurs, after the error occurs, before returning.
                        log_any_error!(diesel::update(dsl::chan_archival.filter(dsl::rowid.eq(ca_id))).set(dsl::failed.eq(true)).execute(&conn));
                        e
                    })?;
                    diesel::update(dsl::chan_archival.filter(dsl::rowid.eq(ca_id))).set(dsl::done.eq(true)).execute(&conn)?;
                    msg.reply(&ctx, "Channel archived")?;
                }else{
                    msg.reply(&ctx, "Channel is already archived!")?;
                }
            } else {
                msg.reply(&ctx, "Error: Could not find archive channel.")?;
            }
        } else {
            msg.reply(&ctx, "This command does not (yet) work on anything but the channel the command is sent in.")?;
        }
        Ok(())
    }
);

command_log!(
    #[aliases("Ping!", "🏓")]
    fn ping(ctx, message) {
        //let val:i64 = 5u64;
        message.reply(ctx, "Pong!")?;
        Ok(())
    }
);

command_log!(
    #[aliases("inviteurl", "invite url", "invite", "inv")]
    fn invite_url(ctx, message) {
        let user_id = USER_ID.load(Ordering::Relaxed);
        let perms = Permissions::READ_MESSAGES | Permissions::SEND_MESSAGES | Permissions::EMBED_LINKS | Permissions::ATTACH_FILES | Permissions::READ_MESSAGE_HISTORY;
        let url = format!(
            "<https://discordapp.com/api/oauth2/authorize?client_id={}&scope=bot&permissions={}>",
            user_id,
            perms.bits(),
        );
        message.reply(ctx, &url)?;
        Ok(())
    }
);

command_log!(
    #[aliases("raw message","rawmessage")]
    fn raw_message(ctx, message, args) {
        let channel_id:u64 = args.single()?;
        let message_to_send_content = args.rest();

        let to_send_to = ChannelId(channel_id);
        to_send_to.send_message(&ctx, |m| m.content(message_to_send_content))?;
        if to_send_to != message.channel_id {
            message.reply(&ctx, "Message sent")?;
        }
        Ok(())
    }
);

command_log!(
    #[aliases("restart")]
    fn restart_bot(ctx, message) {
        warn!("Restart command called");
        message.reply(&ctx, "About to restart")?;
        {
            let mut do_re_exec = DO_RE_EXEC.lock().unwrap();
            *do_re_exec = Some(message.channel_id.0);
        }
        ctx.data.read().get::<ShardManagerArcKey>().unwrap().lock().shutdown_all();
        Ok(())
    }
);

command_log!(
    #[aliases("r&r","rnr")]
    fn recompile_and_run(ctx, message) {
        warn!("Recompile command called");
        let mut messages_to_show = Vec::new();
        let analyzer_res = coral::Analyzer::with_args(coral::Checker::Build, &["--release"]);
        let analyzer = match analyzer_res {
            Ok(anal) => anal,
            Err(why) => {
                message.reply(&ctx, &format!("Error running cargo: {:?}", why))?;
                return Ok(())
            }
        };
        message.reply(&ctx, "Starting compilation")?;
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

            message.reply(&ctx, &format!("Errors or warnings found, not running:\n\n```{}```", message_reports.join("\n\n")))?;
        } else {
            message.reply(&ctx, "About to restart into new code.")?;
            {
                let mut do_re_exec = DO_RE_EXEC.lock().unwrap();
                *do_re_exec = Some(message.channel_id.0);
            }
            ctx.data.read().get::<ShardManagerArcKey>().unwrap().lock().shutdown_all();
        }
        Ok(())
    }
);

command_log!(
    fn horse(ctx, message){
        message.channel_id.send_files(&ctx, vec!["996592.png"], |m| m)?;
        Ok(())
    }
);

command_log!(
    fn pony(ctx, message){
        message.channel_id.send_files(&ctx, vec!["1955980.png"], |m| m)?;
        Ok(())
    }
);

command_log!(
    #[aliases("binarydecode", "bd")]
    fn binary_de(ctx, message, args){
        message.reply(&ctx, &decode_binary(ctx, message, args.rest().to_string())?)?;
        Ok(())
    }
);

command_log!(
    #[aliases("binaryencode", "be")]
    fn binary_en(ctx, message, args){
        message.reply(&ctx, &encode_binary(ctx, message, args.rest().to_string())?)?;
        Ok(())
    }
);

command_log!(
    #[aliases("b")]
    fn binary(ctx, message, args){
        //let res = decode_binary(context, message, args.rest().to_string())?;
        let res = guess_binary(ctx, message, args)?;
        message.reply(&ctx, &res)?;
        Ok(())
    }
);

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

command_log!(
    #[aliases("prefix", "get prefix", "getprefix", "p")]
    fn get_prefix(ctx, message) {
        if let Some(guild_id) = message.guild_id {
            // I'd like to put this in a cache, but premature
            // optimization is the root of all evil.
            match get_guild_prefix(&Arc::clone(ctx.data.read().get::<PoolArcKey>().unwrap()), guild_id)? {
                Some(res) => message.reply(&ctx, &format!("Prefix is currently {:?}", res))?,
                None => message.reply(&ctx, "Prefix is currently unset")?,
            };
        } else {
            message.reply(&ctx, "No prefix needed for private messages")?;
        }
        Ok(())
    }
);


command_log!(
    #[aliases("help", "info", "\u{2139}", "\u{2139}\u{fe0f}", "?", "h", "-h", "-?", "/?", "/h", "--help")]
    fn help_info(ctx, message) {
        let reply_text = format!(
            "
{0} {1}
Commands:
{2}ping - Play table tennis
{2}setprefix - Change the prefix that bot will use (shortcut: sp)
{2}getprefix - Display the current prefix (shortcut: p)
{2}binary - Convert between binary and text. Will try to guess \"\"intelligently\"\" what direction you're trying to go (shortcut: b)
{2}binaryencode - Convert from text to binary (shortcut: be)
{2}binarydecode - Convert from binary to text (shortcut: bd)
{2}help - Display this message (also: info)
{2}rpschallenge - Challenge someone to a game of rock-paper-scissors (shortcut: rps)
{2}inviteurl - Display the invite url for this bot (shortcut: inv)
{2}archivechannel - Move the current channel into the category named \"Archived\"

Note: You can always perform commands by pinging this bot at the beginning, eg:
<@{3}> command

Source code available at <https://github.com/shelvacu/cetrizine>
App icon based on <https://icons8.com/icon/114217/floppy-disk>
This {0} has Super Pony Powers",
            some_kind_of_uppercase_first_letter(env!("CARGO_PKG_NAME")),
            env!("CARGO_PKG_VERSION"),
            inner_dynamic_prefix(&ctx, message)?.unwrap_or_else(String::new),
            USER_ID.load(Ordering::Relaxed),
        );
        message.channel_id.send_message(&ctx, |m| m.content(&reply_text))?;
        Ok(())
    }
);

command_log!(
    #[aliases("setprefix", "set prefix", "sp")]
    fn set_prefix(ctx, message, args) {
        use crate::schema::guild_prefixes::dsl;
        let guild_id = match message.guild_id {
            None => {
                message.reply(&ctx, "Cannot set prefix in private messages. No prefix needed!")?;
                return Ok(())
            },
            Some(id) => id
        };
        let new_prefix:String = args.rest().into();
        let conn = ctx.get_pool_arc().get()?;
        if new_prefix.is_empty() {
            diesel::delete(
                dsl::guild_prefixes.filter(
                    dsl::guild_id.eq(SmartHax(guild_id))
                )
            ).execute(&conn)?;
            message.reply(&ctx, "Prefix unset, use at-mention to execute commands")?;
        } else {
            diesel::insert_into(
                dsl::guild_prefixes
            ).values((
                dsl::guild_id.eq(SmartHax(guild_id)),
                dsl::command_prefix.eq(&new_prefix),
            )).on_conflict(
                dsl::guild_id
            ).do_update().set(
                dsl::command_prefix.eq(&new_prefix),
            ).execute(&conn)?;
            message.reply(&ctx, &format!("Prefix set to {:?}.",new_prefix))?;
        }
        Ok(())
    }
);
