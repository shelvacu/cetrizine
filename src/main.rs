#![feature(type_ascription)]
#![deny(unused_must_use)]
#![allow(clippy::mutex_atomic,unused_imports,non_camel_case_types)]
#![recursion_limit="1024"]

#[macro_use]
extern crate lazy_static;

//Crates for debugging/logging
#[macro_use]
extern crate log;
extern crate backtrace;
extern crate simplelog;
extern crate multi_log;

//Main discord library
extern crate serenity;

//Database, database connection pooling
#[macro_use]
extern crate diesel;

//Used by serenity for sharedata
extern crate typemap;

//Gotta keep time
extern crate chrono;
extern crate time;

//This crate allows calling cargo and getting structured output
extern crate coral;

//A map for per-channel mutexes. Somewhat overkill for what I need, but no other crate can have *many* readers at a time.
extern crate evmap;

//For grabbing files stored on discord's servers
extern crate sha2;
extern crate hex;
extern crate reqwest;

extern crate serde;
extern crate serde_json;
extern crate flate2;
extern crate regex;

use sha2::{Sha256,Digest};

pub use diesel::r2d2;

use chrono::prelude::{DateTime,Utc};

use serenity::{
    model::{gateway::Ready, channel::Message, channel::Channel},
    model::prelude::*,
    prelude::*,
    client::bridge::gateway::{
        event::ShardStageUpdateEvent,
        ShardManager,
    },
};

use std::sync::{
    Mutex as StdMutex,
    Arc,
    mpsc::{self, Sender},
    atomic::{
        Ordering,
        AtomicI64,
        AtomicU64,
    },
};
use std::convert::{TryFrom,TryInto};
use std::process;
use std::os::unix::process::CommandExt;
use std::ffi::OsString;
use std::time::Duration;

macro_rules! pg_insert_helper {
    ( $db:expr, $table_name:ident, $( $column_name:ident => $column_value:expr , )+ ) => {{
        use schema::$table_name::dsl;
        crate::diesel::insert_into(dsl::$table_name)
            .values((
                $(
                    dsl::$column_name.eq($column_value),
                )+
            ))
            .returning(dsl::rowid)
            .get_result($db):Result<i64,crate::diesel::result::Error>
    }};
}

macro_rules! log_any_error {
    ( $e:expr ) => {{
        match $e {
            Ok(val) => Some(val),
            Err(e) => {
                warn!("ERROR in {}:{} \"{}\" {:?}",file!(),line!(),stringify!($e),e);
                None
            },
        }
    }};
}

pub mod db_types;
pub mod migrations;
pub mod schema;
pub mod postgres_logger;
#[macro_use]
pub mod command_log_macro;
pub mod commands;
pub mod attachments;
pub mod error;
pub mod rps;

use db_types::*;
use diesel::prelude::*;
use diesel::expression::AsExpression;

#[allow(clippy::unreadable_literal)]
const DISCORD_MAX_SNOWFLAKE:Snowflake = Snowflake(9223372036854775807); // (2^63)-1

static SESSION_ID:AtomicI64 = AtomicI64::new(0);
static USER_ID:AtomicU64 = AtomicU64::new(0);
lazy_static! {
    static ref DO_RE_EXEC:StdMutex<Option<u64>> = StdMutex::new(None);
}
//I really *do* want a bool in a mutex, since I want to send the reboot message exactly once and record that it's been sent, just in case discord sneezes and sends two Readys in quick succession. This *could* be implemented as two statics (I think), an AtomicBool and a Mutex<()>, but the code using this doesn't need to be anywhere near performant so the complexity isn't worth it.
lazy_static! {
    static ref SENT_REBOOT_NOTIF:StdMutex<bool> = StdMutex::new(false);
}

type DBPool = r2d2::Pool<r2d2::ConnectionManager<diesel::pg::PgConnection>>;
type ArcPool = Arc<DBPool>;

struct PoolArcKey;
impl typemap::Key for PoolArcKey {
    type Value = ArcPool;
}

struct IsBotBoolKey;
impl typemap::Key for IsBotBoolKey {
    type Value = bool;
}

struct ShardManagerArcKey;
impl typemap::Key for ShardManagerArcKey {
    type Value = Arc<Mutex<ShardManager>>;
}

struct NewAttachmentNotifSenderKey;
impl typemap::Key for NewAttachmentNotifSenderKey {
    type Value = Mutex<Sender<()>>;
}

trait ContextExt {
    fn get_pool_arc(&self) -> ArcPool;
    fn is_bot(&self) -> bool;
    fn get_attachment_sender(&self) -> Sender<()>;
}

impl ContextExt for Context {
    fn get_pool_arc(&self) -> ArcPool {
        Arc::clone(self.data.read().get::<PoolArcKey>().unwrap())
    }

    fn is_bot(&self) -> bool {
        *self.data.read().get::<IsBotBoolKey>().unwrap()
    }

    fn get_attachment_sender(&self) -> Sender<()> {
        let data_lock = self.data.read();
        let lock = data_lock.get::<NewAttachmentNotifSenderKey>().unwrap().lock();
        (&*lock).clone()
    }
}    

struct Handler{
    beginning_of_time: std::time::Instant,
    session_id: i64,
    archival_queue: StdMutex<Sender<(Channel, String)>>,
}

pub trait EnumIntoString : Sized {
    fn into_str(&self) -> &'static str;
    fn from_str(input: &str) -> Option<Self>;
}

pub trait FilterExt {
    type Return;
    fn filter_null(&self) -> Self::Return;
}

impl FilterExt for str {
    type Return = String;
    fn filter_null(&self) -> Self::Return {
        if self.chars().all(|c| c != '\0' && c != '$') {
            String::from(self)
        } else {
            let mut res = String::with_capacity(self.len() + 4);
            for c in self.chars() {
                match c {
                    _ if c == '\0' => {
                        res.push('$');
                        res.push('0');
                    },
                    _ if c == '$' => {
                        res.push('$');
                        res.push('$');
                    },
                    c => {
                        res.push(c);
                    },
                }
            }
            res
        }
    }
}

impl FilterExt for String {
    type Return = String;
    fn filter_null(&self) -> Self::Return {
        self.as_str().filter_null()
    }
}

impl FilterExt for Option<&str> {
    type Return = Option<String>;
    fn filter_null(&self) -> Self::Return {
        self.map(FilterExt::filter_null)
    }
}

impl FilterExt for Option<String> {
    type Return = Option<String>;
    fn filter_null(&self) -> Self::Return {
        self.clone().map(|s| FilterExt::filter_null(s.as_str()))
    }
}

impl FilterExt for Option<Option<String>> {
    type Return = Option<String>;
    fn filter_null(&self) -> Self::Return {
        self.clone().flatten().filter_null()
    }
}

macro_rules! enum_stringify {
    ( $enum:path => $( $var:ident ),+ ) => {
        impl EnumIntoString for $enum {
            fn into_str(&self) -> &'static str {
                match self {
                    $( <$enum>::$var => stringify!($var), )+
                }
            }
            fn from_str(input: &str) -> Option<Self> {
                match input {
                    $( v if v == stringify!($var) => Some(<$enum>::$var), )+
                    _ => None,
                }
            }
        }
    };
}

enum_stringify!{ serenity::model::channel::MessageType => Regular, GroupRecipientAddition, GroupRecipientRemoval, GroupCallCreation, GroupNameUpdate, GroupIconUpdate, PinsAdd, MemberJoin, NitroBoost, NitroTier1, NitroTier2, NitroTier3 }
enum_stringify!{ serenity::model::guild::DefaultMessageNotificationLevel => All, Mentions }
enum_stringify!{ serenity::model::guild::ExplicitContentFilter => None, WithoutRole, All }
enum_stringify!{ serenity::model::guild::MfaLevel => None, Elevated }
enum_stringify!{ serenity::model::guild::VerificationLevel => None, Low, Medium, High, Higher }
enum_stringify!{ serenity::model::channel::ChannelType => Text, Private, Voice, Group, Category, News, Store }
enum_stringify!{ serenity::model::gateway::ActivityType => Playing, Streaming, Listening }
enum_stringify!{ serenity::model::user::OnlineStatus => DoNotDisturb, Idle, Invisible, Offline, Online }
enum_stringify!{ serenity::gateway::ConnectionStage => Connected, Connecting, Disconnected, Handshake, Identifying, Resuming }
enum_stringify!{ log::Level => Error, Warn, Info, Debug, Trace }

impl EnumIntoString for serenity::WsMessage {
    fn into_str(&self) -> &'static str {
        use serenity::WsMessage::*;
        match self {
            Text(_) => "Text",
            Binary(_) => "Binary",
            Ping(_) => "Ping",
            Pong(_) => "Pong",
            Close(_) => "Close", //change schema, see 7to8.sql
        }
    }
    fn from_str(_input: &str) -> Option<Self> { None }
}

enum OwnedMessageData {
    TextData(String),
    BinaryData(Option<Vec<u8>>),
}

impl OwnedMessageData {
    pub fn into_two(self) -> (Option<String>, Option<Vec<u8>>) {
        use OwnedMessageData::*;
        match self {
            TextData(s) => (Some(s), None),
            BinaryData(od) => (None, od),
        }
    }
}

trait OwnedMessageExt : Sized {
    fn into_data(self) -> OwnedMessageData;
    fn into_two_data(self) -> (Option<String>, Option<Vec<u8>>) {
        self.into_data().into_two()
    }
}

impl OwnedMessageExt for serenity::WsMessage {
    fn into_data(self) -> OwnedMessageData {
        use serenity::WsMessage::*;
        use OwnedMessageData::*;
        match self {
            Text(s) => TextData(s),
            Binary(d) => BinaryData(Some(d)),
            Ping(d) => BinaryData(Some(d)),
            Pong(d) => BinaryData(Some(d)),
            Close(_) => BinaryData(None), //TODO: There is a close reason in the data but whatever
        }
    }
}

fn get_name(chan:&Channel) -> String {
    use serenity::model::channel::Channel::*;
    
    match chan {
        Group(group) => group.read().name.clone().unwrap_or_else(String::new),
        Guild(guild) => guild.read().name.clone(),
        Private(private) => private.read().recipient.read().name.clone(),
        Category(cat) => cat.read().name.clone(),
    }
}

fn get_last_message_id(chan:&Channel) -> Option<MessageId> {
    use serenity::model::channel::Channel::*;

    match chan {
        Group(group) => group.read().last_message_id,
        Guild(guild) => guild.read().last_message_id,
        Private(pri) =>   pri.read().last_message_id,
        Category(_) => None,
    }
}

fn make_synthetic_mag(c: &diesel::pg::PgConnection, session_id_arg: i64, channel_id_arg: i64) -> Result<(), CetrizineError> {
    use schema::message_archive_gets::dsl;
    diesel::insert_into(dsl::message_archive_gets)
        .values((
            dsl::session_id.eq(session_id_arg),
            dsl::channel_id.eq(Snowflake(channel_id_arg)),
            dsl::synthetic.eq(true),
            dsl::finished.eq(false),
        ))
        .on_conflict_do_nothing()
        .execute(c)?;
    Ok(())
}

fn set_after_message_id(c: &diesel::pg::PgConnection, session_id_arg: i64, channel_id_arg: ChannelId, maybe_last_message_id: Option<MessageId>) -> Result<(), CetrizineError> {
    use schema::message_archive_gets::dsl;
    make_synthetic_mag(c, session_id_arg, channel_id_arg.get_snowflake_i64())?;
    if let Some(last_message_id) = maybe_last_message_id {
        diesel::update(
            dsl::message_archive_gets.filter(
                dsl::session_id.eq(session_id_arg)
                .and(dsl::channel_id.eq(Snowflake::from(channel_id_arg)))
                .and(dsl::synthetic.eq(true))
                .and(dsl::after_message_id.is_null())
            )
        ).set(
            dsl::after_message_id.eq(Snowflake::from(last_message_id))
        ).execute(c)?;
    }
    Ok(())
}

pub use error::CetrizineError;

fn text_to_emoji(text: &str) -> &'static str {
    match text {
        "Rock" => "\u{1F5FF}",
        "Paper" => "\u{1F4F0}",
        "Scissors" => "\u{2702}",
        _ => "?"
    }
}

impl Handler {
    fn reaction_add_result(&self, ctx: Context, reaction: Reaction) -> Result<(), CetrizineError> {
        use schema::rps_game::dsl;
        use serenity::model::channel::ReactionType;
        enum Winner {
            Challenger,
            Receiver,
            Draw,
        }
        #[derive(Debug,Queryable)]
        struct RpsGame {
            rowid: i64,
            game_location: Snowflake,
            challenger: Snowflake,
            receiver: Snowflake,
            c_choice: Option<String>,
            r_choice: Option<String>,
        }

        struct FinishedRpsGame {
            rowid: i64,
            game_location: Snowflake,
            challenger: Snowflake,
            receiver: Snowflake,
            c_choice: String,
            r_choice: String,
        }

        impl RpsGame {
            fn into_finished(self) -> Option<FinishedRpsGame> {
                if let (Some(c_choice),Some(r_choice)) = (self.c_choice, self.r_choice) {
                    Some(FinishedRpsGame{
                        rowid: self.rowid,
                        game_location: self.game_location,
                        challenger: self.challenger,
                        receiver: self.receiver,
                        c_choice,
                        r_choice,
                    })
                } else { None }
            }
        }

        let rps_game_select = (
            dsl::rowid,
            dsl::game_location_channel_id,
            dsl::challenger_user_id,
            dsl::receiver_user_id,
            dsl::challenger_choice,
            dsl::receiver_choice,
        );
        if reaction.user_id == ctx.cache().read().user.id {
            return Ok(())
        }
        let conn = ctx.get_pool_arc().get()?;        
        if "\u{267b}".into():ReactionType == reaction.emoji {
            //rematch
            let data:Option<(i64,Snowflake,Snowflake,bool,bool)> = dsl::rps_game.filter(
                dsl::gameover_message_id.eq(SmartHax(reaction.message_id)).and(
                    dsl::challenger_user_id.eq(SmartHax(reaction.user_id)).or(
                        dsl::receiver_user_id.eq(SmartHax(reaction.user_id))
                    )
                )
            ).select((
                dsl::rowid,
                dsl::challenger_user_id,
                dsl::receiver_user_id,
                dsl::challenger_wants_rematch,
                dsl::receiver_wants_rematch,
            )).get_result(&*conn).optional()?;
            if let Some((rowid, c_id, r_id, mut c_rm, mut r_rm)) = data {
                if c_id.0 == reaction.user_id.0 as i64 {
                    diesel::update(dsl::rps_game.filter(dsl::rowid.eq(rowid))).set(
                        dsl::challenger_wants_rematch.eq(true)
                    ).execute(&*conn)?;
                    c_rm = true;
                }
                if r_id.0 == reaction.user_id.0 as i64 {
                    diesel::update(dsl::rps_game.filter(dsl::rowid.eq(rowid))).set(
                        dsl::receiver_wants_rematch.eq(true)
                    ).execute(&*conn)?;
                    r_rm = true;
                }
                if c_rm && r_rm {
                    rps::start_game(
                        &ctx,
                        UserId::from(c_id.0 as u64),
                        UserId::from(r_id.0 as u64),
                        reaction.channel_id,
                        reaction.channel_id.0 ^ reaction.message_id.0 ^ reaction.user_id.0,
                    )?;
                }
            }
            return Ok(())
        }
        let choice = match reaction.emoji {
            ReactionType::Unicode(ref val) if val == "\u{1F5FF}" => "Rock",
            ReactionType::Unicode(ref val) if val == "\u{1F4F0}" => "Paper",
            ReactionType::Unicode(ref val) if val == "\u{2702}" => "Scissors",
            _ => return Ok(()),
        };
        let choices_a:Option<RpsGame> = diesel::update(dsl::rps_game.filter(
            dsl::challenger_private_message_id.eq(SmartHax(reaction.message_id)).and(
                dsl::challenger_choice.is_null()
            )
        )).set(
            dsl::challenger_choice.eq(choice)
        ).returning(rps_game_select).get_result(&*conn).optional()?;
        let choices_b:Option<RpsGame> = diesel::update(dsl::rps_game.filter(
            dsl::receiver_private_message_id.eq(SmartHax(reaction.message_id)).and(
                dsl::receiver_choice.is_null()
            )
        )).set(
            dsl::receiver_choice.eq(choice)
        ).returning(rps_game_select).get_result(&*conn).optional()?;

        let choices = if let Some(Some(game)) = choices_a.map(RpsGame::into_finished) {
            Some(game)
        } else if let Some(Some(game)) = choices_b.map(RpsGame::into_finished) {
            Some(game)
        } else { None };

        if let Some(game) = choices {
            let result = match (game.c_choice.as_str(), game.r_choice.as_str()) {
                ("Rock", "Paper") => Winner::Receiver,
                ("Rock", "Scissors") => Winner::Challenger,
                ("Paper", "Scissors") => Winner::Receiver,
                ("Paper", "Rock") => Winner::Challenger,
                ("Scissors", "Rock") => Winner::Receiver,
                ("Scissors", "Paper") => Winner::Challenger,
                (c,r) if c == r => Winner::Draw,
                _ => unreachable!(),
            };

            let challenger_choice_emoji = text_to_emoji(&game.c_choice);
            let receiver_choice_emoji = text_to_emoji(&game.r_choice);
            let result_text = match result {
                Winner::Draw => "It's a draw!".to_string(),
                Winner::Challenger => format!("{} wins!", UserId::from(game.challenger.0.try_into().unwrap():u64).mention()),
                Winner::Receiver => format!("{} wins!", UserId::from(game.receiver.0.try_into().unwrap():u64).mention()),
            };

            let game_over_message = format!(
                "Rock-paper-scissors game #{} has concluded, here are the results:\n||{} played {}\n{} played {}\n{}||\nBoth players may press \u{267b} for a rematch!",
                game.rowid,
                UserId::from(game.challenger.0.try_into().unwrap():u64).mention(),
                challenger_choice_emoji,
                UserId::from(game.receiver.0.try_into().unwrap():u64).mention(),
                receiver_choice_emoji,
                result_text,
            );
            let msg = ChannelId::from(game.game_location.0.try_into().unwrap():u64).send_message(&ctx, |cm| 
                cm.content(game_over_message)
                    .reactions(vec!["\u{267b}"])
            )?;
            diesel::update(dsl::rps_game.filter(
                dsl::rowid.eq(game.rowid)
            )).set(
                dsl::gameover_message_id.eq(SmartHax(msg.id))
            ).execute(&*conn)?;
        }
        Ok(())
    }
    fn archive_raw_event(&self, ctx: Context, ev: WsEvent) -> Result<(), CetrizineError> {
        use schema::raw_message::dsl;
        let conn = ctx.get_pool_arc().get()?;
        let recvd_duration = ev.happened_at_instant - self.beginning_of_time;
        let msg_type_str = ev.data.into_str();
        let (msg_content_text, msg_content_binary) = ev.data.into_two_data();
        diesel::insert_into(dsl::raw_message)
            .values((
                dsl::recvd_at_datetime.eq(ev.happened_at_chrono),
                dsl::recvd_at_duration_secs.eq(recvd_duration.as_secs().try_into().unwrap():i64),
                dsl::recvd_at_duration_nanos.eq(recvd_duration.subsec_nanos().try_into().unwrap():i32),
                dsl::session_rowid.eq(self.session_id),
                dsl::kind.eq(msg_type_str),
                dsl::content_text.eq(msg_content_text),
                dsl::content_binary.eq(msg_content_binary),
            ))
            .execute(&conn)?;
        trace!(
            "Inserted raw message {}, {}, {}, {}",
            &ev.happened_at_chrono,
            &(recvd_duration.as_secs().try_into().unwrap():i64),
            &(recvd_duration.subsec_nanos().try_into().unwrap():i32),
            &self.session_id,
        );
        Ok(())
    }

    fn guild_create_result(&self, ctx: Context, guild: Guild, is_new: bool) -> Result<(), CetrizineError> {
        use schema::guild_create_event::dsl;
        let channel_archiver_sender = self.archival_queue.lock().unwrap().clone();
        let conn = ctx.get_pool_arc().get()?;
        let ev = ctx.raw_event.clone().unwrap();
        let recvd_duration = ev.happened_at_instant - self.beginning_of_time;
        trace!(
            "About to insert guild create event {}, {}, {}, {}",
            &ev.happened_at_chrono,
            &(recvd_duration.as_secs().try_into().unwrap():i64),
            &(recvd_duration.subsec_nanos().try_into().unwrap():i32),
            &self.session_id,
        );

        let guild_create_event_rowid = diesel::insert_into(dsl::guild_create_event)
            .values((
                dsl::is_new.eq(is_new),
                dsl::recvd_at_datetime.eq(ev.happened_at_chrono),
                dsl::recvd_at_duration_secs.eq(recvd_duration.as_secs().try_into().unwrap():i64),
                dsl::recvd_at_duration_nanos.eq(recvd_duration.subsec_nanos().try_into().unwrap():i32),
                dsl::session_rowid.eq(self.session_id),
            ))
            .returning(dsl::rowid)
            .get_result(&conn)?;

        let session_id_copy = self.session_id;
        std::thread::spawn(move || {
            log_any_error!(Self::archive_guild(
                &*conn,
                &ctx,
                &guild,
                GuildParentId::CreateEvent(guild_create_event_rowid),
                &channel_archiver_sender,
                session_id_copy,
            ));
        });
        Ok(())
    }

    fn ready_result(&self, ctx: Context, ready: Ready) -> Result<(), CetrizineError> {
        use schema::ready::dsl;
        use diesel::sql_types::BigInt;
        #[derive(Debug,QueryableByName)]
        struct StupidStruct {
            #[sql_type = "BigInt"]
            count: i64,
        }
        USER_ID.store(ready.user.id.0, Ordering::Relaxed);
        info!("Ready event received. Connected as: {}", &ready.user.name);
        if let Ok(val) = std::env::var("RE_EXECD") {
            let chan_id:u64 = val.parse().unwrap();
            let mut sent_notif = SENT_REBOOT_NOTIF.lock().unwrap();
            if !*sent_notif {
                ChannelId(chan_id).send_message(&ctx, |m| m.content("Reboot finished"))?;
                *sent_notif = true;
            }
        }
        let conn = ctx.get_pool_arc().get()?;

        let is_bot = ctx.is_bot();
        if is_bot && !ready.user.bot {
            // This is really bad, this means the bot will
            // respond to commands when it's not supposed to.

            error!("Thought we were a bot when we weren't! Bailing immediately!");
            std::process::exit(1);
        } else if !is_bot && ready.user.bot {
            // I really want to use like "uber warn" or something
            // It's not an error because we continue on just fine...
            warn!("The given token appeared to be a user token but discord tells us we're a bot. Commands are currently disabled.");
        }
        if !is_bot && !ready.user.bot {
            ctx.shard.set_status(OnlineStatus::Idle)
        }

        let count_that_should_be_zero:StupidStruct = diesel::sql_query(
            "SELECT COUNT(*) as count FROM ready WHERE ((user_info).inner_user).discord_id != $1;"
        ).bind::<BigInt,_>(i64::from(ready.user.id)).get_result(&conn)?;
        let count_that_should_be_zero = count_that_should_be_zero.count;

        if count_that_should_be_zero != 0 {
            panic!("Error: you should always log in with the same user for the same database! Found ready with differing user id (current user is id {} aka {}#{})",ready.user.id.0,ready.user.name,ready.user.discriminator);
        }

        let channel_archiver_sender = self.archival_queue.lock().unwrap().clone();
        let session_id_copy = self.session_id;
        std::thread::spawn(move || {
            log_any_error!(
                Self::grab_full_archive(
                    &*conn,
                    ctx,
                    ready,
                    channel_archiver_sender,
                    session_id_copy,
                )
            );
        });
        Ok(())
    }
            
    fn archive_message(conn: &diesel::pg::PgConnection, msg: &Message, recvd_at:DateTime<Utc>) -> Result<(), CetrizineError> {
        conn.transaction(|| {
            let message_rowid:i64 = pg_insert_helper!(
                conn, message,
                discord_id => SmartHax(msg.id),
                author => DbDiscordUser::from(msg.author.clone()),
                channel_id => SmartHax(msg.channel_id),
                content => msg.content.filter_null(),
                edited_timestamp => msg.edited_timestamp,
                guild_id => SmartHaxO(msg.guild_id),
                kind => msg.kind.into_str(),
                member => msg.member.clone().map(DbPartialMember::from),
                mention_everyone => msg.mention_everyone,
                mention_roles => msg.mention_roles.clone().into_iter().map(SmartHax).collect():Vec<_>,
                mentions => msg.mentions.clone().into_iter().map(DbDiscordUser::from).collect():Vec<_>,
                nonce_debug => format!("{:?}",msg.nonce), //TODO: should probably file a bug and/or pull request with serenity about this one
                pinned => msg.pinned,
                timestamp => msg.timestamp,
                tts => msg.tts,
                webhook_id => SmartHaxO(msg.webhook_id),
                archive_recvd_at => recvd_at,
            )?;

            //attachments
            for attachment in &msg.attachments {
                pg_insert_helper!(
                    conn, attachment,
                    message_rowid => message_rowid,
                    discord_id => Snowflake::from(attachment.id),
                    filename => attachment.filename.filter_null(),
                    height => attachment.height.map(|h| h.try_into().unwrap():i64),
                    width  => attachment.width .map(|w| w.try_into().unwrap():i64),
                    proxy_url => attachment.proxy_url.filter_null(),
                    size => attachment.size.try_into().unwrap():i64,
                    url => attachment.url.filter_null(),
                )?;
            }

            //embeds
            for embed in &msg.embeds {
                let embed_rowid:i64 = pg_insert_helper!(
                    conn, embed,
                    message_rowid => message_rowid,
                    author => embed.author.clone().map(DbEmbedAuthor::from),
                    colour_u32 => DiscordColour::from(embed.colour),
                    description => embed.description.filter_null(),
                    footer => embed.footer.clone().map(DbEmbedFooter::from),
                    image => embed.image.clone().map(DbEmbedImage::from),
                    kind => embed.kind.filter_null(),
                    provider => embed.provider.clone().map(DbEmbedProvider::from),
                    thumbnail => embed.thumbnail.clone().map(DbEmbedImage::from),
                    timestamp => embed.timestamp.filter_null(),
                    title => embed.title.filter_null(),
                    url => embed.url.filter_null(),
                    video => embed.video.clone().map(DbEmbedVideo::from),
                )?;

                //insert embed fields
                for embed_field in &embed.fields {
                    pg_insert_helper!(
                        conn, embed_field,
                        embed_rowid => embed_rowid,
                        inline => embed_field.inline,
                        name   => embed_field.name.filter_null(),
                        value  => embed_field.value.filter_null(),
                    )?;
                }
            }

            //reactions
            for reaction in &msg.reactions {
                let (is_custom, animated, id, name, string) = match reaction.reaction_type.clone() {
                    serenity::model::channel::ReactionType::Custom{animated, id, name} =>
                        (true, Some(animated), Some(id), name, None),
                    serenity::model::channel::ReactionType::Unicode(s) =>
                        (false, None, None, None, Some(s)),
                };
                
                pg_insert_helper!(
                    conn, reaction,
                    message_rowid => message_rowid,
                    count => reaction.count.try_into().unwrap():i64,
                    me => reaction.me,
                    reaction_is_custom => is_custom,
                    
                    reaction_animated => animated,
                    reaction_id => SmartHaxO(id),
                    reaction_name => name.filter_null(),
                    
                    reaction_string => string.filter_null(),
                )?;
            }
            Ok(())
        })
    }

    //guild_name is used purely for the pretty output and debug messages
    fn grab_channel_archive(
        http: impl AsRef<serenity::http::raw::Http>,
        conn: &diesel::pg::PgConnection,
        chan: &Channel,
        guild_name: String
    ) -> Result<(), CetrizineError> {
        use schema::message_archive_gets::dsl;
        #[derive(Debug,Queryable)]
        struct MagData {
            //#[sql_type = "BigInt"]
            rowid: i64,
            //#[sql_type = "BigInt"]
            emi: Option<Snowflake>,
            //#[sql_type = "BigInt"]
            ami: Option<Snowflake>,
            //#[sql_type = "Bool"]
            r_gt_r: bool,
        }

        let name = get_name(chan);
        info!("ARCHIVING CHAN {}#{} (id {})", &guild_name, &name, &chan.id());
        let mut got_messages = false;

        let last_msg_id = match get_last_message_id(chan) {
            Some(id) => id,
            None => return Ok(()),
        };

        trace!("DEBUG: selecting maybe_res WHERE start >= {} >= end AND chan = {}",last_msg_id,chan.id());

        let last = last_msg_id.get_snowflake_i64();
        let mut maybe_mag_data:Option<MagData> = dsl::message_archive_gets.filter(
            (
                (
                    (
                        dsl::start_message_id.ge(Snowflake(last))
                    ).and(
                        dsl::end_message_id.le(Snowflake(last))
                    )
                ).or(dsl::around_message_id.eq(Snowflake(last)))
            ).and(
                dsl::channel_id.eq(SmartHax(chan.id()))
            ).and(
                dsl::finished.eq(true)
            )
        ).order(dsl::end_message_id.asc()).limit(1).select((
            dsl::rowid,
            dsl::end_message_id,
            dsl::after_message_id,
            (
                dsl::message_count_received.is_not_null()
            ).and(
                dsl::message_count_requested.is_not_null()
            ).and(
                dsl::message_count_received.lt(dsl::message_count_requested)
            ),
        )).get_result(conn).optional()?;                      
        trace!("maybe_mag_data is {:?}",maybe_mag_data);

        let mut get_before = DISCORD_MAX_SNOWFLAKE;
        
        while let Some(mag_data) = maybe_mag_data {
            //the query includes `WHERE ... finished = true`, so because of constraint message_archive_gets_message_ids_check this must always be non-null in the database, so not None here
            get_before = mag_data.emi.unwrap();
            //after_message_id = mag_data.ami;
            if mag_data.r_gt_r {
                return Ok(());
            }
            trace!("selecting maybe_res again get_before {:?} after_message_id {:?} mag_data.rowid {}",get_before,mag_data.ami,mag_data.rowid);

            let last = get_before.try_into().unwrap():i64;
            maybe_mag_data = dsl::message_archive_gets.filter(
                (
                    (
                        (
                            dsl::start_message_id.ge(Snowflake(last))
                        ).and(
                            dsl::end_message_id.lt(Snowflake(last))
                        )
                    ).or(
                        dsl::before_message_id.eq(Snowflake(last))
                    ).or(
                        (
                            dsl::start_message_id.eq(mag_data.ami).is_not_null()
                        ).and(
                            dsl::start_message_id.eq(mag_data.ami)
                        )
                    )
                ).and(
                    dsl::channel_id.eq(SmartHax(chan.id()))
                ).and(
                    dsl::rowid.ne(mag_data.rowid)
                ).and(
                    dsl::finished.eq(true)
                )
            ).order(dsl::start_message_id.asc()).limit(1).select((
                dsl::rowid,
                dsl::end_message_id,
                dsl::after_message_id,
                (
                    dsl::message_count_received.is_not_null()
                ).and(
                    dsl::message_count_requested.is_not_null()
                ).and(
                    dsl::message_count_received.lt(dsl::message_count_requested)
                ),
            )).get_result(conn).optional()?;
            trace!("maybe_mag_data again is {:?}",maybe_mag_data);
        }

        // We're "in" a gap, lets find where this gap ends.

        trace!("selecting gap_end where end < {:?} and chan = {}",get_before,chan.id());
        let gap_end:Snowflake = dsl::message_archive_gets.filter(
            (
                dsl::end_message_id.lt(get_before)
            ).and(
                dsl::channel_id.eq(SmartHax(chan.id()))
            ).and(
                dsl::finished.eq(true)
            )
        ).order(dsl::start_message_id.desc()).limit(1).select(dsl::start_message_id)
            .get_result(conn).optional()?.flatten().unwrap_or(Snowflake(0));

        trace!("gap_end is {:?}",gap_end);
        let mut earliest_message_recvd = get_before;

        while earliest_message_recvd > gap_end {
            let asking_for = 100u8;
            
            let mut msgs;
            let around;
            if earliest_message_recvd == DISCORD_MAX_SNOWFLAKE {
                trace!("asking for around {:?}", last_msg_id);
                around = true;
                msgs = chan.id().messages(&http, |r| r.limit(asking_for.into()).around(last_msg_id))?;
            }else{
                trace!("asking for before {:?}",earliest_message_recvd);
                around = false;
                msgs = chan.id().messages(&http, |r| r.limit(asking_for.into()).before(earliest_message_recvd))?;
            }
            let recvd_at = Utc::now();
            
            // Messages seem to usually be ordered, largest ids first,
            // However, that's documented literally nowhere, so lets sort them.
            msgs.sort_unstable_by_key(|msg| {
                let msg_id:u64 = msg.id.into();
                std::u64::MAX - msg_id
            });
            let msgs = msgs;

            let mut should_break = false;
            conn.transaction(|| {
                for msg in &msgs {
                    trace!("Archiving Message id {:?}", msg.id);
                    Self::archive_message(conn, msg, recvd_at)?;
                }
                if msgs.is_empty() {
                    should_break = true;
                    return Ok(());
                }
                let first_msg = msgs.first().expect("im a bad");
                let last_msg  = msgs.last().expect("im a bad");

                pg_insert_helper!(
                    conn, message_archive_gets,
                    channel_id => SmartHax(chan.id()),
                    around_message_id => if around { Some(SmartHax(last_msg_id)) } else { None },
                    before_message_id => if around { None } else { Some(earliest_message_recvd) },
                    start_message_id => SmartHax(first_msg.id),
                    end_message_id => SmartHax(last_msg.id),
                    message_count_requested => i64::from(asking_for),
                    message_count_received => i64::try_from(msgs.len()).expect("That's a lot of messages"),
                )?;
                got_messages = true;
                info!(
                    "Archived {} message(s), {} thru {} in {}#{}",
                    msgs.len(),
                    first_msg.id,
                    last_msg.id,
                    guild_name,
                    name
                );
                earliest_message_recvd = last_msg.id.into();
                Ok(()):Result<(), CetrizineError>
            })?;
            if should_break { break; }
        } //while earliest_message_recvd > gap_end
        
        if got_messages {
            trace!("recursing!");
            Self::grab_channel_archive(http, conn, chan, guild_name)
        } else {
            Ok(())
        }
    }

    fn archive_guild(
        conn: &diesel::pg::PgConnection,
        ctx: &Context,
        guild: &Guild,
        parent_rowid: GuildParentId,
        channel_archiver_sender: &Sender<(Channel, String)>,
        session_id: i64,
    ) -> Result<(), CetrizineError> {
        info!("ARCHIVING GUILD {}", &guild.name);
        let raw_event = ctx.raw_event.as_ref().unwrap();
        //let tx = conn.transaction()?;

        let mut guild_channels = Vec::<(ChannelId,i64)>::new();
        conn.transaction(|| {

            let guild_rowid = pg_insert_helper!(
                conn, guild,
                ready_rowid => parent_rowid.as_ready(),
                guild_create_event_rowid => parent_rowid.as_create_event(),
                discord_id => SmartHax(guild.id), 
                afk_channel_id => SmartHaxO(guild.afk_channel_id), 
                afk_timeout => guild.afk_timeout.try_into().unwrap():i64, 
                application_id => SmartHaxO(guild.application_id), 
                default_message_notification_level => guild.default_message_notifications.into_str(),
                explicit_content_filter => guild.explicit_content_filter.into_str(),
                features => guild.features.clone().into_iter().map(|s| s.filter_null()).collect():Vec<_>,
                icon => guild.icon.filter_null(),
                joined_at => guild.joined_at,
                large => guild.large,
                member_count => guild.member_count.try_into().unwrap():i64,
                mfa_level => guild.mfa_level.into_str(),
                name => guild.name.filter_null(),
                owner_id => SmartHax(guild.owner_id),
                region => guild.region.filter_null(),
                splash => guild.splash.filter_null(),
                system_channel_id => SmartHaxO(guild.system_channel_id),
                verification_level => guild.verification_level.into_str(),
                archive_recvd_at => raw_event.happened_at_chrono,
            )?;


            //channels
            for chan_a_lock in guild.channels.values() {
                let chan = chan_a_lock.read();

                let chan_rowid = pg_insert_helper!(
                    conn, guild_channel,
                    discord_id => SmartHax(chan.id),
                    guild_rowid => guild_rowid,
                    guild_id => SmartHax(chan.guild_id),
                    bitrate => chan.bitrate.map(|b| b.try_into().unwrap():i64),
                    category_id => SmartHaxO(chan.category_id),
                    kind => chan.kind.into_str(),
                    last_message_id => SmartHaxO(chan.last_message_id),
                    last_pin_timestamp => chan.last_pin_timestamp,
                    name => chan.name.filter_null(),
                    position => chan.position,
                    topic => chan.topic.filter_null(),
                    user_limit => chan.user_limit.map(|u| u.try_into().unwrap():i64),
                    nsfw => chan.nsfw,
                )?;

                set_after_message_id(&conn, session_id, chan.id, chan.last_message_id)?;
                guild_channels.push((chan.id, chan_rowid));

                for overwrite in &chan.permission_overwrites {
                    use serenity::model::channel::PermissionOverwriteType::*;
                    let (ov_type_str, ov_id) = match overwrite.kind {
                        Member(uid) => ("Member", uid.into():Snowflake),
                        Role(rid) => ("Role", rid.into():Snowflake),
                    };

                    pg_insert_helper!(
                        conn, permission_overwrite,
                        guild_channel_rowid => chan_rowid,
                        allow_bits => PermsToSql(overwrite.allow),
                        deny_bits => PermsToSql(overwrite.deny),
                        permission_overwrite_type => ov_type_str,
                        permission_overwrite_id => ov_id,
                    )?;
                }
            }

            //emojis
            for emoji in guild.emojis.values() {
                pg_insert_helper!(
                    conn, emoji,
                    discord_id => SmartHax(emoji.id),
                    guild_rowid => guild_rowid,
                    animated => emoji.animated,
                    name => emoji.name.filter_null(),
                    managed => emoji.managed,
                    require_colons => emoji.require_colons,
                    roles => emoji.roles.clone().into_iter().map(Snowflake::from).collect():Vec<_>,
                )?;
            }

            //members
            for member in guild.members.values() {
                let user = member.user.read();

                pg_insert_helper!(
                    conn, member,
                    guild_rowid => guild_rowid,
                    guild_id => SmartHax(member.guild_id),
                    deaf => member.deaf,
                    joined_at => member.joined_at,
                    mute => member.mute,
                    nick => member.nick.filter_null(),
                    roles => member.roles.clone().into_iter().map(Snowflake::from).collect():Vec<_>,
                    user_info => DbDiscordUser::from(user.clone()),
                )?;

            }

            //presences
            for presence in guild.presences.values() {
                pg_insert_helper!(
                    conn, user_presence,
                    ready_rowid => None:Option<i64>,
                    guild_rowid => guild_rowid, //guild_rowid
                    game => presence.activity.clone().map(DbUserPresenceGame::from),
                    last_modified => presence.last_modified.map(|v| v.try_into().unwrap():i64),
                    nick => presence.nick.filter_null(),
                    status => presence.status.into_str(),
                    user_id => SmartHax(presence.user_id),
                )?;
            }

            //roles
            for role in guild.roles.values() {
                pg_insert_helper!(
                    conn, guild_role,
                    discord_id => SmartHax(role.id), 
                    guild_rowid => guild_rowid, 
                    colour_u32 => Some(DiscordColour::from(role.colour)),
                    hoist => role.hoist,
                    managed => role.managed,
                    mentionable => role.mentionable,
                    name => role.name.filter_null(),
                    permissions_bits => PermsToSql(role.permissions),
                    position => role.position,
                )?;
            }

            //voice_states
            for voice_state in guild.voice_states.values() {
                pg_insert_helper!(
                    conn, voice_state,
                    guild_rowid => guild_rowid,
                    channel_id => SmartHaxO(voice_state.channel_id),
                    deaf => voice_state.deaf,
                    mute => voice_state.mute,
                    self_deaf => voice_state.self_deaf,
                    self_mute => voice_state.self_mute,
                    session_id => voice_state.session_id.filter_null(),
                    suppress => voice_state.suppress,
                    token => voice_state.token.filter_null(),
                    user_id => SmartHax(voice_state.user_id),
                )?;
            }
            Ok(()):Result<_, CetrizineError>
        })?;
        //tx.commit()?;

        for (chan_id, _chan_rowid) in guild_channels {
            let cell = &guild.channels[&chan_id];
            let chan = cell.read();
            use serenity::model::channel::ChannelType::*;
            match chan.kind {
                Text => (),
                Private => (),
                Voice => continue,
                Group => (),
                Category => continue,
                News => (),
                Store => (), // I don't *think* a store has any messages, but the discord docs are hella unclear, as per usual.
            }

            let uid = USER_ID.load(Ordering::Relaxed);
            if uid == 0 {
                warn!("USER_ID was read as 0.");
            }

            let perms = guild.user_permissions_in(chan_id, UserId(uid));
            if !perms.read_message_history() {
                info!("Skipping {}, cannot read message history", chan.name);
                continue;
            }
            channel_archiver_sender.send((
                Channel::Guild(Arc::clone(&cell)),
                guild.name.clone(),
            )).unwrap();
        } //for (chan_id, chan) in guild.id.channels()?
        Ok(())
    }
    
    /// This fn will block until it appears that all previous messages have been retrieved. Caller should probably run this in another thread.
    fn grab_full_archive(
        conn: &diesel::pg::PgConnection,
        ctx: Context,
        rdy: Ready,
        channel_archiver_sender: Sender<(Channel, String)>,
        session_id: i64,
    ) -> Result<(), CetrizineError> {
        let mut ready_rowid = 0;
        let mut private_channels_to_archive:Vec<ChannelId> = Vec::new();
        let rdy = &rdy;
        conn.transaction(|| {
            let dbscu = DbSerenityCurrentUser::from(rdy.user.clone());
            ready_rowid = pg_insert_helper!(
                conn, ready,
                session_id => &rdy.session_id,
                shard => rdy.shard.map(|a| vec![
                    a[0].try_into().unwrap():i64,
                    a[1].try_into().unwrap():i64,
                ]),
                trace => rdy.trace.iter().collect():Vec<_>,
                user_info => dbscu,
                version => rdy.version.try_into().unwrap():i64,
            )?;

            //presences
            for presence in rdy.presences.values() {            
                pg_insert_helper!(
                    conn, user_presence,
                    ready_rowid => ready_rowid,
                    guild_rowid => None:Option<i64>,
                    game => presence.activity.clone().map(DbUserPresenceGame::from),
                    last_modified => presence.last_modified.map(|v| v.try_into().unwrap():i64),
                    nick => presence.nick.filter_null(),
                    status => presence.status.into_str(),
                    user_id => SmartHax(presence.user_id),
                )?;
            }
            
            //private_channels
            for (id, channel) in &rdy.private_channels {
                use serenity::model::channel::Channel::*;
                match channel {
                    Guild(_) | Category(_) => warn!("Discord sent a Guild channel in the private channels list, id {}", id),
                    Group(group_lock) => {
                        let group = group_lock.read();
                        pg_insert_helper!(
                            conn, group_channel,
                            discord_id => SmartHax(group.channel_id),
                            ready_rowid => ready_rowid,
                            icon => group.icon.filter_null(),
                            last_message_id => SmartHaxO(group.last_message_id),
                            last_pin_timestamp => group.last_pin_timestamp,
                            name => group.name.filter_null(),
                            owner_id => SmartHax(group.owner_id),
                            recipients => group.recipients.iter().map(|rl| DbDiscordUser::from(rl.1.read().clone())).collect():Vec<_>,
                        )?;

                        private_channels_to_archive.push(group.channel_id);
                        set_after_message_id(&conn, session_id, group.channel_id, group.last_message_id)?;
                    },
                    Private(chan_lock) => {
                        let chan = chan_lock.read();
                        let user = chan.recipient.read();
                        pg_insert_helper!(
                            conn, private_channel,
                            discord_id => SmartHax(chan.id),
                            ready_rowid => ready_rowid,
                            last_message_id => SmartHaxO(chan.last_message_id),
                            last_pin_timestamp => chan.last_pin_timestamp,
                            kind => chan.kind.into_str(),
                            recipient => DbDiscordUser::from(user.clone()),
                        )?;
                        private_channels_to_archive.push(chan.id);
                        set_after_message_id(&conn, session_id, chan.id, chan.last_message_id)?;
                    },
                }
            }
            Ok(()):Result<_,CetrizineError>    
        })?;
        //tx.commit()?;

        for guild_status in &rdy.guilds {
            let guild = match guild_status {
                serenity::model::guild::GuildStatus::OnlineGuild(g) => g,
                _ => {
                    info!("SKIPPING GUILD, dont have full info needed");
                    continue;
                }
            };
            Self::archive_guild(
                conn,
                &ctx,
                guild,
                GuildParentId::Ready(ready_rowid),
                &channel_archiver_sender,
                session_id,
            )?;
        }

        //This is for consistency in debugging purposes, if a certain channel is causing problems it can be annoying when it changes ordering
        //private_channels_to_archive.sort_unstable_by_key(|p| p.0);

        for chan_id in &private_channels_to_archive {
            channel_archiver_sender.send((
                chan_id.to_channel_cached(&ctx).expect("channel absolutely should be cached"),
                String::from(""),
            )).unwrap();
        }
        Ok(())
    } //fn grab_archive ...
    
    fn message_result(&self, ctx: Context, msg: Message) -> Result<(), CetrizineError> {
        let handler_start = (&ctx.raw_event).as_ref().expect("Should be eventful").happened_at_chrono;
        let conn = ctx.get_pool_arc().get()?;
        let guild_str;
        if let Some(guild_id) = msg.guild_id {
            if let Some(guild) = guild_id.to_guild_cached(&ctx) {
                guild_str = format!("GNAME: {}", guild.read().name);
            } else {
                match guild_id.to_partial_guild(&ctx) {
                    Ok(part_guild) => guild_str = format!("GNAME: {}", part_guild.name),
                    Err(e) => guild_str = format!("GNAME_ERR: {:?}", e)
                }
            }
        } else {
            guild_str = String::from("NOGUILD");
        }
        let chan_info = format!("CHAN: {:?}", msg.channel_id.to_channel_cached(&ctx).map(|c| get_name(&c)));
        let date_info = format!("DATE: {:?}", handler_start);
        let user_info = format!("USER: {:?}#{}", msg.author.name, msg.author.discriminator);
        let mesg_info = format!("MESG: {:?}", msg.content);

        //let tx = conn.transaction()?;
        conn.transaction(|| {
            use schema::message_archive_gets::dsl;
            Self::archive_message(&conn, &msg, handler_start)?;
            if !msg.attachments.is_empty() {
                ctx.get_attachment_sender().send(()).unwrap();
            }
            make_synthetic_mag(&conn, self.session_id, msg.channel_id.get_snowflake_i64())?;
            diesel::update(dsl::message_archive_gets.filter(
                (
                    dsl::session_id.eq(self.session_id)
                ).and(
                    dsl::channel_id.eq(SmartHax(msg.channel_id))
                ).and(
                    dsl::synthetic.eq(true)
                ).and(
                    (
                        dsl::start_message_id.is_null()
                    ).or(
                        dsl::start_message_id.lt(SmartHax(msg.id))
                    )
                )
            )).set(
                dsl::start_message_id.eq(SmartHax(msg.id))
            ).execute(&conn)?;
            /*tx.execute(
                "UPDATE message_archive_gets SET start_message_id = $1 WHERE session_id = $2 and channel_id = $3 AND synthetic = true AND (start_message_id IS NULL OR start_message_id < $4)",
                &[
                    &Snowflake::from(msg.id),
                    &self.session_id,
                    &msg.channel_id.get_snowflake_i64(),
                    &msg.id.get_snowflake_i64(),
                ],
            )?;*/
            diesel::update(dsl::message_archive_gets.filter(
                (
                    dsl::session_id.eq(self.session_id)
                ).and(
                    dsl::channel_id.eq(SmartHax(msg.channel_id))
                ).and(
                    dsl::synthetic.eq(true)
                ).and(
                    (
                        dsl::end_message_id.is_null()
                    ).or(
                        dsl::end_message_id.gt(SmartHax(msg.id))
                    )
                )
            )).set(
                dsl::end_message_id.eq(SmartHax(msg.id))
            ).execute(&conn)?;
            /*tx.execute(
                "UPDATE message_archive_gets SET end_message_id = $1 WHERE session_id = $2 AND channel_id = $3 AND synthetic = true AND (end_message_id IS NULL OR end_message_id > $4)",
                &[
                    &Snowflake::from(msg.id),
                    &self.session_id,
                    &msg.channel_id.get_snowflake_i64(),
                    &msg.id.get_snowflake_i64(),
                ],
            )?;*/
            diesel::update(dsl::message_archive_gets.filter(
                (
                    dsl::session_id.eq(self.session_id)
                ).and(
                    dsl::channel_id.eq(SmartHax(msg.channel_id))
                ).and(
                    dsl::synthetic.eq(true)
                )
            )).set(
                dsl::finished.eq(true)
            ).execute(&conn)?;
            /*tx.execute(
                "UPDATE message_archive_gets SET finished = true WHERE session_id = $1 AND channel_id = $2 AND synthetic = true",
                &[
                    &self.session_id,
                    &msg.channel_id.get_snowflake_i64(),
                ],
            )?;*/
            Ok(()):Result<_,CetrizineError>
        })?;

        println!("{}\n{}\n{}\n{}\n{}\n", guild_str, chan_info, date_info, user_info, mesg_info);
        Ok(())
    }

    // fn _channel_create(&self, ctx: Context, channel_lock: Arc<RwLock<GuildChannel>>) -> Result<(), CetrizineError> {
    //     let _recvd_at = Utc::now();
    //     let _conn = ctx.get_pool_arc().get()?;
    //     let chan = channel_lock.read();
    //     info!("Chan create! {:?}", chan.name);
    //     Ok(())
    // }

    fn record_shard_stage_update(&self, ctx: Context, ssue: ShardStageUpdateEvent) -> Result<(), CetrizineError> {
        let conn = ctx.get_pool_arc().get()?;
        let moment = DbMoment::now(self.session_id, self.beginning_of_time);
        pg_insert_helper!(
            &conn, shard_stage_update_event,
            happened_at => moment,
            new_stage => ssue.new.into_str(),
            old_stage => ssue.old.into_str(),
            shard_id => SmartHax(ssue.shard_id),
        )?;
        Ok(())
    }

    fn channel_update_result(&self, ctx: Context, _old: Option<Channel>, new: Channel) -> Result<(), CetrizineError> {
        let channel_archiver_sender = self.archival_queue.lock().unwrap().clone();
        let maybe_guild_lock = match &new {
            Channel::Guild(guild_channel_lock) => match guild_channel_lock.read().guild_id.to_guild_cached(&ctx) {
                Some(g) => Some(g),
                None => {
                    warn!("Received a channel update event but guild was not cached");
                    None
                }
            },
            _ => None,
        };
        //let () = maybe_guild_lock;
        let user_id = UserId(USER_ID.load(Ordering::Relaxed));
        if let Some(guild_lock) = maybe_guild_lock.as_ref() {
            let guild = guild_lock.read();
            if guild.user_permissions_in(new.id(), user_id).read_message_history() {
                channel_archiver_sender.send((
                    new,
                    guild.name.clone(),
                )).unwrap();
            }
        } else {
            channel_archiver_sender.send((
                new,
                String::from(""),
            )).unwrap();
        }
        Ok(())
    }

    fn recheck_guild_permissions(&self, ctx: Context, guild_id: GuildId) -> Result<(), CetrizineError> {
        let channel_archiver_sender = self.archival_queue.lock().unwrap().clone();
        let my_id = UserId(USER_ID.load(Ordering::Relaxed));
        debug!("Rechecking perms for guild id {:?}", guild_id);
        let mut new_chans:Vec<Channel> = Vec::new();
        if let Some(guild_lock) = guild_id.to_guild_cached(&ctx) {
            let guild = guild_lock.read();
            for chan_id in guild.channels.keys() {
                debug!("Rechecking perms for chan id {:?}", chan_id);
                if guild.user_permissions_in(chan_id, my_id).read_message_history() {
                    let new_chan = ctx.http().get_channel(chan_id.0)?;
                    debug!("Rechecking found we can read messages in {:?}; chan is {:?}", chan_id, new_chan);
                    channel_archiver_sender.send((
                        new_chan.clone(),
                        guild.name.clone(),
                    )).unwrap();
                    new_chans.push(new_chan);
                }
            }
        } else { warn!("Guild {:?} not found in cache", guild_id); }

        for new_chan in new_chans {
            let mut cache = ctx.cache().write();
            let _old = cache.insert_channel(&new_chan);
        }

        Ok(())
    }

    fn guild_member_update_result(&self, ctx: Context, _old: Option<Member>, new: Member) -> Result<(), CetrizineError> {
        let my_id = UserId(USER_ID.load(Ordering::Relaxed));
        debug!("gmur: new is {:?}, I'm {:?}", new, my_id);
        if new.user_id() == my_id {
            self.recheck_guild_permissions(ctx, new.guild_id)?;
        }
        Ok(())
    }
}

impl EventHandler for Handler {
    fn reaction_add(&self, ctx: Context, add_reaction: Reaction) {
        log_any_error!(self.reaction_add_result(ctx, add_reaction));
    }
    fn ready(&self, ctx: Context, ready: Ready) {
        // Discord can send multiple readys! What the fuck discord!?
        // This is okay, at worst there'll be multiple channels to
        // archive in the queue.
        log_any_error!(self.ready_result(ctx, ready));
    }

    fn message(&self, ctx: Context, msg: Message) {
        //if true { return }
        log_any_error!(self.message_result(ctx, msg));
    }

    /*fn channel_create(&self, ctx: Context, channel: Arc<RwLock<GuildChannel>>) {
        log_any_error!(self._channel_create(ctx, channel));
    }*/

    fn raw_websocket_packet(&self, ctx: Context, packet: WsEvent) {
        log_any_error!(self.archive_raw_event(ctx, packet));
    }

    fn shard_stage_update(&self, ctx: Context, ssue: ShardStageUpdateEvent) {
        log_any_error!(self.record_shard_stage_update(ctx, ssue));
    }

    fn guild_create(&self, ctx: Context, guild: Guild, is_new: bool) {
        log_any_error!(self.guild_create_result(ctx, guild, is_new));
    }

    fn channel_update(&self, context: Context, old: Option<Channel>, new: Channel) {
        log_any_error!(self.channel_update_result(context, old, new));
    }

    fn guild_member_update(&self, context: Context, old: Option<Member>, new: Member) {
        log_any_error!(self.guild_member_update_result(context, old, new));
    }

    fn guild_role_create(&self, context: Context, guild_id: GuildId, _new: Role) {
        log_any_error!(self.recheck_guild_permissions(context, guild_id));
    }

    fn guild_role_delete(&self, context: Context, guild_id: GuildId, _removed_id: RoleId, _removed: Option<Role>) {
        log_any_error!(self.recheck_guild_permissions(context, guild_id));
    }
    fn guild_role_update(&self, context: Context, guild_id: GuildId, _old: Option<Role>, _new: Role) {
        log_any_error!(self.recheck_guild_permissions(context, guild_id));
    }
    fn guild_update(
        &self,
        context: Context,
        _old_data_if_available: Option<Arc<RwLock<Guild>>>,
        new: PartialGuild
    ) {
        log_any_error!(self.recheck_guild_permissions(context, new.id));
    }
}

fn main() {
    use argparse::{ArgumentParser, StoreTrue, StoreOption, Store, Print};
    {
        let beginning_of_time = std::time::Instant::now();
        let started_at = chrono::Utc::now();
        //let mut verbose = false;
        let mut discord_token = String::from("");
        let mut postgres_path = String::from("");
        let mut no_auto_migrate = false;
        let mut migrate_only = false;
        let mut init_db = false;

        {
            let mut ap = ArgumentParser::new();
            ap.set_description("A discord bot for recording/archiving");
            ap.refer(&mut discord_token)
                .envvar("DISCORD_TOKEN")
                .required()
                .add_option(&["-t", "--token"], Store,
                            "Discord API token to use. Can also be provided in environment variable DISCORD_TOKEN");
            ap.add_option(
                &["-V", "-v", "--version"],
                Print(format!(
                    "{} {}
Target triple: {}
Built at: {}
Commit: {} committed on {}
DB migration version: {}",
                    env!("CARGO_PKG_NAME"),
                    env!("CARGO_PKG_VERSION"),
                    env!("VERGEN_TARGET_TRIPLE"),
                    env!("VERGEN_BUILD_TIMESTAMP"),
                    env!("VERGEN_SHA"),
                    env!("VERGEN_COMMIT_DATE"),
                    migrations::CURRENT_MIGRATION_VERSION),
                ),
                "Show version"
            );
            ap.refer(&mut postgres_path)
                .envvar("POSTGRES_PATH")
                .required()
                .add_option(&["-p", "--postgres-path"], Store,
                            "Postgres connection path. Can also be provided in environment variable POSTGRES_PATH");
            ap.refer(&mut no_auto_migrate)
                .add_option(&["--no-auto-migrate"], StoreTrue,
                            "Don't automatically perform database migrations");
            ap.refer(&mut migrate_only)
                .add_option(&["--migrate-only"], StoreTrue,
                            "Perform any neccesary database migrations and exit");
            ap.refer(&mut init_db)
                .add_option(&["--init-db"], StoreTrue,
                            "Initializes the database schema. Note that the `CREATE DATABASE` command needs to be run separately. This will also run the program normally unless --migrate-only is specified.");
            ap.parse_args_or_exit()
        }

        let manager = r2d2::ConnectionManager::new(postgres_path.as_str());
        let pool = r2d2::Pool::new(manager).unwrap();

        let simple_logger_config = simplelog::ConfigBuilder::new()
            .set_target_level(simplelog::LevelFilter::Error)
            .set_location_level(simplelog::LevelFilter::Error)
            .build();
        let simple_logger = simplelog::SimpleLogger::new(simplelog::LevelFilter::Debug, simple_logger_config);
        let pg_logger = Box::new(
            postgres_logger::PostgresLogger::new(
                pool.clone(),
                log::Level::Info,
                beginning_of_time
            )
        );
        multi_log::MultiLogger::init(vec![simple_logger, pg_logger], simplelog::Level::Debug).expect("Failed to intialize logging.");
        info!("Cetrizine logging initialized.");
        
        let session_id;
        {
            let setup_conn = pool.get().unwrap();
            if init_db {
                use schema::migration_version::dsl;
                setup_conn.transaction(||{
                    use diesel::connection::SimpleConnection;
                    setup_conn.batch_execute(migrations::DB_INIT_SQL).unwrap();
                    diesel::insert_into(dsl::migration_version)
                        .values(
                            dsl::version.eq(7i64)
                        )
                        .execute(&setup_conn)?;
                    Ok(()):Result<_,CetrizineError>
                }).unwrap();
            }
            if migrate_only || init_db || !no_auto_migrate {
                migrations::do_postgres_migrate(&*setup_conn);
                if migrate_only { std::process::exit(0); }
            } else if !migrations::migration_is_current(&*setup_conn) {
                panic!("Migration version mismatch, no auto migrate, aborting.");
            }

            session_id = pg_insert_helper!(
                &setup_conn, run_session,
                started_at => started_at,
                pkg_name => env!("CARGO_PKG_NAME"),
                version => env!("CARGO_PKG_VERSION"),
                target => env!("VERGEN_TARGET_TRIPLE"),
                build_timestamp => env!("VERGEN_BUILD_TIMESTAMP"),
                git_sha_ref => env!("VERGEN_SHA"),
                git_commit_date => env!("VERGEN_COMMIT_DATE"),
            ).unwrap();
        }

        SESSION_ID.store(session_id, Ordering::Relaxed);

        let (chan_tx, chan_rx) = mpsc::channel();
        let arc_pool = Arc::new(pool);

        let handler = Handler{
            beginning_of_time,
            session_id,
            archival_queue: StdMutex::new(chan_tx),
        };
        let mut client = Client::new(&discord_token, handler).expect("Err creating client");

        let threads_cache_and_http = Arc::clone(&client.cache_and_http);
        let threads_arc_pool = Arc::clone(&arc_pool);
        std::thread::spawn(move || {
            while let Ok((channel, guild_name)) = chan_rx.recv() {
                if let Some(conn) = log_any_error!(threads_arc_pool.get()) {
                    let res = Handler::grab_channel_archive(threads_cache_and_http.as_ref(), &*conn, &channel, guild_name);
                    log_any_error!(res);
                }
            }
        });

        let (new_attachment_tx, new_attachment_rx) = std::sync::mpsc::channel::<()>();

        let attachment_download_thread_arc_pool = Arc::clone(&arc_pool);
        std::thread::spawn(move || {
            loop {
                while let Ok(()) = new_attachment_rx.try_recv() {}
                info!("Starting archive_attachments thread");
                log_any_error!(attachments::archive_attachments(
                    &attachment_download_thread_arc_pool,
                    session_id,
                    beginning_of_time,
                ));
                info!("Finished archive_attachments thread");
                new_attachment_rx.recv().unwrap();
            }
        });

        let scan_thread_arc_pool = Arc::clone(&arc_pool);
        std::thread::spawn(move ||{
            use schema::raw_message::dsl;
            let conn = scan_thread_arc_pool.get().unwrap();
            log_any_error!{(|| {
                let mut empty_notif_count:u64 = 0;
                loop {
                    let needing_scan:Vec<attachments::RawMessage> = dsl::raw_message.filter(
                        dsl::scanned_for_urls.eq(false)
                    ).limit(100).load(&conn)?;
                    if needing_scan.is_empty() {
                        if empty_notif_count % 1024 == 0 {
                            info!("Found no rows needing scan.");
                        }
                        empty_notif_count += 1;
                        std::thread::sleep(Duration::from_millis(1000));
                        continue;
                    }
                    info!("Found {} rows needing scan.", needing_scan.len());
                    for row in needing_scan {
                        attachments::scan_urls_from_ws_message(&*conn, &row)?;
                    }
                }
            })():Result<(), CetrizineError>};
        });

        let download_thread_arc_pool = Arc::clone(&arc_pool);
        std::thread::spawn(move ||{
            use schema::raw_message_url::dsl;
            let conn = download_thread_arc_pool.get().unwrap();
            log_any_error!{(|| {
                let mut empty_notif_count:u64 = 0;
                loop {
                    let needing_download:Vec<attachments::RawMessageUrl> = dsl::raw_message_url.filter(
                        dsl::been_downloaded.eq(false)
                    ).limit(100).load(&conn)?;
                    if needing_download.is_empty() {
                        if empty_notif_count % 1024 == 0 {
                            info!("Found no rows needing download.");
                        }
                        empty_notif_count += 1;
                        std::thread::sleep(Duration::from_millis(1000));
                        continue;
                    }
                    info!("Found {} rows needing download.", needing_download.len());

                    let mut client = attachments::build_client();

                    for row in needing_download {
                        attachments::download_scanned_url(&*conn, &mut client, session_id, beginning_of_time, row)?;
                    }
                }
            })():Result<(), CetrizineError>};
        });

        let is_bot = discord_token.starts_with("Bot ");
        if is_bot {
            info!("Detected bot token, running with commands enabled");
            let current_user = client.cache_and_http.http.get_current_user().expect("I don't know who I am!");
            client.with_framework(commands::cetrizine_framework(current_user.id));
        }else{
            warn!("Found non-bot token, will not respond to commands");
        }

        {
            let mut data = client.data.write();
            data.insert::<PoolArcKey>(Arc::clone(&arc_pool));
            data.insert::<IsBotBoolKey>(is_bot);
            let shard_manager_arc = Arc::clone(&client.shard_manager);
            data.insert::<ShardManagerArcKey>(shard_manager_arc);
            data.insert::<NewAttachmentNotifSenderKey>(Mutex::new(new_attachment_tx));
        }
        
        client.start().expect("Error starting client");
    }

    //If we get to this point, that means the client shut down gracefully.

    let do_re_exec = *DO_RE_EXEC.lock().unwrap();
    if let Some(chan_id) = do_re_exec {
        let args:Vec<OsString> = std::env::args_os().collect();
        let chan_str = format!("{}", chan_id);
        info!("Going in for re-exec");
        let err = process::Command::new(&args[0])
            .args(&args[1..])
            .env("RE_EXECD", chan_str)
            .exec();
        error!("Error re-execing: {:?}", err);
    }
}

    
