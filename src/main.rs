#![feature(type_alias_enum_variants)]
#![feature(trace_macros)]
#![feature(nll)]
#![feature(const_slice_len)]
#![feature(option_flattening)]
#![deny(unused_must_use)]

//Crates for debugging/logging
#[macro_use]
extern crate log;
extern crate backtrace;
extern crate simplelog;
extern crate multi_log;

//Crates for sqlite -> postgres migration
extern crate rusqlite as sqlite;
extern crate pbr;

//Main discord library
#[macro_use]
extern crate serenity;

//Database, database connection pooling
extern crate r2d2_postgres;
#[macro_use]
extern crate postgres as pg;
#[macro_use]
extern crate postgres_derive;

//Used by serenity for sharedata
extern crate typemap;

//Gotta keep time
extern crate chrono;
extern crate time;

use backtrace::Backtrace;

use r2d2_postgres::r2d2;

//use pg::types::{IsNull,Type,ToSql,FromSql};

use chrono::prelude::{DateTime,Utc};

use serenity::{
    model::{gateway::Ready, channel::Message, channel::Channel},
    model::prelude::*,
    prelude::*,
    client::bridge::gateway::{
        event::ShardStageUpdateEvent,
        //ShardId,
    },
};

use std::sync::{
    Mutex,
    Arc,
    mpsc::{self, Sender},
    atomic::{Ordering, AtomicI64, AtomicU64},
};

macro_rules! pg_insert_helper {
    ($db:ident, $table_name:expr, $( $column_name:ident => $column_value:expr , )+ ) => {{
        let table_name = $table_name;
        let values:&[&pg::types::ToSql] = &[
            $(
                &$column_value as &pg::types::ToSql,
            )+
        ];

        let pg_column_names = &[
            $( stringify!($column_name), )+
        ];

        let pg_parameters = (0..pg_column_names.len()).into_iter().map(|j| format!("${}",j+1)).collect::<Vec<String>>().join(","); //makes a string like "$1,$2,$3" if pg_column_names.len() == 3


        let sql = &[
            "INSERT INTO ", table_name,
            " (", &pg_column_names.join(","), ") ",
            "VALUES (", &pg_parameters, ") ",
        ].join("");
        
        ( || -> pg::Result<()>{
            assert_eq!($db.prepare_cached(sql)?.execute(values)?,1);
            Ok(())
        } )()
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

mod legacy; //this *must* come after the pg_insert_helper macro
mod db_types;
mod migrations;
mod postgres_logger;
mod commands;

use db_types::*;

const DISCORD_MAX_SNOWFLAKE:u64 = 9223372036854775807;

static SESSION_ID:AtomicI64 = AtomicI64::new(0);
static USER_ID:AtomicU64 = AtomicU64::new(0);

struct PoolArcKey;
impl typemap::Key for PoolArcKey {
    type Value = Arc<r2d2::Pool<r2d2_postgres::PostgresConnectionManager>>;
}

struct IsBotBoolKey;
impl typemap::Key for IsBotBoolKey {
    type Value = bool;
}

trait ContextExt {
    fn get_pool_arc(&self) -> Arc<r2d2::Pool<r2d2_postgres::PostgresConnectionManager>>;
    fn is_bot(&self) -> bool;
}

impl ContextExt for Context {
    fn get_pool_arc(&self) -> Arc<r2d2::Pool<r2d2_postgres::PostgresConnectionManager>> {
        Arc::clone(self.data.lock().get::<PoolArcKey>().unwrap())
    }

    fn is_bot(&self) -> bool {
        *self.data.lock().get::<IsBotBoolKey>().unwrap()
    }
}    

struct Handler{
    pool: Arc<r2d2::Pool<r2d2_postgres::PostgresConnectionManager>>,
    beginning_of_time: std::time::Instant,
    session_id: i64,
    archival_queue: Mutex<Sender<(Channel, String)>>,
}

pub trait EnumIntoString : Sized {
    fn into_str(&self) -> &'static str;
    fn from_str<'a>(input: &'a str) -> Option<Self>;
}

trait OptionExt {
    type Inner;
    fn collapse(self) -> Option<Self::Inner>;
}

impl<T> OptionExt for Option<Option<T>> {
    type Inner = T;
    fn collapse(self) -> Option<Self::Inner> {
        match self {
            None => None,
            Some(None) => None,
            Some(Some(inner)) => Some(inner),
        }
    }
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
        self.clone().collapse().filter_null()
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
            fn from_str<'a>(input: &'a str) -> Option<Self> {
                match input {
                    $( v if v == stringify!($var) => Some(<$enum>::$var), )+
                    _ => None,
                }
            }
        }
    };
}

enum_stringify!{ serenity::model::channel::MessageType => Regular, GroupRecipientAddition, GroupRecipientRemoval, GroupCallCreation, GroupNameUpdate, GroupIconUpdate, PinsAdd, MemberJoin }
enum_stringify!{ serenity::model::guild::DefaultMessageNotificationLevel => All, Mentions }
enum_stringify!{ serenity::model::guild::ExplicitContentFilter => None, WithoutRole, All }
enum_stringify!{ serenity::model::guild::MfaLevel => None, Elevated }
enum_stringify!{ serenity::model::guild::VerificationLevel => None, Low, Medium, High, Higher }
enum_stringify!{ serenity::model::channel::ChannelType => Text, Private, Voice, Group, Category }
enum_stringify!{ serenity::model::gateway::GameType => Playing, Streaming, Listening }
enum_stringify!{ serenity::model::user::OnlineStatus => DoNotDisturb, Idle, Invisible, Offline, Online }
enum_stringify!{ serenity::gateway::ConnectionStage => Connected, Connecting, Disconnected, Handshake, Identifying, Resuming }
enum_stringify!{ log::Level => Error, Warn, Info, Debug, Trace }

impl EnumIntoString for serenity::OwnedMessage {
    fn into_str(&self) -> &'static str {
        use serenity::OwnedMessage::*;
        match self {
            Text(_) => "Text",
            Binary(_) => "Binary",
            Close(_) => "Close",
            Ping(_) => "Ping",
            Pong(_) => "Pong",
        }
    }
    fn from_str<'a>(_input: &'a str) -> Option<Self> { None }
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

impl OwnedMessageExt for serenity::OwnedMessage {
    fn into_data(self) -> OwnedMessageData {
        use serenity::OwnedMessage::*;
        use OwnedMessageData::*;
        match self {
            Text(s) => TextData(s),
            Binary(d) => BinaryData(Some(d)),
            Close(od) => BinaryData(od.map(|cd| cd.into_bytes().unwrap())),
            Ping(d) => BinaryData(Some(d)),
            Pong(d) => BinaryData(Some(d)),
        }
    }
}

fn get_name(chan:&Channel) -> String {
    use serenity::model::channel::Channel::*;
    
    match chan {
        Group(group) => group.read().name.clone().unwrap_or("".to_owned()),
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

fn pg_sequence_currval<C: pg::GenericConnection>(c: &C, table: &str, column: &str) -> Result<i64, CetrizineError> {
    let res:i64 = c.query(&format!("SELECT currval(pg_get_serial_sequence('{}','{}'));",table,column),&[])?.get(0).get(0);
    return Ok(res);
}

#[derive(Debug)]
pub struct CetrizineError{
    pub error_type: CetrizineErrorType,
    pub backtrace: Backtrace,
}

impl CetrizineError {
    fn new(error_type: CetrizineErrorType) -> Self {
        CetrizineError{
            error_type,
            backtrace: Backtrace::new(),
        }
    }
}

#[derive(Debug)]
pub enum CetrizineErrorType{
    Pool(r2d2::Error),
    Sql(pg::Error),
    Serenity(serenity::Error),
}

impl std::fmt::Display for CetrizineError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for CetrizineError {
    fn source(&self) -> Option<&(std::error::Error + 'static)> {
        use CetrizineErrorType::*;
        match &self.error_type {
            //Mutex(e)    => Some(e),
            Pool(e)     => Some(e),
            Sql(e)      => Some(e),
            Serenity(e) => Some(e),
        }
    }
}

impl From<pg::Error> for CetrizineError {
    fn from(err: pg::Error) -> Self {
        CetrizineError::new(CetrizineErrorType::Sql(err))
    }
}

impl From<serenity::Error> for CetrizineError {
    fn from(err: serenity::Error) -> Self {
        CetrizineError::new(CetrizineErrorType::Serenity(err))
    }
}

impl From<r2d2::Error> for CetrizineError {
    fn from(err: r2d2::Error) -> Self {
        CetrizineError::new(CetrizineErrorType::Pool(err))
    }
}


impl Handler {
    fn archive_raw_event(&self, _ctx: Context, ev: RawEvent) -> Result<(), CetrizineError> {
        let conn = self.pool.get()?;
        let recvd_duration = ev.happened_at_instant - self.beginning_of_time;
        let msg_type_str = ev.data.into_str();
        let (msg_content_text, msg_content_binary) = ev.data.into_two_data();
        pg_insert_helper!(
            conn, "raw_message",
            recvd_at_datetime => ev.happened_at_chrono,
            recvd_at_duration_secs => (recvd_duration.as_secs() as i64),
            recvd_at_duration_nanos => (recvd_duration.subsec_nanos() as i32),
            session_rowid => self.session_id,
            kind => msg_type_str,
            content_text => msg_content_text,
            content_binary => msg_content_binary,
        )?;
        trace!(
            "Inserted raw message {}, {}, {}, {}",
            &ev.happened_at_chrono,
            &(recvd_duration.as_secs() as i64),
            &(recvd_duration.subsec_nanos() as i32),
            &self.session_id,
        );
        Ok(())
    }

    fn guild_create_result(&self, ctx: Context, guild: Guild, is_new: bool) -> Result<(), CetrizineError> {
        let channel_archiver_sender = self.archival_queue.lock().unwrap().clone();
        let conn = self.pool.get()?;
        let ev = ctx.raw_event.clone().unwrap();
        let recvd_duration = ev.happened_at_instant - self.beginning_of_time;
        trace!(
            "About to insert guild create event {}, {}, {}, {}",
            &ev.happened_at_chrono,
            &(recvd_duration.as_secs() as i64),
            &(recvd_duration.subsec_nanos() as i32),
            &self.session_id,
        );

        pg_insert_helper!(
            conn, "guild_create_event",
            is_new => is_new,
            recvd_at_datetime => ev.happened_at_chrono,
            recvd_at_duration_secs => (recvd_duration.as_secs() as i64),
            recvd_at_duration_nanos => (recvd_duration.subsec_nanos() as i32),
            session_rowid => self.session_id,
        )?;

        let guild_create_event_rowid = pg_sequence_currval(&*conn, "guild_create_event", "rowid")?;
        std::thread::spawn(move || {
            log_any_error!(Self::archive_guild(
                &*conn,
                &ctx,
                &guild,
                GuildParentId::CreateEvent(guild_create_event_rowid),
                &channel_archiver_sender,
            ));
        });
        Ok(())
    }

    fn ready_result(&self, ctx: Context, ready: Ready) -> Result<(), CetrizineError> {
        info!("Ready event received. Connected as: {}", &ready.user.name);
        let conn = self.pool.get()?;
        USER_ID.store(ready.user.id.0, Ordering::Relaxed);

        let is_bot = ctx.is_bot();
        if is_bot && !ready.user.bot {
            // This is really bad, this means the bot will
            // respond to commands when it's not supposed to.

            error!("Thought we were a bot when we weren't! Bailing immediately!");
            std::process::exit(1);
        } else if !is_bot && ready.user.bot {
            // I really want to use like "uber warn" or something
            // It's not an error because we continue on just fine...
            warn!("The given token appeared to be a user token but discord tells us a bot. Commands are currently disabled.");
        }
        
        let count_that_should_be_zero:i64 = conn.query(
            "SELECT COUNT(*) FROM ready WHERE ((user_info).inner_user).discord_id != $1;",
            &[&i64::from(ready.user.id)],
        )?.get(0).get(0);

        if count_that_should_be_zero != 0 {
            panic!("Error: you should always log in with the same user for the same database! Found ready with differing user id (current user is id {} aka {}#{})",ready.user.id.0,ready.user.name,ready.user.discriminator);
        }

        let channel_archiver_sender = self.archival_queue.lock().unwrap().clone();
        std::thread::spawn(move || {
            log_any_error!(Self::grab_full_archive(&*conn, ctx, ready, channel_archiver_sender));
        });
        Ok(())
    }
            
    fn archive_message(tx: &pg::transaction::Transaction, msg: &Message, recvd_at:DateTime<Utc>) -> Result<(), CetrizineError> {
        pg_insert_helper!(
            tx, "message",
            discord_id => SmartHax(msg.id),
            author => DbDiscordUser::from(msg.author.clone()),
            channel_id => SmartHax(msg.channel_id),
            content => msg.content.filter_null(),
            edited_timestamp => msg.edited_timestamp,
            guild_id => SmartHax(msg.guild_id),
            kind => msg.kind.into_str(),
            member => msg.member.clone().map(DbPartialMember::from),
            mention_everyone => msg.mention_everyone,
            mention_roles => msg.mention_roles.clone().into_iter().map(|r| SmartHax(r)).collect::<Vec<_>>(),
            mentions => msg.mentions.clone().into_iter().map(DbDiscordUser::from).collect::<Vec<_>>(),
            nonce_debug => format!("{:?}",msg.nonce), //TODO: should probably file a bug and/or pull request with serenity about this one
            pinned => msg.pinned,
            timestamp => msg.timestamp,
            tts => msg.tts,
            webhook_id => SmartHax(msg.webhook_id),
            archive_recvd_at => recvd_at,
        )?;

        let message_rowid = pg_sequence_currval(tx, "message", "rowid")?;

        //attachments
        for attachment in &msg.attachments {
            pg_insert_helper!(
                tx, "attachment",
                message_rowid => message_rowid,
                discord_id => DbSnowflake::from(attachment.id.parse::<i64>().expect("could not parse attachment discord id")), //Yes, attachment.id is a String when all the other .id's are a u64 wrapper
                filename => attachment.filename.filter_null(),
                height => attachment.height.map(|h| h as i64),
                width => attachment.width.map(|w| w as i64),
                proxy_url => attachment.proxy_url.filter_null(),
                size => (attachment.size as i64),
                url => attachment.url.filter_null(),
            )?;
        }

        //embeds
        for embed in &msg.embeds {
            pg_insert_helper!(
                tx, "embed",
                message_rowid => message_rowid,
                author => embed.author.clone().map(DbEmbedAuthor::from),
                colour_u32 => DbDiscordColour::from(embed.colour),
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

            let embed_rowid = pg_sequence_currval(tx, "embed", "rowid")?;

            //insert embed fields
            for embed_field in &embed.fields {
                pg_insert_helper!(
                    tx, "embed_field",
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
                tx, "reaction",
                message_rowid => message_rowid,
                count => (reaction.count as i64),
                me => reaction.me,
                reaction_is_custom => is_custom,
                
                reaction_animated => animated,
                reaction_id => SmartHax(id.clone()),
                reaction_name => name.filter_null(),
                
                reaction_string => string.filter_null(),
            )?;
        }
        
        Ok(())
    }

    //guild_name is used purely for the pretty output and debug messages
    fn grab_channel_archive<C: pg::GenericConnection>(conn: &C, chan: &Channel, guild_name: String) -> Result<(), CetrizineError> {
        let name = get_name(chan);
        info!("ARCHIVING CHAN {}#{} (id {})", &guild_name, &name, &chan.id());
        let mut got_messages = false;

        let last_msg_id = match get_last_message_id(chan) {
            Some(id) => id,
            None => return Ok(()),
        };

        trace!("DEBUG: selecting maybe_res WHERE start >= {} >= end AND chan = {}",last_msg_id,chan.id());
        let rows = conn.query(
            "
SELECT
  rowid,
  end_message_id,
  after_message_id,
  (message_count_received < message_count_requested) 
FROM message_archive_gets 
WHERE 
  (
    (
      start_message_id >= $1 AND $1 >= end_message_id
    ) OR 
    around_message_id = $1
  ) 
  AND 
  channel_id = $2
ORDER BY end_message_id ASC
LIMIT 1;",
            &[
                &(last_msg_id.get_snowflake_i64()),
                &(chan.id().get_snowflake_i64())
            ]
        )?;
        
        let mut maybe_res:Option<(i64,u64,Option<u64>,bool)> =
            if rows.len() == 0 {
                None
            } else if rows.len() == 1 {
                let r = rows.get(0);
                Some((r.get(0),r.get::<_,i64>(1) as u64,r.get::<_,Option<i64>>(2).map(|i| i as u64),r.get(3)))
            } else { panic!() };
        trace!("maybe_res is {:?}",maybe_res);

        let mut get_before = DISCORD_MAX_SNOWFLAKE;
        let mut before_message_id:Option<u64>;
        
        while let Some(res) = maybe_res {
            get_before = res.1;
            before_message_id = res.2;
            if res.3 {
                return Ok(());
            }
            trace!("selecting maybe_res again get_before {} before_message_id {:?} res.0 {}",get_before,before_message_id,res.0);
            let rows = conn.query(
                "SELECT rowid,end_message_id,after_message_id,(message_count_received < message_count_requested) FROM message_archive_gets WHERE (
  ( start_message_id >= $1 AND $1 > end_message_id ) OR
  before_message_id = $1 OR
  ($2::int8 IS NOT NULL AND end_message_id = $2::int8)
) AND channel_id = $3 AND rowid != $4
ORDER BY start_message_id ASC LIMIT 1;",
                &[
                    &(get_before as i64),
                    &(before_message_id.map(|u| u as i64)),
                    &(chan.id().get_snowflake_i64()),
                    &res.0
                ]
            )?;
            maybe_res =
                if rows.len() == 0 {
                    None
                } else if rows.len() == 1 {
                    let r = rows.get(0);
                    Some((r.get(0),r.get::<_,i64>(1) as u64,r.get::<_,Option<i64>>(2).map(|i| i as u64),r.get(3)))
                } else { panic!("Query ending in LIMIT 1 did not return 0 or 1 rows.") };
            trace!("maybe_res again is {:?}",maybe_res);
        }

        // We're "in" a gap, lets find where this gap ends.

        trace!("selecting gap_end where end < {} and chan = {}",get_before,chan.id());
        let rows = conn.query(
            "SELECT start_message_id FROM message_archive_gets WHERE end_message_id < $1 AND channel_id = $2 ORDER BY start_message_id DESC LIMIT 1;",
            &[
                &(get_before as i64),
                &(chan.id().get_snowflake_i64())
            ],
        )?;
        let gap_end =
            if rows.len() == 0 {
                0i64
            } else if rows.len() == 1 {
                rows.get(0).get(0)
            } else { panic!("Query ending in LIMIT 1 did not return 0 or 1 rows.") };

        trace!("gap_end is {:?}",gap_end);
        let mut earliest_message_recvd = get_before;

        while earliest_message_recvd > (gap_end as u64) {
            let asking_for = 100u8;
            
            let mut msgs;
            let around;
            if earliest_message_recvd == DISCORD_MAX_SNOWFLAKE {
                trace!("asking for around {}", last_msg_id);
                around = true;
                msgs = chan.id().messages(|r| r.limit(asking_for.into()).around(last_msg_id))?;
            }else{
                trace!("asking for before {}",earliest_message_recvd);
                around = false;
                msgs = chan.id().messages(|r| r.limit(asking_for.into()).before(earliest_message_recvd))?;
            }
            let recvd_at = Utc::now();
            
            // Messages seem to usually be ordered, largest ids first,
            // However, that's documented literally nowhere, so lets sort them.
            msgs.sort_unstable_by_key(|msg| {
                let msg_id:u64 = msg.id.into();
                std::u64::MAX - msg_id
            });
            let msgs = msgs;
            
            let tx = conn.transaction()?;
            for msg in &msgs {
                trace!("Archiving Message id {:?}", msg.id);
                Self::archive_message(&tx, msg, recvd_at)?;
            }
            if msgs.len() == 0 { break }
            let first_msg = msgs.first().expect("im a bad");
            let last_msg  = msgs.last().expect("im a bad");

            //DEBUG START
            let already_archived_start:bool = tx.query(
                "SELECT COUNT(*) FROM message_archive_gets WHERE (start_message_id >= $1 AND $1 >= end_message_id) AND channel_id = $2 AND rowid > 193737",
                &[
                    &(first_msg.id.get_snowflake_i64()),
                    &(chan.id().get_snowflake_i64())
                ],
            )?.get(0).get::<_,i64>(0) > 0;
            let already_archived_end:bool = tx.query(
                "SELECT COUNT(*) FROM message_archive_gets WHERE (start_message_id >= $1 AND $1 >= end_message_id) AND channel_id = $2 AND rowid > 193737",
                &[
                    &(last_msg.id.get_snowflake_i64()),
                    &(chan.id().get_snowflake_i64())
                ],
            )?.get(0).get::<_,i64>(0) > 0;
            let already_archived_around:bool = tx.query(
                "SELECT COUNT(*) FROM message_archive_gets WHERE (start_message_id >= $1 AND $1 >= end_message_id) AND channel_id = $2 AND rowid > 193737",
                &[
                    &(last_msg_id.get_snowflake_i64()),
                    &(chan.id().get_snowflake_i64())
                ],
            )?.get(0).get::<_,i64>(0) > 0;
            let we_have_archived_this_before = already_archived_start && already_archived_end && (!around || already_archived_around);
            if we_have_archived_this_before {
                error!(
                    "We've archived this before! chan id {} {} {} start {} end {} in {}#{}",
                    chan.id(),
                    if around {"around"} else {"before"},
                    if around {last_msg_id.0} else {earliest_message_recvd},
                    first_msg.id,
                    last_msg.id,
                    guild_name,
                    name
                );
                //std::process::exit(1);
            }
            //DEBUG END
            
            //TODO: put channel_rowid in there somwhere
            pg_insert_helper!(
                tx, "message_archive_gets",
                channel_id => SmartHax(chan.id()),
                around_message_id => if around { Some(SmartHax(last_msg_id)) } else { None },
                before_message_id => if around { None } else { Some(DbSnowflake::from(earliest_message_recvd)) },
                start_message_id => SmartHax(first_msg.id),
                end_message_id => SmartHax(last_msg.id),
                message_count_requested => asking_for as i64,
                message_count_received => msgs.len() as i64,
            )?;
            tx.commit()?;
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
        } //while earliest_message_recvd > gap_end
        
        if got_messages {
            trace!("recursing!");
            return Self::grab_channel_archive(conn, chan, guild_name);
        } else {
            Ok(())
        }
    }

    fn archive_guild<C: pg::GenericConnection>(
        conn: &C,
        ctx: &Context,
        guild: &Guild,
        parent_rowid: GuildParentId,
        channel_archiver_sender: &Sender<(Channel, String)>
    ) -> Result<(), CetrizineError> {
        info!("ARCHIVING GUILD {}", &guild.name);
        let raw_event = ctx.raw_event.as_ref().unwrap();
        let tx = conn.transaction()?;

        pg_insert_helper!(
            tx, "guild",
            ready_rowid => parent_rowid.as_ready(),
            guild_create_event_rowid => parent_rowid.as_create_event(),
            discord_id => SmartHax(guild.id), 
            afk_channel_id => SmartHax(guild.afk_channel_id), 
            afk_timeout => (guild.afk_timeout as i64), 
            application_id => SmartHax(guild.application_id), 
            default_message_notification_level => guild.default_message_notifications.into_str(),
            explicit_content_filter => guild.explicit_content_filter.into_str(),
            features => guild.features.clone().into_iter().map(|s| s.filter_null()).collect::<Vec<_>>(),
            icon => guild.icon.filter_null(),
            joined_at => guild.joined_at,
            large => guild.large,
            member_count => (guild.member_count as i64),
            mfa_level => guild.mfa_level.into_str(),
            name => guild.name.filter_null(),
            owner_id => SmartHax(guild.owner_id),
            region => guild.region.filter_null(),
            splash => guild.splash.filter_null(),
            system_channel_id => SmartHax(guild.system_channel_id),
            verification_level => guild.verification_level.into_str(),
            archive_recvd_at => raw_event.happened_at_chrono,
        )?;            

        let guild_rowid = pg_sequence_currval(&tx, "guild", "rowid")?;

        let mut guild_channels = Vec::<(ChannelId,i64)>::new();

        //channels
        for (_id, chan_a_lock) in &guild.channels {
            let chan = chan_a_lock.read();

            pg_insert_helper!(
                tx, "guild_channel",
                discord_id => SmartHax(chan.id),
                guild_rowid => guild_rowid,
                guild_id => SmartHax(chan.guild_id),
                bitrate => chan.bitrate.map(|b| b as i64),
                category_id => SmartHax(chan.category_id),
                kind => chan.kind.into_str(),
                last_message_id => SmartHax(chan.last_message_id),
                last_pin_timestamp => chan.last_pin_timestamp,
                name => chan.name.filter_null(),
                position => chan.position,
                topic => chan.topic.filter_null(),
                user_limit => chan.user_limit.map(|u| u as i64),
                nsfw => chan.nsfw,
            )?;

            let chan_rowid = pg_sequence_currval(&tx, "guild_channel", "rowid")?;

            guild_channels.push((chan.id.clone(), chan_rowid));

            for overwrite in &chan.permission_overwrites {
                use serenity::model::channel::PermissionOverwriteType::*;
                let (ov_type_str, ov_id) = match overwrite.kind {
                    Member(uid) => ("Member", uid.0),
                    Role(rid) => ("Role", rid.0),
                };

                pg_insert_helper!(
                    tx, "permission_overwrite",
                    guild_channel_rowid => chan_rowid,
                    allow_bits => PermsToSql(overwrite.allow),
                    deny_bits => PermsToSql(overwrite.deny),
                    permission_overwrite_type => ov_type_str,
                    permission_overwrite_id => DbSnowflake::from(ov_id),
                )?;
            }
        }

        //emojis
        for (_, emoji) in &guild.emojis {
            pg_insert_helper!(
                tx, "emoji",
                discord_id => SmartHax(emoji.id),
                guild_rowid => guild_rowid,
                animated => emoji.animated,
                name => emoji.name.filter_null(),
                managed => emoji.managed,
                require_colons => emoji.require_colons,
                roles => emoji.roles.clone().into_iter().map(DbSnowflake::from).collect::<Vec<_>>(),
            )?;
        }

        //members
        for (_, member) in &guild.members {
            let user = member.user.read();

            pg_insert_helper!(
                tx, "member",
                guild_rowid => guild_rowid,
                guild_id => SmartHax(member.guild_id),
                deaf => member.deaf,
                joined_at => member.joined_at,
                mute => member.mute,
                nick => member.nick.filter_null(),
                roles => member.roles.clone().into_iter().map(DbSnowflake::from).collect::<Vec<_>>(),
                user_info => DbDiscordUser::from(user.clone()),
            )?;

        }

        //presences
        for (_, presence) in &guild.presences {
            pg_insert_helper!(
                tx, "user_presence",
                ready_rowid => (None as Option<i64>),
                guild_rowid => guild_rowid, //guild_rowid
                game => presence.game.clone().map(DbUserPresenceGame::from),
                last_modified => presence.last_modified.map(|v| v as i64),
                nick => presence.nick.filter_null(),
                status => presence.status.into_str(),
                user_id => SmartHax(presence.user_id),
            )?;
        }

        //roles
        for (_, role) in &guild.roles {
            pg_insert_helper!(
                tx, "guild_role",
                discord_id => SmartHax(role.id), 
                guild_rowid => guild_rowid, 
                colour_u32 => DbDiscordColour::from(role.colour),
                hoist => role.hoist,
                managed => role.managed,
                mentionable => role.mentionable,
                name => role.name.filter_null(),
                permissions_bits => PermsToSql(role.permissions),
                position => role.position,
            )?;
        }

        //voice_states
        for (_, voice_state) in &guild.voice_states {
            pg_insert_helper!(
                tx, "voice_state",
                guild_rowid => guild_rowid,
                channel_id => SmartHax(voice_state.channel_id),
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
        
        tx.commit()?;

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
            }

            let uid = USER_ID.load(Ordering::Relaxed);
            if uid == 0 {
                warn!("USER_ID was read as 0.");
            }

            let perms = guild.permissions_in(chan_id, UserId(uid));
            if !perms.read_message_history() {
                info!("Skipping {}, cannot read message history", chan.name);
                continue;
            }
            channel_archiver_sender.send((
                Channel::Guild(Arc::clone(&cell)),
                guild.name.clone(),
            )).unwrap();
            /*Self::grab_channel_archive(
            &*conn,
            &Channel::Guild(Arc::clone(&cell)),
            guild.name.clone(),
        )?;*/
        } //for (chan_id, chan) in guild.id.channels()?
        Ok(())
    }
    
    /// This fn will block until it appears that all previous messages have been retrieved. Caller should probably be run in another thread.
    fn grab_full_archive<C: pg::GenericConnection>(
        conn: &C,
        ctx: Context,
        rdy: Ready,
        channel_archiver_sender: Sender<(Channel, String)>
    ) -> Result<(), CetrizineError> {
        let tx = conn.transaction()?;

        pg_insert_helper!(
            tx, "ready",
            session_id => rdy.session_id.filter_null(),
            shard => rdy.shard.clone().map(|a| vec![a[0] as i64, a[1] as i64]),
            trace => rdy.trace.into_iter().map(|s| s.filter_null()).collect::<Vec<_>>(),
            user_info => DbCurrentUser::from(rdy.user.clone()),
            version => (rdy.version as i64),
        )?;

        let ready_rowid = pg_sequence_currval(&tx, "ready", "rowid")?;

        //presences
        for (_,presence) in &rdy.presences {            
            pg_insert_helper!(
                tx, "user_presence",
                ready_rowid => ready_rowid,
                guild_rowid => (None as Option<i64>),
                game => presence.game.clone().map(DbUserPresenceGame::from),
                last_modified => presence.last_modified.map(|v| v as i64),
                nick => presence.nick.filter_null(),
                status => presence.status.into_str(),
                user_id => SmartHax(presence.user_id),
            )?;
        }

        let mut private_channels_to_archive:Vec<ChannelId> = Vec::with_capacity(rdy.private_channels.len());
        
        //private_channels
        for (id, channel) in &rdy.private_channels {
            use serenity::model::channel::Channel::*;
            match channel {
                Guild(_) | Category(_) => panic!("Discord sent a Guild channel in the private channels list, id {}", id),
                Group(group_lock) => {
                    let group = group_lock.read();
                    pg_insert_helper!(
                        tx, "group_channel",
                        discord_id => SmartHax(group.channel_id),
                        ready_rowid => ready_rowid,
                        icon => group.icon.filter_null(),
                        last_message_id => SmartHax(group.last_message_id),
                        last_pin_timestamp => group.last_pin_timestamp,
                        name => group.name.filter_null(),
                        owner_id => SmartHax(group.owner_id),
                        recipients => group.recipients.iter().map(|rl| DbDiscordUser::from(rl.1.read().clone())).collect::<Vec<_>>(),
                    )?;

                    private_channels_to_archive.push(group.channel_id);
                },
                Private(chan_lock) => {
                    let chan = chan_lock.read();
                    let user = chan.recipient.read();
                    pg_insert_helper!(
                        tx, "private_channel",
                        discord_id => SmartHax(chan.id),
                        ready_rowid => ready_rowid,
                        last_message_id => SmartHax(chan.last_message_id),
                        last_pin_timestamp => chan.last_pin_timestamp,
                        kind => chan.kind.into_str(),
                        recipient => DbDiscordUser::from(user.clone()),
                    )?;
                    private_channels_to_archive.push(chan.id);
                },
            }
        }
                

        tx.commit()?;

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
                &channel_archiver_sender
            )?;
        }

        //This is for consistency/debugging purposes, if a certain channel is causing problems it can be annoying when it changes ordering
        //private_channels_to_archive.sort_unstable_by_key(|p| p.0);

        for chan_id in &private_channels_to_archive {
            channel_archiver_sender.send((
                chan_id.to_channel_cached().expect("channel absolutely should be cached"),
                String::from(""),
            )).unwrap();
            /*Self::grab_channel_archive(
                &*conn,
                &chan_id.to_channel_cached().expect("channel absolutely should be cached"),
                String::from("")
            )?;*/
        }
        
        //info!("FINISHed archiving old messages");
        Ok(())
    } //fn grab_archive ...
    
    fn message_result(&self, _: Context, msg: Message) -> Result<(), CetrizineError> {
        let handler_start = Utc::now();
        let conn = self.pool.get()?;
        let guild_str;
        if let Some(guild_id) = msg.guild_id {
            if let Some(guild) = guild_id.to_guild_cached() {
                guild_str = format!("GNAME: {}", guild.read().name);
            } else {
                match guild_id.to_partial_guild() {
                    Ok(part_guild) => guild_str = format!("GNAME: {}", part_guild.name),
                    Err(e) => guild_str = format!("GNAME_ERR: {:?}", e)
                }
            }
        } else {
            guild_str = String::from("NOGUILD");
        }
        let chan_info = format!("CHAN: {:?}", msg.channel_id.to_channel_cached().map(|c| get_name(&c)));
        let date_info = format!("DATE: {:?}", handler_start);
        let user_info = format!("USER: {:?}#{}", msg.author.name, msg.author.discriminator);
        let mesg_info = format!("MESG: {:?}", msg.content);

        let tx = conn.transaction()?;
        Self::archive_message(&tx, &msg, handler_start)?;
        tx.commit()?;

        println!("{}\n{}\n{}\n{}\n{}\n", guild_str, chan_info, date_info, user_info, mesg_info);
        Ok(())
    }

    fn _channel_create(&self, _ctx: Context, channel_lock: Arc<RwLock<GuildChannel>>) -> Result<(), CetrizineError> {
        let _recvd_at = Utc::now();
        let _conn = self.pool.get()?;
        let chan = channel_lock.read();
        info!("Chan create! {:?}", chan.name);
        Ok(())
    }

    fn record_shard_stage_update(&self, _ctx: Context, ssue: ShardStageUpdateEvent) -> Result<(), CetrizineError> {
        let conn = self.pool.get()?;
        let moment = DbMoment::now(self.session_id, self.beginning_of_time);
        pg_insert_helper!(
            conn, "shard_stage_update_event",
            happened_at => moment,
            new_stage => ssue.new.into_str(),
            old_stage => ssue.old.into_str(),
            shard_id => SmartHax(ssue.shard_id),
        )?;
        Ok(())
    }       
}   

impl EventHandler for Handler {
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

    fn raw_websocket_packet(&self, ctx: Context, packet: RawEvent) {
        log_any_error!(self.archive_raw_event(ctx, packet));
    }

    fn shard_stage_update(&self, ctx: Context, ssue: ShardStageUpdateEvent) {
        log_any_error!(self.record_shard_stage_update(ctx, ssue));
    }

    fn guild_create(&self, ctx: Context, guild: Guild, is_new: bool) {
        log_any_error!(self.guild_create_result(ctx, guild, is_new));
    }

    fn channel_update(&self, _ctx: Context, _old: Option<Channel>, _new: Channel) {
        //guild.permissions_in(chan_id, UserId(uid)).read_message_history()
    }
}

fn main() {
    use argparse::{ArgumentParser, StoreTrue, StoreOption, Store, Print};
    let beginning_of_time = std::time::Instant::now();
    let started_at = chrono::Utc::now();
    //let mut verbose = false;
    let mut do_migrate = false;
    let mut discord_token = String::from("");
    let mut postgres_path_opt:Option<String> = None;
    let mut no_auto_migrate = false;
    let mut migrate_only = false;
    let mut init_db = false;

    {
        let mut ap = ArgumentParser::new();
        ap.set_description("A discord bot for recording/archiving");
        ap.refer(&mut do_migrate)
            .add_option(&["--sqlite-migration"], StoreTrue,
                        "Perform a migration from a legacy SQLite database. Put the db file name in DATABASE_FILENAME");
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
        ap.refer(&mut postgres_path_opt)
            //.envvar("PG_PATH")
            .required()
            .add_option(&["-p", "--postgres-path"], StoreOption,
                        "postgres connection path");
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

    let postgres_path = postgres_path_opt.unwrap();

    let manager = r2d2_postgres::PostgresConnectionManager::new(postgres_path.as_str(), r2d2_postgres::TlsMode::None).unwrap();
    let pool = r2d2::Pool::new(manager).unwrap();

    let simple_logger_config = simplelog::Config{
        time: Some(simplelog::Level::Info),
        level: Some(simplelog::Level::Info),
        target: Some(simplelog::Level::Info),
        location: Some(simplelog::Level::Info),
        .. Default::default()
    };
    let simple_logger = simplelog::SimpleLogger::new(simplelog::LevelFilter::Info, simple_logger_config);
    let pg_logger = Box::new(
        postgres_logger::PostgresLogger::new(
            pool.clone(),
            log::Level::Info,
            beginning_of_time
        )
    );
    multi_log::MultiLogger::init(vec![simple_logger, pg_logger], simplelog::Level::Info).expect("Failed to intialize logging.");
    info!("Cetrizine logging initialized.");
    
    if do_migrate {
        legacy::migrate_sqlite_to_postgres(&postgres_path);
        info!("Finished legacy sqlite migration");
        std::process::exit(0);
    }
    
    let session_id;
    {
        let setup_conn = pool.get().unwrap();
        if init_db {
            let tx = setup_conn.transaction().unwrap();
            tx.batch_execute(migrations::DB_INIT_SQL).unwrap();
            pg_insert_helper!(
                tx, "migration_version",
                version => 7i64,
            ).unwrap();
            tx.commit().unwrap();
        }
        if migrate_only || init_db || !no_auto_migrate {
            migrations::do_postgres_migrate(&*setup_conn);
            if migrate_only { std::process::exit(0); }
        } else {
            if !migrations::migration_is_current(&*setup_conn) {
                panic!("Migration version mismatch, no auto migrate, aborting.");
            }
        }

        pg_insert_helper!(
            setup_conn, "run_session",
            started_at => started_at,
            pkg_name => env!("CARGO_PKG_NAME"),
            version => env!("CARGO_PKG_VERSION"),
            target => env!("VERGEN_TARGET_TRIPLE"),
            build_timestamp => env!("VERGEN_BUILD_TIMESTAMP"),
            git_sha_ref => env!("VERGEN_SHA"),
            git_commit_date => env!("VERGEN_COMMIT_DATE"),
        ).unwrap();

        session_id = pg_sequence_currval(&*setup_conn, "run_session", "rowid").unwrap();
    }

    SESSION_ID.store(session_id, Ordering::Relaxed);

    let (chan_tx, chan_rx) = mpsc::channel();
    let arc_pool = Arc::new(pool);

    let handler = Handler{
        pool: Arc::clone(&arc_pool),
        beginning_of_time,
        session_id,
        archival_queue: Mutex::new(chan_tx),
    };
    let mut client = Client::new(&discord_token, handler).expect("Err creating client");

    let threads_arc_pool = Arc::clone(&arc_pool);
    std::thread::spawn(move || {
        while let Ok((channel, guild_name)) = chan_rx.recv() {
            if let Some(conn) = log_any_error!(threads_arc_pool.get()) {
                let res = Handler::grab_channel_archive(&*conn, &channel, guild_name);
                if let Err(CetrizineError{backtrace: bt, error_type: CetrizineErrorType::Serenity(serenity::Error::Http(serenity::prelude::HttpError::UnsuccessfulRequest(mut http_response)))}) = res {
                    warn!(
                        "HTTP error!\n\nHTTP response: {:?}\n\nBacktrace: {:?}",
                        bt,
                        http_response
                    );
                    let mut body = String::new();
                    match std::io::Read::read_to_string(&mut http_response, &mut body) {
                        Ok(body) => warn!("HTTP body: {:?}", body),
                        Err(why) => warn!("could not read http body {:?}", why),
                    }
                }else{ log_any_error!(res); }
            }
        }
    });

    let is_bot;
    if discord_token.starts_with("Bot ") {
        info!("Detected bot token, running with commands enabled");
        client.with_framework(commands::cetrizine_framework());
        is_bot = true;
    }else{
        info!("Found non-bot token, will not respond to commands");
        is_bot = false;
    }

    {
        let mut data = client.data.lock();
        data.insert::<PoolArcKey>(Arc::clone(&arc_pool));
        data.insert::<IsBotBoolKey>(is_bot);
    }
    
    client.start().expect("Error starting client");
}

    
