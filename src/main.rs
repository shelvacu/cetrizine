#![feature(type_alias_enum_variants)]
#![feature(trace_macros)]
#![deny(unused_must_use)]

extern crate serenity;
extern crate rusqlite as sqlite;
extern crate r2d2_postgres;
extern crate postgres as pg;
extern crate chrono;
extern crate time;

use sqlite::types::{ToSql,ToSqlOutput,FromSql,FromSqlResult,ValueRef};
use sqlite::{Connection, NO_PARAMS};
use sqlite::OptionalExtension;

use std::sync::{Mutex,Arc};
use std::env;

mod legacy;

const DISCORD_MAX_SNOWFLAKE:u64 = 9223372036854775807;

use serenity::{
    model::{gateway::Ready, channel::Message, channel::Channel},
    model::prelude::*,
    //model::id::*,
    prelude::*,
};

struct Handler{
    //TODO: I'm pretty sure serenity multithreads by default, this should really be a connection pool.
    conn: Mutex<Connection>,
    currently_archiving: Arc<Mutex<()>>,
}

pub trait EnumIntoString : Sized {
    fn into_str(&self) -> &'static str;
    fn from_str<'a>(input: &'a str) -> Option<Self>;
}

const AUTO_RETRY_DELAYS:&[u64] = &[0,1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536];

trait ConnectionExtension {
    fn execute_auto_retry(&self, sql: &str, params: &[&ToSql]) -> sqlite::Result<usize>;
}

struct NullEscape<T>(pub T);

impl ToSql for NullEscape<String>{
    fn to_sql(&self) -> sqlite::Result<ToSqlOutput> {
        if self.0.chars().all(|c| c != '\0' && c != '$') {
            self.0.to_sql()
        } else {
            let mut res = String::with_capacity(self.0.len() + 4);
            for c in self.0.chars() {
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
            Ok(sqlite::types::ToSqlOutput::Owned(res.into()))
        }
    }
}

/*impl ToSql for NullEscape<Option<String>> {
    fn to_sql(&self) -> sqlite::Result<ToSqlOutput> {
        match &self.0 {
            None => Ok(sqlite::types::ToSqlOutput::Owned(sqlite::types::Value::Null)),
            Some(s) => NullEscape(s.clone()).to_sql(),
        }
    }
}*/

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

trait FilterExt {
    type Return;
    fn filter_null(&self) -> Self::Return;
}

impl FilterExt for str {
    type Return = NullEscape<String>;
    fn filter_null(&self) -> Self::Return {
        NullEscape(self.into())
    }
}

/*impl<T: FilterExt> FilterExt for Option<T> {
    type Return = Option<NullEscape<String>>;
    fn filter_null(&self) -> Self::Return {
        self.map(|t| t.filter_null())
    }
}*/

impl FilterExt for Option<&str> {
    type Return = Option<NullEscape<String>>;
    fn filter_null(&self) -> Self::Return {
        self.map(FilterExt::filter_null)
    }
}

impl FilterExt for Option<String> {
    type Return = Option<NullEscape<String>>;
    fn filter_null(&self) -> Self::Return {
        self.clone().map(|s| s.filter_null())
    }
}

impl FilterExt for Option<Option<String>> {
    type Return = Option<NullEscape<String>>;
    fn filter_null(&self) -> Self::Return {
        self.clone().collapse().filter_null()
    }
}

/*impl FilterExt for String {
    fn filter(&self) -> NullEscape {
        NullEscape(self.clone())
    }
}*/

impl ConnectionExtension for sqlite::Connection {
    fn execute_auto_retry(&self, sql: &str, params: &[&ToSql]) -> sqlite::Result<usize> {
        let mut tries = 0;
        let mut res;
        loop {
            res = self.execute(sql, params);
            match res {
                Err(sqlite::Error::SqliteFailure(err, _)) if err.code == sqlite::ffi::ErrorCode::DatabaseBusy => {
                    if tries >= AUTO_RETRY_DELAYS.len() { break }
                    let wait_time = AUTO_RETRY_DELAYS[tries];
                    eprintln!("Database busy, waiting {}ms", wait_time);
                    std::thread::sleep(std::time::Duration::from_millis(wait_time));
                    tries += 1;
                    
                },
                res => return res,
            }
        }
        return res
    }
}

macro_rules! print_any_error {
    ( $e:expr ) => {{
        match $e {
            Ok(_) => (),
            Err(e) => eprintln!("ERROR in {}:{} {:?}",file!(),line!(),e),
        }
    }};
}

macro_rules! insert_helper {
    ($db:ident, $table_name:expr, $( $column_name:expr => $column_value:expr , )+ ) => {{
        let table_name = $table_name;
        let values:&[&ToSql] = &[
            $(
                &$column_value as &ToSql,
            )+
        ];

        let sql_column_names = &[
            $( $column_name, )+
        ];

        let placeholders:Vec<&str> = sql_column_names.into_iter().map(|_| "?").collect();

        let sql = &[
            "INSERT INTO ", table_name,
            " (", &sql_column_names.join(","), ") ",
            "VALUES (", &placeholders.join(","), ") ",
        ].join("");

        $db.execute_auto_retry(sql, values)
    }};
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

#[derive(Debug)]
pub enum CetrizineError<'a>{
    Mutex(std::sync::PoisonError<std::sync::MutexGuard<'a,Connection>>),
    Sql(sqlite::Error),
    Serenity(serenity::Error),
}

impl<'a> std::fmt::Display for CetrizineError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        //TODO: Should maybe do this differently, probably delegate to sub-errors' fmt()?
        write!(f, "{:?}", self)
    }
}

impl<'a> std::error::Error for CetrizineError<'a> {
    fn description(&self) -> &str {
        use CetrizineError::*;
        //TODO: Include sub-errors' description? Does that happen already?
        match self {
            Mutex(_)    => "Mutex poisoned",
            Sql(_)      => "SQLite Error",
            Serenity(_) => "Serenity Error",
        }
    }

    fn cause(&self) -> Option<&std::error::Error> {
        use CetrizineError::*;
        match self {
            Mutex(e)    => Some(e),
            Sql(e)      => Some(e),
            Serenity(e) => Some(e),
        }
    }
}

impl<'a> From<std::sync::PoisonError<std::sync::MutexGuard<'a,Connection>>> for CetrizineError<'a>{
    fn from(err: std::sync::PoisonError<std::sync::MutexGuard<'a,Connection>>) -> Self {
        CetrizineError::Mutex(err)
    }
}

impl<'a> From<sqlite::Error> for CetrizineError<'a> {
    fn from(err: sqlite::Error) -> Self {
        CetrizineError::Sql(err)
    }
}

impl<'a> From<serenity::Error> for CetrizineError<'a> {
    fn from(err: serenity::Error) -> Self {
        CetrizineError::Serenity(err)
    }
}

pub struct DumbHax<T>(pub T);

impl<T: Clone + Snowflake> ToSql for DumbHax<T>{
    fn to_sql(&self) -> sqlite::Result<ToSqlOutput> {
        let val:u64 = self.0.clone().get_snowflake();
        Ok(ToSqlOutput::from(format!("{}",val)))
    }
}

impl<T: Clone + Snowflake> ToSql for DumbHax<Option<T>>{
    fn to_sql(&self) -> sqlite::Result<ToSqlOutput> {
        match &self.0 {
            None => Ok(sqlite::types::ToSqlOutput::Owned(sqlite::types::Value::Null)),
            Some(v) => Ok(ToSqlOutput::from(format!("{}", v.clone().get_snowflake()))),//DumbHax(v).to_sql(),
        }
    }
}

pub trait Snowflake {
    fn get_snowflake(&self) -> u64;

    fn get_snowflake_i64(&self) -> i64 {
        self.get_snowflake() as i64
    }
}

macro_rules! snowflake_impl {
    ($klass:ty) => {
        impl Snowflake for $klass {
            fn get_snowflake(&self) -> u64 {
                self.0
            }
        }
    };
}

snowflake_impl!{ApplicationId}
snowflake_impl!{AuditLogEntryId}
snowflake_impl!{ChannelId}
snowflake_impl!{EmojiId}
snowflake_impl!{GuildId}
snowflake_impl!{IntegrationId}
snowflake_impl!{MessageId}
snowflake_impl!{RoleId}
snowflake_impl!{UserId}
snowflake_impl!{WebhookId}

pub struct SmartHax<T>(pub T);

impl<T: Snowflake> ToSql for SmartHax<T>{
    fn to_sql(&self) -> sqlite::Result<ToSqlOutput> {
        Ok(ToSqlOutput::Owned(sqlite::types::Value::Integer(self.0.get_snowflake_i64())))
    }
}

impl<U: Snowflake + Copy> ToSql for SmartHax<Option<U>>{
    fn to_sql(&self) -> sqlite::Result<ToSqlOutput> {
        let val = match self.0 {
            Some(v) => sqlite::types::Value::Integer(v.get_snowflake_i64()),
            None => sqlite::types::Value::Null,
        };
        Ok(ToSqlOutput::Owned(val))
    }
}

pub struct PermsToSql(pub serenity::model::permissions::Permissions);

impl ToSql for PermsToSql{
    fn to_sql(&self) -> sqlite::Result<ToSqlOutput> {
        let val_u:u64 = self.0.bits();
        //TODO: Do I *really* need unsafe here? Is this platform-independent? (probably not) (also see below)
        let val_i:i64 = unsafe { std::mem::transmute(val_u) };
        Ok(ToSqlOutput::Owned(sqlite::types::Value::Integer(val_i)))
    }
}

impl FromSql for PermsToSql{
    fn column_result(value: ValueRef) -> FromSqlResult<Self> {
        i64::column_result(value).map(|val_i| {
            //TODO: see above
            let val_u:u64 = unsafe { std::mem::transmute(val_i) };
            Self(serenity::model::permissions::Permissions::from_bits_truncate(val_u))
        })
    }
}

fn make_arr<T: Clone + Snowflake>(tx: &sqlite::Transaction, arr: &[T]) -> Result<i64, sqlite::Error> {
    tx.execute(
        "INSERT INTO id_arr (row_id) VALUES (null)",
        NO_PARAMS
    )?;

    let id = tx.last_insert_rowid();

    for item in arr {
        insert_helper!(
            tx, "id",
            "id_arr_rowid" => id,
            "id" => DumbHax(item.clone()),
        )?;
    }

    return Ok(id)
}

impl Handler {
    fn archive_message<'a>(tx: &sqlite::Transaction, msg: &Message, recvd_at:chrono::DateTime<chrono::offset::Local>) -> Result<(), CetrizineError<'a>> {
        let memb = &msg.member;

        //let member_roles_arr_id = memb.map(|m| make_arr(tx, &m.roles)?); <-- This doesn't work because the '?' is inside the closure and thus the closure's return type would be a Result<...> which would make member_roles_arr_id a Option<Result<...>> which is no bueno, so I did the below instead
        let member_roles_arr_id = match memb {
            Some(m) => Some(make_arr(tx, &m.roles)?),
            None => None,
        };
        
        let mention_roles_arr_id = make_arr(tx, &msg.mention_roles)?;

        let mut filtered_content = msg.content.clone();
        filtered_content.retain(|c| c != '\0');
        let is_filtered = filtered_content != msg.content;
        insert_helper!(
            tx, "message",
            "discord_id" => DumbHax(msg.id),
            "author_id" => DumbHax(msg.author.id),
            "author_avatar" => msg.author.avatar.filter_null(),
            "author_is_bot" => msg.author.bot,
            "author_discriminator" => msg.author.discriminator,
            "author_name" => msg.author.name.filter_null(),
            "channel_id" => DumbHax(msg.channel_id),
            "content" => filtered_content,
            "content_binary" => if is_filtered { Some(msg.content.as_bytes()) } else { None },
            "edited_timestamp" => msg.edited_timestamp,
            "guild_id" => DumbHax(msg.guild_id),
            "kind" => msg.kind.into_str(),
            "member_is_some" => memb.is_some(),
            "member_deaf" => memb.clone().map(|m| m.deaf),
            "member_joined_at" => memb.clone().map(|m| m.joined_at),
            "member_mute" => memb.clone().map(|m| m.mute),
            "member_roles" => member_roles_arr_id,
            "mention_everyone" => msg.mention_everyone,
            "mention_roles" => mention_roles_arr_id,
            "nonce_debug" => format!("{:?}",msg.nonce), //TODO: should probably file a bug and/or pull request with serenity about this one
            "pinned" => msg.pinned,
            "timestamp" => msg.timestamp,
            "tts" => msg.tts,
            "webhook_id" => DumbHax(msg.webhook_id),
            "archive_recvd_at" => recvd_at,
        )?;

        let message_rowid = tx.last_insert_rowid();

        //attachments
        for attachment in &msg.attachments {
            insert_helper!(
                tx, "attachment",
                "message_rowid" => message_rowid,
                "discord_id" => attachment.id.parse::<i64>().expect("could not parse attachment discord id"), //Yes, attachment.id is a String when all the other .id's are a u64 wrapper
                "filename" => attachment.filename.filter_null(),
                "height" => attachment.height.map(|h| h as i64),
                "width" => attachment.width.map(|w| w as i64),
                "proxy_url" => attachment.proxy_url,
                "size" => (attachment.size as i64),
                "url" => attachment.url.filter_null(),
            )?;
        }

        //embeds
        for embed in &msg.embeds {
            insert_helper!(
                tx, "embed",
                "message_rowid" => message_rowid,
                "author_is_some" => embed.author.is_some(),
                "author_icon_url" => embed.author.clone().map(|a| a.icon_url).filter_null(),
                "author_name" => embed.author.clone().map(|a| a.name).filter_null(),
                "author_proxy_icon_url" => embed.author.clone().map(|a| a.proxy_icon_url).filter_null(),
                "author_url" => embed.author.clone().map(|a| a.url).filter_null(),
                "colour_u32" => embed.colour.0,
                "description" => embed.description.filter_null(),
                "footer_is_some" => embed.footer.is_some(),
                "footer_icon_url" => embed.footer.clone().map(|f| f.icon_url).filter_null(),
                "footer_proxy_icon_url" => embed.footer.clone().map(|f| f.proxy_icon_url).filter_null(),
                "footer_text" => embed.footer.clone().map(|f| f.text).filter_null(),
                "image_is_some" => embed.image.is_some(),
                "image_height" => embed.image.clone().map(|i| i.height as i64),
                "image_width" => embed.image.clone().map(|i| i.width as i64),
                "image_proxy_url" => embed.image.clone().map(|i| i.proxy_url).filter_null(),
                "image_url" => embed.image.clone().map(|i| i.url).filter_null(),
                "kind" => embed.kind,
                "provider_is_some" => embed.provider.is_some(),
                "provider_name" => embed.provider.clone().map(|p| p.name).filter_null(),
                "provider_url" => embed.provider.clone().map(|p| p.url).filter_null(),
                "thumbnail_is_some" => embed.thumbnail.is_some(),
                "thumbnail_height" => embed.thumbnail.clone().map(|t| t.height as i64),
                "thumbnail_width" => embed.thumbnail.clone().map(|t| t.width as i64),
                "thumbnail_url" => embed.thumbnail.clone().map(|t| t.url).filter_null(),
                "thumbnail_proxy_url" => embed.thumbnail.clone().map(|t| t.proxy_url).filter_null(),
                "timestamp" => embed.timestamp,
                "title" => embed.title.filter_null(),
                "url" => embed.url.filter_null(),
                "video_is_some" => embed.video.is_some(),
                "video_height" => embed.video.clone().map(|v| v.height as i64),
                "video_width" => embed.video.clone().map(|v| v.width as i64),
                "video_url" => embed.video.clone().map(|v| v.url).filter_null(),
            )?;

            let embed_rowid = tx.last_insert_rowid();

            //insert embed fields
            for embed_field in &embed.fields {
                insert_helper!(
                    tx, "embed_field",
                    "embed_rowid" => embed_rowid,
                    "inline" => embed_field.inline,
                    "name"  => embed_field.name.filter_null(),
                    "value" => embed_field.value.filter_null(),
                )?;
            }
        }

        //mentions
        for user in &msg.mentions {
            insert_helper!(
                tx, "user_mention",
                "message_rowid" => message_rowid,
                "id" => SmartHax(user.id),
                "avatar" => user.avatar.filter_null(),
                "bot" => user.bot,
                "discriminator" => user.discriminator,
                "name" => user.name.filter_null(),
            )?;
        }

        //reactions
        for reaction in &msg.reactions {
            let (is_custom, animated, id, name, string) = match reaction.reaction_type.clone() {
                serenity::model::channel::ReactionType::Custom{animated, id, name} =>
                    (true, Some(animated), Some(id), name, None),
                serenity::model::channel::ReactionType::Unicode(s) =>
                    (false, None, None, None, Some(s)),
            };
            
            insert_helper!(
                tx, "reaction",
                "message_rowid" => message_rowid,
                "count" => (reaction.count as i64),
                "me" => reaction.me,
                "reaction_is_custom" => is_custom,
                
                "reaction_animated" => animated,
                "reaction_id" => SmartHax(id.map(|i| i.clone())),
                "reaction_name" => name.filter_null(),
                
                "reaction_string" => string.filter_null(),
            )?;
        }
        
        Ok(())
    }

    fn grab_channel_archive<'a>(conn: &mut sqlite::Connection, chan: &Channel, guild_name: String) -> Result<(), CetrizineError<'a>> {
        let name = get_name(chan);
        println!("ARCHIVING CHAN {} (id {})", &name, &chan.id());
        let mut got_messages = false;

        let last_msg_id = match get_last_message_id(chan) {
            Some(id) => id,
            None => return Ok(()),
        };

        //println!("DEBUG: selecting maybe_res WHERE start >= {} >= end AND chan = {}",last_msg_id,chan.id());
        let mut maybe_res:Option<(i64,u64,Option<u64>,bool)> = conn.query_row("
SELECT
  rowid,
  end_message_id,
  after_message_id,
  (message_count_received < message_count_requested) 
FROM message_archive_gets 
WHERE 
  (
    (
      start_message_id >= ?1 AND ?1 >= end_message_id
    ) OR 
    around_message_id = ?1
  ) 
  AND 
  channel_id = ?2
ORDER BY end_message_id ASC
LIMIT 1;",
            &[&SmartHax(last_msg_id) as &ToSql, &SmartHax(chan.id())],
            |r| Ok((r.get(0)?,r.get::<_,i64>(1)? as u64,r.get::<_,Option<i64>>(2)?.map(|i| i as u64),r.get(3)?))
        ).optional()?;
        //println!("maybe_res is {:?}",maybe_res);
        //std::process::exit(1);
        let mut get_before = DISCORD_MAX_SNOWFLAKE;
        //let before_id:u64 = last_msg_id.into();
        let mut before_message_id:Option<u64>;// = Some(before_id.to_string());
        
        while let Some(res) = maybe_res {
            get_before = res.1;//.get::<_,String>(0).parse::<u64>().unwrap(); //49
            before_message_id = res.2;//.get::<_,Option<String>>(1); //null
            if res.3 {
                return Ok(());
            }
            //println!("DEBUG: selecting maybe_res again get_before {} before_message_id {:?} res.0 {}",get_before,before_message_id,res.0);
            maybe_res = conn.query_row(
                "SELECT rowid,end_message_id,after_message_id,(message_count_received < message_count_requested) FROM message_archive_gets WHERE (
  ( start_message_id >= ?1 AND ?1 > end_message_id ) OR
  before_message_id = ?1 OR
  (?2 NOT NULL AND end_message_id = ?2)
) AND channel_id = ?3 AND rowid != ?4
ORDER BY start_message_id ASC LIMIT 1;",
                &[&(get_before as i64) as &ToSql,&(before_message_id.map(|u| u as i64)),&SmartHax(chan.id()),&res.0],
                |r| Ok((r.get(0)?,r.get::<_,i64>(1)? as u64,r.get::<_,Option<i64>>(2)?.map(|i| i as u64),r.get(3)?))
            ).optional()?;
            //println!("maybe_res again is {:?}",maybe_res);
        }

        //we're "in" a gap, lets find where this gap ends.

        println!("DEBUG: selecting gap_end where end < {} and chan = {}",get_before,chan.id());
        let gap_end = conn.query_row(
            "SELECT start_message_id FROM message_archive_gets WHERE end_message_id < ?1 AND channel_id = ?2 ORDER BY start_message_id DESC LIMIT 1;",
            &[&(get_before as i64) as &ToSql, &SmartHax(chan.id())],
            |r| r.get(0)
        ).optional()?.unwrap_or(0i64);
        //println!("DEBUG: gap_end is {:?}",gap_end);
        let mut earliest_message_recvd = get_before;//DISCORD_MAX_SNOWFLAKE;

        while earliest_message_recvd > (gap_end as u64) {
            let asking_for = 100u8;
            
            let mut msgs;
            let around;
            if earliest_message_recvd == DISCORD_MAX_SNOWFLAKE {
                //println!("asking for around {}", last_msg_id);
                around = true;
                msgs = chan.id().messages(|r| r.limit(asking_for.into()).around(last_msg_id))?;
            }else{
                //println!("asking for before {}",earliest_message_recvd);
                around = false;
                msgs = chan.id().messages(|r| r.limit(asking_for.into()).before(earliest_message_recvd))?;
            }
            let recvd_at = chrono::offset::Local::now();
            //messages seem to usually be ordered, largest ids first
            //however, that's documented literally nowhere, so lets sort them
            msgs.sort_unstable_by_key(|msg| {
                let msg_id:u64 = msg.id.into();
                std::u64::MAX - msg_id
            });
            let tx = conn.transaction()?;
            for msg in &msgs {
                //println!("Archiving Message id {:?}", msg.id);
                Self::archive_message(&tx, msg, recvd_at)?;
            }
            if msgs.len() == 0 { break }
            let first_msg = msgs.first().expect("im a bad");
            let last_msg  = msgs.last().expect("im a bad");

            //DEBUG START
            /*let we_have_archived_this_before:bool = tx.query_row(
                "SELECT COUNT(*) FROM message_archive_gets WHERE (start_message_id = ? AND end_message_id = ? AND rowid > 126941)",
                &[&SmartHax(first_msg.id) as &ToSql,&SmartHax(last_msg.id)],
                |r| r.get(0)
            )?;*/
            let already_archived_start:bool = tx.query_row(
                "SELECT COUNT(*) FROM message_archive_gets WHERE (start_message_id >= ?1 AND ?1 >= end_message_id) AND channel_id = ?2 AND rowid > 193737",
                &[&SmartHax(first_msg.id) as &ToSql,&SmartHax(chan.id())],
                |r| r.get(0)
            )?;
            let already_archived_end:bool = tx.query_row(
                "SELECT COUNT(*) FROM message_archive_gets WHERE (start_message_id >= ?1 AND ?1 >= end_message_id) AND channel_id = ?2 AND rowid > 193737",
                &[&SmartHax(last_msg.id) as &ToSql,&SmartHax(chan.id())],
                |r| r.get(0)
            )?;
            let we_have_archived_this_before = already_archived_start && already_archived_end && (!around || {
                tx.query_row(
                    "SELECT COUNT(*) FROM message_archive_gets WHERE (start_message_id >= ?1 AND ?1 >= end_message_id) AND channel_id = ?2 AND rowid > 193737",
                    &[&SmartHax(last_msg_id) as &ToSql,&SmartHax(chan.id())],
                    |r| r.get(0)
            )?
            });
            if we_have_archived_this_before {
                eprintln!(
                    "We've archived this before! chan id {} {} {} start {} end {} in {}#{}",
                    chan.id(),
                    if around {"around"} else {"before"},
                    if around {last_msg_id.0} else {earliest_message_recvd},
                    first_msg.id,
                    last_msg.id,
                    guild_name,
                    name
                );
            }
            //DEBUG END
            
            //TODO: put channel_rowid in there somwhere
            insert_helper!(
                tx, "message_archive_gets",
                "channel_id" => SmartHax(chan.id()),
                "around_message_id" => if around { Some(SmartHax(last_msg_id)) } else { None },
                "before_message_id" => if around { None } else { Some(earliest_message_recvd as i64) },
                "start_message_id" => SmartHax(first_msg.id),
                "end_message_id" => SmartHax(last_msg.id),
                "message_count_requested" => asking_for,
                "message_count_received" => (msgs.len() as i64),
            )?;
            tx.commit()?;
            got_messages = true;
            println!(
                "Archived {} message(s), {} thru {} in {}#{}",
                msgs.len(),
                first_msg.id,
                last_msg.id,
                guild_name,
                name
            );
            earliest_message_recvd = last_msg.id.into();
        } //while earliest_message_recvd > gap_end {
        if got_messages {
            //println!("recursing!");
            return Self::grab_channel_archive(conn, chan, guild_name);
        } else {
            Ok(())
        }
    }
    
    /// This fn will block until it appears that all previous messages have been retrieved. This should probably be run in another thread.
    fn grab_full_archive<'a>(_: Context, rdy: Ready) -> Result<(), CetrizineError<'a>> {
        let func_start = chrono::offset::Local::now();
        let mut conn = legacy::make_sqlite_connection()?;

        let tx = conn.transaction()?;
        insert_helper!(
            tx, "ready",
            "session_id" => rdy.session_id,
            "shard_0" => rdy.shard.clone().map(|a| a[0] as i64),
            "shard_1" => rdy.shard.clone().map(|a| a[1] as i64),
            "trace" => rdy.trace.join(",").filter_null(),
            "user_id" => SmartHax(rdy.user.id),
            "user_avatar" => rdy.user.avatar.filter_null(),
            "user_bot" => rdy.user.bot,
            "user_discriminator" => rdy.user.discriminator,
            "user_email" => rdy.user.email.filter_null(),
            "user_mfa_enabled" => rdy.user.mfa_enabled,
            "user_name" => rdy.user.name.filter_null(),
            "user_verified" => rdy.user.verified,
            "version" => (rdy.version as i64),
        )?;

        let ready_rowid = tx.last_insert_rowid();

        //presences
        for (_,presence) in &rdy.presences {            
            insert_helper!(
                tx, "user_presence",
                "ready_rowid" => ready_rowid,
                "guild_rowid" => (None as Option<i64>),
                "game_is_some" => presence.game.is_some(),
                "game_type" => presence.game.clone().map(|g| g.kind.into_str()),
                "game_name" => presence.game.clone().map(|g| g.name).filter_null(),
                "game_url" => presence.game.clone().map(|g| g.url).filter_null(),
                "last_modified" => presence.last_modified.map(|v| v as i64),
                "nick" => presence.nick.filter_null(),
                "status" => presence.status.into_str(),
                "user_id" => SmartHax(presence.user_id),
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
                    insert_helper!(
                        tx, "group_channel",
                        "discord_id" => SmartHax(group.channel_id),
                        "ready_rowid" => ready_rowid,
                        "icon" => group.icon.filter_null(),
                        "last_message_id" => SmartHax(group.last_message_id),
                        "last_pin_timestamp" => group.last_pin_timestamp,
                        "name" => group.name.filter_null(),
                        "owner_id" => SmartHax(group.owner_id),
                    )?;

                    let group_rowid = tx.last_insert_rowid();

                    for (_, user_lock) in &group.recipients {
                        let user = user_lock.read();
                        insert_helper!(
                            tx, "group_user",
                            "discord_id" => SmartHax(user.id),
                            "group_channel_rowid" => group_rowid,
                            "avatar" => user.avatar.filter_null(),
                            "bot" => user.bot,
                            "discriminator" => user.discriminator,
                            "name" => user.name.filter_null(),
                        )?;
                    }

                    private_channels_to_archive.push(group.channel_id);
                },
                Private(chan_lock) => {
                    let chan = chan_lock.read();
                    let user = chan.recipient.read();
                    insert_helper!(
                        tx, "private_channel",
                        "discord_id" => SmartHax(chan.id),
                        "ready_rowid" => ready_rowid,
                        "last_message_id" => SmartHax(chan.last_message_id),
                        "last_pin_timestamp" => chan.last_pin_timestamp,
                        "kind" => chan.kind.into_str(),
                        "recipient_id" => SmartHax(user.id),
                        "recipient_avatar" => user.avatar.filter_null(),
                        "recipient_bot" => user.bot,
                        "recipient_discriminator" => user.discriminator,
                        "recipient_name" => user.name.filter_null(),
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
                    println!("SKIPPING GUILD, dont have full info needed");
                    continue;
                }
            };
            println!("ARCHIVING GUILD {:?}", &guild);

            let tx = conn.transaction()?;

            insert_helper!(
                tx, "guild",
                "ready_rowid" => ready_rowid, 
                "discord_id" => SmartHax(guild.id), 
                "afk_channel_id" => SmartHax(guild.afk_channel_id), 
                "afk_timeout" => (guild.afk_timeout as i64), 
                "application_id" => SmartHax(guild.application_id), 
                "default_message_notification_level" => guild.default_message_notifications.into_str(),
                "explicit_content_filter" => guild.explicit_content_filter.into_str(),
                "features" => guild.features.join(",").filter_null(),
                "icon" => guild.icon.filter_null(),
                "joined_at" => guild.joined_at,
                "large" => guild.large,
                "member_count" => (guild.member_count as i64),
                "mfa_level" => guild.mfa_level.into_str(),
                "name" => guild.name.filter_null(),
                "owner_id" => SmartHax(guild.owner_id),
                "region" => guild.region.filter_null(),
                "splash" => guild.splash.filter_null(),
                "system_channel_id" => SmartHax(guild.system_channel_id),
                "verification_level" => guild.verification_level.into_str(),
                "archive_recvd_at" => func_start,
            )?;            

            let guild_rowid = tx.last_insert_rowid();

            let mut guild_channels = Vec::<(ChannelId,i64)>::new();

            //channels
            for (_id, chan_a_lock) in &guild.channels {
                let chan = chan_a_lock.read();

                insert_helper!(
                    tx, "guild_channel",
                    "discord_id" => SmartHax(chan.id),
                    "guild_rowid" => guild_rowid,
                    "guild_id" => SmartHax(chan.guild_id),
                    "bitrate" => chan.bitrate.map(|b| b as i64),
                    "category_id" => SmartHax(chan.category_id),
                    "kind" => chan.kind.into_str(),
                    "last_message_id" => SmartHax(chan.last_message_id),
                    "last_pin_timestamp" => chan.last_pin_timestamp,
                    "name" => chan.name.filter_null(),
                    "position" => chan.position,
                    "topic" => chan.topic.filter_null(),
                    "user_limit" => chan.user_limit.map(|u| u as i64),
                    "nsfw" => chan.nsfw,
                )?;

                let chan_rowid = tx.last_insert_rowid();

                guild_channels.push((chan.id.clone(), chan_rowid));

                for overwrite in &chan.permission_overwrites {
                    use serenity::model::channel::PermissionOverwriteType::*;
                    let (ov_type_str, ov_id) = match overwrite.kind {
                        Member(uid) => ("Member", uid.0),
                        Role(rid) => ("Role", rid.0),
                    };

                    insert_helper!(
                        tx, "permission_overwrite",
                        "guild_channel_rowid" => chan_rowid,
                        "allow_bits" => PermsToSql(overwrite.allow),
                        "deny_bits" => PermsToSql(overwrite.deny),
                        "permission_overwrite_type" => ov_type_str,
                        "permission_overwrite_id" => (ov_id as i64),
                    )?;
                }
            }

            //emojis
            for (_, emoji) in &guild.emojis {
                let id_arr_rowid = make_arr(&tx, &emoji.roles)?;

                insert_helper!(
                    tx, "emoji",
                    "discord_id" => SmartHax(emoji.id),
                    "guild_rowid" => guild_rowid,
                    "animated" => emoji.animated,
                    "name" => emoji.name.filter_null(),
                    "managed" => emoji.managed,
                    "require_colons" => emoji.require_colons,
                    "roles" => id_arr_rowid,
                )?;
            }

            //members
            for (_, member) in &guild.members {
                let id_arr_rowid = make_arr(&tx, &member.roles)?;

                let user = member.user.read();

                insert_helper!(
                    tx, "member",
                    "guild_rowid" => guild_rowid,
                    "deaf" => member.deaf,
                    "guild_id" => SmartHax(member.guild_id),
                    "joined_at" => member.joined_at,
                    "mute" => member.mute,
                    "nick" => member.nick.filter_null(),
                    "roles" => id_arr_rowid,
                    "user_id" => SmartHax(user.id),
                    "user_avatar" => user.avatar.filter_null(),
                    "user_is_bot" => user.bot,
                    "user_discriminator" => user.discriminator,
                    "user_name" => user.name.filter_null(),
                )?;

            }

            //presences
            for (_, presence) in &guild.presences {
                insert_helper!(
                    tx, "user_presence",
                    "ready_rowid" => (None as Option<i64>),
                    "guild_rowid" => guild_rowid, //guild_rowid
                    "game_is_some" => presence.game.is_some(), //game_is_some
                    "game_type" => presence.game.clone().map(|g| g.kind.into_str()), //game_type
                    "game_name" => presence.game.clone().map(|g| g.name).filter_null(), //game_name
                    "game_url" => presence.game.clone().map(|g| g.url).filter_null(), //game_url
                    "last_modified" => presence.last_modified.map(|v| v as i64),
                    "nick" => presence.nick.filter_null(),
                    "status" => presence.status.into_str(),
                    "user_id" => SmartHax(presence.user_id),
                )?;
            }

            //roles
            for (_, role) in &guild.roles {

                insert_helper!(
                    tx, "guild_role",
                    "discord_id" => SmartHax(role.id), 
                    "guild_rowid" => guild_rowid, 
                    "colour_u32" => role.colour.0, 
                    "hoist" => role.hoist,
                    "managed" => role.managed,
                    "mentionable" => role.mentionable,
                    "name" => role.name.filter_null(),
                    "permissions_bits" => PermsToSql(role.permissions),
                    "position" => role.position,
                )?;
            }

            //voice_states
            for (_, voice_state) in &guild.voice_states {
                insert_helper!(
                    tx, "voice_state",
                    "guild_rowid" => guild_rowid,
                    "channel_id" => SmartHax(voice_state.channel_id),
                    "deaf" => voice_state.deaf,
                    "mute" => voice_state.mute,
                    "self_deaf" => voice_state.self_deaf,
                    "self_mute" => voice_state.self_mute,
                    "session_id" => voice_state.session_id,
                    "suppress" => voice_state.suppress,
                    "token" => voice_state.token.filter_null(),
                    "user_id" => SmartHax(voice_state.user_id),
                )?;
            }
                               

            tx.commit()?;
            //for (chan_id, chan) in guild.id.channels()? {
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

                let perms = guild.permissions_in(chan_id, rdy.user.id);
                if !perms.read_message_history() {
                    println!("Skipping {}, cannot read message history", chan.name);
                    continue;
                }
                Self::grab_channel_archive(
                    &mut conn,
                    &Channel::Guild(Arc::clone(&cell)),
                    guild.name.clone(),
                )?;
            } // for (chan_id, chan) in guild.id.channels()? {
        } //for guild_status in &rdy.guilds {
        private_channels_to_archive.sort_unstable_by_key(|p| p.0);
        for chan_id in &private_channels_to_archive {
            Self::grab_channel_archive(&mut conn, &chan_id.to_channel_cached().expect("channel absolutely should be cached"),String::from(""))?;
        }
        
        println!("FINISHed archiving old messages");
        Ok(())
    } //fn grab_archive ...
    
    fn message_result(&self, _: Context, msg: Message) -> Result<(), CetrizineError> {
        let handler_start = chrono::prelude::Local::now();
        let mut conn = self.conn.lock()?;
        if let Some(guild_id) = msg.guild_id {
            if let Some(guild) = guild_id.to_guild_cached() {
                println!("GNAME: {}", guild.read().name);
            } else {
                match guild_id.to_partial_guild() {
                    Ok(part_guild) => println!("GNAME: {}", part_guild.name),
                    Err(e) => println!("GNAME_ERR: {:?}", e)
                }
            }
        } else {
            println!("NOGUILD");
        }
        println!("CHAN: {:?}", msg.channel_id.to_channel_cached().map(|c| get_name(&c)));
        println!("DATE: {:?}", handler_start);
        println!("USR: {:?}#{}", msg.author.name, msg.author.discriminator);
        println!("MSG: {:?}", msg.content);

        let tx = conn.transaction()?;
        Self::archive_message(&tx, &msg, handler_start)?;
        tx.commit()?;

        println!();
        Ok(())
    }

    fn _channel_create(&self, _ctx: Context, channel_lock: Arc<RwLock<GuildChannel>>) -> Result<(), CetrizineError> {
        let recvd_at = chrono::offset::Local::now();
        let mut conn = self.conn.lock()?;
        let chan = channel_lock.read();
        println!("Chan create! {:?}", chan.name);
        let tx = conn.transaction()?;
        insert_helper!(
            tx, "channel_create_event",
            "recvd_at" => recvd_at,
        )?;

        let cce_rowid = tx.last_insert_rowid();

        insert_helper!(
            tx, "guild_channel",
            "discord_id" => SmartHax(chan.id),
            "channel_create_event_rowid" => cce_rowid,
            "guild_id" => SmartHax(chan.guild_id),
            "bitrate" => chan.bitrate.map(|b| b as i64),
            "category_id" => SmartHax(chan.category_id),
            "kind" => chan.kind.into_str(),
            "last_message_id" => SmartHax(chan.last_message_id),
            "last_pin_timestamp" => chan.last_pin_timestamp,
            "name" => chan.name.filter_null(),
            "position" => chan.position,
            "topic" => chan.topic.filter_null(),
            "user_limit" => chan.user_limit.map(|u| u as i64),
            "nsfw" => chan.nsfw,
        )?;
        tx.commit()?;

        let guild_name:String = match chan.guild_id.to_guild_cached() {
            Some(guild_lock) => guild_lock.read().name.clone(),
            None => "NOT IN CACHE".into(),
        };
        let mut threads_conn = legacy::make_sqlite_connection()?;
        std::mem::drop(chan);
        let threads_chan = Channel::Guild(channel_lock);
        std::thread::spawn(move || {
            print_any_error!(Self::grab_channel_archive(&mut threads_conn, &threads_chan, guild_name));
        });
        Ok(())
    }   
}   

impl EventHandler for Handler {
    fn ready(&self, ctx: Context, ready: Ready) {
        //TODO: Discord can send multiple readys! What the fuck discord!?
        println!("READY RCVD");
        let conn = self.conn.lock().unwrap();
        let count_that_should_be_zero:i64 = conn.query_row(
            "SELECT COUNT(*) FROM ready WHERE user_id != ?",
            &[&SmartHax(ready.user.id) as &ToSql], |r| r.get(0)
        ).unwrap();

        if count_that_should_be_zero != 0 {
            panic!("Error: you should always log in with the same user for the same database! Found ready with differing user id (current user is id {} aka {}#{})",ready.user.id.0,ready.user.name,ready.user.discriminator);
        }

        let archiving_mutex = Arc::clone(&self.currently_archiving);
        std::thread::spawn(move || {
            let _lock = match archiving_mutex.try_lock() {
                Ok(v) => v,
                Err(_) => {
                    eprintln!("WARN: received ready when already started archiving, ignoring");
                    return
                },
            };
            let res = Self::grab_full_archive(ctx, ready);
            if let Err(CetrizineError::Serenity(serenity::Error::Http(serenity::prelude::HttpError::UnsuccessfulRequest(mut http_response)))) = res {
                eprintln!("http response: {:?}", http_response);
                let mut body = String::new();
                std::io::Read::read_to_string(&mut http_response, &mut body).expect("could not read http body");
                eprintln!("body: {:?}", body);
            }else{ res.expect("unable to grab archive") }
        });
    }

    fn message(&self, ctx: Context, msg: Message) {
        //if true { return }
        print_any_error!(self.message_result(ctx, msg));
    }

    fn channel_create(&self, ctx: Context, channel: Arc<RwLock<GuildChannel>>) {
        print_any_error!(self._channel_create(ctx, channel));
    }
}

fn main() {
    legacy::migrate_sqlite_to_postgres();
    println!("FINISH");
    println!();
    println!();
    println!();
    println!();
    println!();
    panic!();
    let token = env::var("DISCORD_TOKEN")
        .or_else(|_| std::fs::read_to_string("../discord.token"))
        .expect("Expected a token in the environment")
        .to_owned();
    
    /*let handler = Handler{conn: Mutex::new(conn), currently_archiving: Arc::new(Mutex::new(()))};
    let mut client = Client::new(&token, handler).expect("Err creating client");

    if let Err(why) = client.start() {
        eprintln!("Client error: {:?}", why);
    }*/
}
