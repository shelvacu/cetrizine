#![feature(type_alias_enum_variants)]
#![feature(trace_macros)]
#![feature(nll)]
#![feature(const_slice_len)]
#![deny(unused_must_use)]

#[macro_use]
extern crate log;
extern crate serenity;
extern crate rusqlite as sqlite;
extern crate r2d2_postgres;
#[macro_use]
extern crate postgres as pg;
#[macro_use]
extern crate postgres_derive;
extern crate chrono;
extern crate time;
extern crate pbr;
extern crate backtrace;
extern crate simplelog;
extern crate multi_log;

use backtrace::Backtrace;

use r2d2_postgres::r2d2;

use pg::types::{IsNull,Type,ToSql,FromSql};

use chrono::prelude::{DateTime,Utc};

use serenity::{
    model::{gateway::Ready, channel::Message, channel::Channel},
    model::prelude::*,
    prelude::*,
};

use std::sync::{Mutex,Arc};
use std::fmt::Debug;
use std::error::Error;

macro_rules! pg_insert_helper {
    ($db:ident, $table_name:expr, $( $column_name:ident => $column_value:expr , )+ ) => {{
        let table_name = $table_name;
        let debug:bool = false && table_name == "message";
        let values:&[&pg::types::ToSql] = &[
            $(
                &$column_value as &pg::types::ToSql,
            )+
        ];

        let pg_column_names = &[
            $( stringify!($column_name), )+
        ];

        //if debug { dbg!(pg_column_names); }

        let pg_parameters = (0..pg_column_names.len()).into_iter().map(|j| format!("${}",j+1)).collect::<Vec<String>>().join(","); //makes a string like "$1,$2,$3" if pg_column_names.len() == 3


        let sql = &[
            "INSERT INTO ", table_name,
            " (", &pg_column_names.join(","), ") ",
            "VALUES (", &pg_parameters, ") ",
        ].join("");

        if debug { dbg!(sql); }
        if debug { dbg!(values); }
        
        ( || -> pg::Result<()>{
            assert_eq!($db.prepare_cached(sql)?.execute(values)?,1);
            Ok(())
        } )()
    }};
}

mod legacy; //this *must* come after the pg_insert_helper macro
mod db_types;
mod migrations;
mod postgres_logger;

use db_types::*;

const DISCORD_MAX_SNOWFLAKE:u64 = 9223372036854775807;

struct Handler{
    pool: r2d2::Pool<r2d2_postgres::PostgresConnectionManager>,
    currently_archiving: Arc<Mutex<()>>,
    beginning_of_time: std::time::Instant,
    started_at: chrono::DateTime<chrono::Utc>,
    session_id: i64,
}

pub trait EnumIntoString : Sized {
    fn into_str(&self) -> &'static str;
    fn from_str<'a>(input: &'a str) -> Option<Self>;
}

//const AUTO_RETRY_DELAYS:&[u64] = &[0,1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536];

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

/*impl<T: FilterExt> FilterExt for Option<T> {
    type Return = Option<NullEscape<String>>;
    fn filter_null(&self) -> Self::Return {
        self.map(|t| t.filter_null())
    }
}*/

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

macro_rules! print_any_error {
    ( $e:expr ) => {{
        match $e {
            Ok(_) => (),
            Err(e) => eprintln!("ERROR in {}:{} {:?}",file!(),line!(),e),
        }
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
enum_stringify!{ log::Level => Error, Warn, Info, Debug, Trace }

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
    //Mutex(std::sync::PoisonError<std::sync::MutexGuard<'a,Connection>>),
    Pool(r2d2::Error),
    Sql(pg::Error),
    Serenity(serenity::Error),
}

impl std::fmt::Display for CetrizineError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        //TODO: Should maybe do this differently, probably delegate to sub-errors' fmt()?
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for CetrizineError {
    fn description(&self) -> &str {
        use CetrizineErrorType::*;
        //TODO: Include sub-errors' description? Does that happen already?
        match self.error_type {
            //Mutex(_)    => "Mutex poisoned",
            Pool(_)     => "Pool Error",
            Sql(_)      => "SQLite Error",
            Serenity(_) => "Serenity Error",
        }
    }

    fn cause(&self) -> Option<&std::error::Error> {
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

        impl Snowflake for &$klass {
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

#[derive(Debug)]
pub struct SmartHax<T: Debug>(pub T);

impl<T: Snowflake + Debug> ToSql for SmartHax<T>{
    fn to_sql(&self,
              ty: &Type,
              out: &mut Vec<u8>) -> Result<IsNull, Box<dyn Error + 'static + Sync + Send>> {
        DbSnowflake::from(self.0.get_snowflake_i64()).to_sql(ty, out)
    }

    fn accepts(ty: &Type) -> bool {
        <DbSnowflake as ToSql>::accepts(ty)
    }

    to_sql_checked!{}
}

impl<U: Snowflake + Debug> ToSql for SmartHax<Option<U>>{
    fn to_sql(&self,
              ty: &Type,
              out: &mut Vec<u8>) -> Result<IsNull, Box<dyn Error + 'static + Sync + Send>> {
        self.0.as_ref().map(|v| DbSnowflake::from(Snowflake::get_snowflake_i64(v))).to_sql(ty, out)
    }

    fn accepts(ty: &Type) -> bool {
        <Option<DbSnowflake> as ToSql>::accepts(ty)
    }

    to_sql_checked!{}
}

#[derive(Debug)]
pub struct PermsToSql(pub serenity::model::permissions::Permissions);

impl ToSql for PermsToSql{
    fn to_sql(&self,
              ty: &Type,
              out: &mut Vec<u8>) -> Result<IsNull, Box<dyn Error + 'static + Sync + Send>> {
        let val_u:u64 = self.0.bits();
        //TODO: Do I *really* need unsafe here? Is this platform-independent? (probably not) (also see below)
        let val_i:i64 = unsafe { std::mem::transmute(val_u) };
        val_i.to_sql(ty, out)
    }

    fn accepts(ty: &Type) -> bool {
        <i64 as ToSql>::accepts(ty)
    }

    to_sql_checked!{}
}

impl FromSql for PermsToSql{
    fn from_sql(ty: &Type,
                raw: &[u8]) -> Result<Self, Box<dyn Error + 'static + Sync + Send>> {
        let val_i = <i64 as FromSql>::from_sql(ty, raw)?;
        let val_u:u64 = unsafe { std::mem::transmute(val_i) };
        Ok(Self(serenity::model::permissions::Permissions::from_bits_truncate(val_u)))
    }

    fn accepts(ty: &pg::types::Type) -> bool {
        <i64 as FromSql>::accepts(ty)
    }
}

impl Handler {
    fn archive_message(tx: &pg::transaction::Transaction, msg: &Message, recvd_at:DateTime<Utc>) -> Result<(), CetrizineError> {
        //let memb = &msg.member;

        //let member_roles_arr_id = memb.map(|m| make_arr(tx, &m.roles)?); <-- This doesn't work because the '?' is inside the closure and thus the closure's return type would be a Result<...> which would make member_roles_arr_id a Option<Result<...>> which is no bueno, so I did the below instead
        /*let member_roles_arr_id = match memb {
            Some(m) => Some(make_arr(tx, &m.roles)?),
            None => None,
        };
        
        let mention_roles_arr_id = make_arr(tx, &msg.mention_roles)?;*/

        /*let mut filtered_content = msg.content.clone();
        filtered_content.retain(|c| c != '\0');
        let is_filtered = filtered_content != msg.content;*/
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

        //let message_rowid:i64 = tx.query("SELECT currval(pg_get_serial_sequence('message','rowid'));",&[])?.get(0).unwrap().get(0);
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

            //let embed_rowid = tx.last_insert_rowid();
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
        println!("ARCHIVING CHAN {}#{} (id {})", &guild_name, &name, &chan.id());
        let mut got_messages = false;

        let last_msg_id = match get_last_message_id(chan) {
            Some(id) => id,
            None => return Ok(()),
        };

        //println!("DEBUG: selecting maybe_res WHERE start >= {} >= end AND chan = {}",last_msg_id,chan.id());
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
            &[&(last_msg_id.get_snowflake_i64()) as &ToSql, &(chan.id().get_snowflake_i64())]
        )?;
        
        let mut maybe_res:Option<(i64,u64,Option<u64>,bool)> =
            if rows.len() == 0 {
                None
            } else if rows.len() == 1 {
                let r = rows.get(0);
                Some((r.get(0),r.get::<_,i64>(1) as u64,r.get::<_,Option<i64>>(2).map(|i| i as u64),r.get(3)))
            } else { panic!() };
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
            let rows = conn.query(
                "SELECT rowid,end_message_id,after_message_id,(message_count_received < message_count_requested) FROM message_archive_gets WHERE (
  ( start_message_id >= $1 AND $1 > end_message_id ) OR
  before_message_id = $1 OR
  ($2::int8 IS NOT NULL AND end_message_id = $2::int8)
) AND channel_id = $3 AND rowid != $4
ORDER BY start_message_id ASC LIMIT 1;",
                &[&(get_before as i64) as &ToSql,&(before_message_id.map(|u| u as i64)),&(chan.id().get_snowflake_i64()),&res.0]
            )?;
            maybe_res =
                if rows.len() == 0 {
                    None
                } else if rows.len() == 1 {
                    let r = rows.get(0);
                    Some((r.get(0),r.get::<_,i64>(1) as u64,r.get::<_,Option<i64>>(2).map(|i| i as u64),r.get(3)))
                } else { panic!() };
            //println!("maybe_res again is {:?}",maybe_res);
        }

        //we're "in" a gap, lets find where this gap ends.

        //println!("DEBUG: selecting gap_end where end < {} and chan = {}",get_before,chan.id());
        let rows = conn.query(
            "SELECT start_message_id FROM message_archive_gets WHERE end_message_id < $1 AND channel_id = $2 ORDER BY start_message_id DESC LIMIT 1;",
            &[&(get_before as i64) as &ToSql, &(chan.id().get_snowflake_i64())],
        )?;
        let gap_end =
            if rows.len() == 0 {
                0i64
            } else if rows.len() == 1 {
                rows.get(0).get(0)
            } else { panic!() };

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
            let recvd_at = Utc::now();
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
                "SELECT COUNT(*) FROM message_archive_gets WHERE (start_message_id = $1 AND end_message_id = $2 AND rowid > 126941)",
                &[&SmartHax(first_msg.id) as &ToSql,&SmartHax(last_msg.id)],
                |r| r.get(0)
            )?;*/
            let already_archived_start:bool = tx.query(
                "SELECT COUNT(*) FROM message_archive_gets WHERE (start_message_id >= $1 AND $1 >= end_message_id) AND channel_id = $2 AND rowid > 193737",
                &[&(first_msg.id.get_snowflake_i64()) as &ToSql,&(chan.id().get_snowflake_i64())],
            )?.get(0).get::<_,i64>(0) > 0;
            let already_archived_end:bool = tx.query(
                "SELECT COUNT(*) FROM message_archive_gets WHERE (start_message_id >= $1 AND $1 >= end_message_id) AND channel_id = $2 AND rowid > 193737",
                &[&(last_msg.id.get_snowflake_i64()) as &ToSql,&(chan.id().get_snowflake_i64())],
            )?.get(0).get::<_,i64>(0) > 0;
            let we_have_archived_this_before = already_archived_start && already_archived_end && (!around /*|| {
                tx.query(
                    "SELECT COUNT(*) FROM message_archive_gets WHERE (start_message_id >= $1 AND $1 >= end_message_id) AND channel_id = $2 AND rowid > 193737",
                    &[&(last_msg_id.get_snowflake_i64()) as &ToSql,&(chan.id().get_snowflake_i64())],
                )?.get(0).get::<_,i64>(0) > 0
            }*/);
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
    fn grab_full_archive<C: pg::GenericConnection>(conn: &C, _: Context, rdy: Ready) -> Result<(), CetrizineError>{
        let func_start = Utc::now();

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
                    println!("SKIPPING GUILD, dont have full info needed");
                    continue;
                }
            };
            println!("ARCHIVING GUILD {}", &guild.name);

            let tx = conn.transaction()?;

            pg_insert_helper!(
                tx, "guild",
                ready_rowid => ready_rowid, 
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
                archive_recvd_at => func_start,
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
                    &*conn,
                    &Channel::Guild(Arc::clone(&cell)),
                    guild.name.clone(),
                )?;
            } // for (chan_id, chan) in guild.id.channels()? {
        } //for guild_status in &rdy.guilds {

        //This is for debugging purposes, if a certain channel is causing problems it can be annoying when it changes ordering
        private_channels_to_archive.sort_unstable_by_key(|p| p.0);

        for chan_id in &private_channels_to_archive {
            Self::grab_channel_archive(&*conn, &chan_id.to_channel_cached().expect("channel absolutely should be cached"),String::from(""))?;
        }
        
        println!("FINISHed archiving old messages");
        Ok(())
    } //fn grab_archive ...
    
    fn message_result(&self, _: Context, msg: Message) -> Result<(), CetrizineError> {
        let handler_start = Utc::now();
        let conn = self.pool.get()?;
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
        let _recvd_at = Utc::now();
        let _conn = self.pool.get()?;
        let chan = channel_lock.read();
        println!("Chan create! {:?}", chan.name);
        /*let tx = conn.transaction()?;
        pg_insert_helper!(
            tx, "channel_create_event",
            "recvd_at" => recvd_at,
        )?;

        let cce_rowid = tx.last_insert_rowid();

        pg_insert_helper!(
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
        });*/
        Ok(())
    }   
}   

impl EventHandler for Handler {
    fn ready(&self, ctx: Context, ready: Ready) {
        //TODO: Discord can send multiple readys! What the fuck discord!?
        println!("READY RCVD");
        let conn = self.pool.get().unwrap();
        let count_that_should_be_zero:i64 = conn.query(
            "SELECT COUNT(*) FROM ready WHERE ((user_info).inner_user).discord_id != $1;",
            &[&i64::from(ready.user.id) as &ToSql],
        ).unwrap().get(0).get(0);

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
            let do_archiving = false || true;
            if do_archiving {
                let res = Self::grab_full_archive(&*conn, ctx, ready);
                if let Err(CetrizineError{backtrace: _, error_type: CetrizineErrorType::Serenity(serenity::Error::Http(serenity::prelude::HttpError::UnsuccessfulRequest(mut http_response)))}) = res {
                    eprintln!("http response: {:?}", http_response);
                    let mut body = String::new();
                    std::io::Read::read_to_string(&mut http_response, &mut body).expect("could not read http body");
                    eprintln!("body: {:?}", body);
                }else{ res.expect("unable to grab archive") }
            }
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
                        "postgres connection path, also PG_PATH");
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

    if do_migrate {
        legacy::migrate_sqlite_to_postgres(&postgres_path);
        println!("FINISH");
        std::process::exit(0);
    }
    
    let manager = r2d2_postgres::PostgresConnectionManager::new(postgres_path.as_str(), r2d2_postgres::TlsMode::None).unwrap();
    let pool = r2d2::Pool::new(manager).unwrap();

    let session_id;
    {
        let setup_conn = pool.get().unwrap();
        if init_db {
            let tx = setup_conn.transaction().unwrap();
            tx.batch_execute(migrations::DB_INIT_SQL).unwrap();
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

    let simple_logger_config = simplelog::Config{
        time: Some(simplelog::Level::Info),
        level: Some(simplelog::Level::Info),
        target: Some(simplelog::Level::Info),
        location: Some(simplelog::Level::Info),
        .. Default::default()
    };
    let simple_logger = simplelog::SimpleLogger::new(simplelog::LevelFilter::Info, simple_logger_config);
    let pg_logger = Box::new(postgres_logger::PostgresLogger::new(pool.clone(), log::Level::Info, session_id, beginning_of_time));
    multi_log::MultiLogger::init(vec![simple_logger, pg_logger], simplelog::Level::Info).expect("Failed to intialize logging.");
    info!("Cetrizine logging initialized.");
    
    let handler = Handler{
        pool,
        currently_archiving: Arc::new(Mutex::new(())),
        beginning_of_time,
        started_at,
        session_id,
    };
    let mut client = Client::new(&discord_token, handler).expect("Err creating client");

    if let Err(why) = client.start() {
        error!("Client error: {:?}", why);
    }
}
