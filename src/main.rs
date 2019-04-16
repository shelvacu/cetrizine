#![feature(nll)]

extern crate serenity;
extern crate rusqlite as sqlite;
extern crate chrono;
extern crate time;

use sqlite::types::{ToSql,ToSqlOutput};
use sqlite::{Connection, NO_PARAMS};
use sqlite::OptionalExtension;

use std::sync::Mutex;
use std::env;

use serenity::{
    model::{gateway::Ready, channel::Message, channel::MessageType},
    prelude::*,
};

struct Handler{
    conn: Mutex<Connection>,
}

fn message_type_to_string(m: MessageType) -> String {
    use serenity::model::channel::MessageType::*;
    match m {
        Regular => "Regular",
        GroupRecipientAddition => "GroupRecipientAddition",
        GroupRecipientRemoval => "GroupRecipientRemoval",
        GroupCallCreation => "GroupCallCreation",
        GroupNameUpdate => "GroupNameUpdate",
        GroupIconUpdate => "GroupIconUpdate",
        PinsAdd => "PinsAdd",
        MemberJoin => "MemberJoin",
    }.into()
}

fn get_name(chan:serenity::model::channel::Channel) -> String {
    //use serenity::model::channel::Channel;
    use serenity::model::channel::Channel::*;
    
    match chan {
        Group(group) => group.read().name.clone().unwrap_or("".to_owned()),
        Guild(guild) => guild.read().name.clone(),
        Private(private) => private.read().recipient.read().name.clone(),
        Category(cat) => cat.read().name.clone(),
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
        write!(f, "{:?}", self)
    }
}

impl<'a> std::error::Error for CetrizineError<'a> {
    fn description(&self) -> &str {
        use CetrizineError::*;
        match self {
            Mutex(_)    => "Mutex poisoned",
            Sql(_)      => "SQLite Error",
            Serenity(_) => "Serenity Error",
        }
    }

    fn cause(&self) -> Option<&std::error::Error> {
        use CetrizineError::*;
        match self {
            Mutex(e) => Some(e),
            Sql(e)   => Some(e),
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

pub struct DumbHax<T: Clone + Into<u64>>(pub T);

impl<T: Clone + Into<u64>> ToSql for DumbHax<T>{
    fn to_sql(&self) -> sqlite::Result<ToSqlOutput> {
        let val:u64 = self.0.clone().into();
        Ok(ToSqlOutput::from(format!("{}",val)))
    }
}

impl Handler {
    fn archive_message<'a>(tx: &sqlite::Transaction, msg: &Message, recvd_at:chrono::DateTime<chrono::offset::Local>) -> Result<(), CetrizineError<'a>> {
        let memb = &msg.member;
        //let tx = conn.transaction()?;
        
        let member_roles_arr_id = if let Some(m) = memb {
            tx.execute(
                "INSERT INTO id_arr (row_id) VALUES (null)",
                NO_PARAMS
            )?;
            let id = tx.last_insert_rowid();
            for r in &m.roles {
                tx.execute(
                    "INSERT INTO id (id_arr_rowid, id) VALUES (?1, ?2)",
                    &[&id as &ToSql,&DumbHax(r.clone())]
                )?;
            }
            Some(id)
        } else { None };

        let mention_roles_arr_id = {
            tx.execute(
                "INSERT INTO id_arr (row_id) VALUES (null)",
                NO_PARAMS
            )?;

            let id = tx.last_insert_rowid();

            for r in &msg.mention_roles {
                tx.execute(
                    "INSERT INTO id (id_arr_rowid, id) VALUES (?1,?2)",
                    &[&id as &ToSql,&DumbHax(r.clone())]
                )?;
            }
            id
        };

        /*let reactions_arr_id = {
            tx.execute(
                "INSERT INTO id_arr () VALUES ()",
                NO_PARAMS
            )?;

            let id = tx.last_insert_rowid();

            for r in &msg.reactions {
                tx.execute(
                    "INSERT INTO id (id_arr_rowid, id) VALUES (?1,?2)",
                    &[&id as &ToSql,&DumbHax(r)]
                )?;
            }
            id
        };*/
        
        tx.execute(
            "INSERT INTO message (
discord_id,
author_id, 
author_avatar,
author_is_bot,
author_discriminator,
author_name,
channel_id,
content,
edited_timestamp,
guild_id,
kind,
member_is_some,
member_deaf,
member_joined_at,
member_mute,
member_roles, --****
mention_everyone,
mention_roles, --****
nonce_debug,
pinned,
timestamp,
tts,
webhook_id,
archive_recvd_at
) VALUES (?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11,?12,?13,?14,?15,?16,?17,?18,?19,?20,?21,?22,?23,?24)",
            &[
                &DumbHax(msg.id) as &ToSql, //discord_id
                &DumbHax(msg.author.id), //author_id
                &msg.author.avatar, //author_avatar
                &msg.author.bot, //author_is_bot
                &msg.author.discriminator, //author_discriminator
                &msg.author.name, //author_name
                &DumbHax(msg.channel_id), //channel_id
                &msg.content, //content
                &msg.edited_timestamp, //edited_timestamp
                &msg.guild_id.map(|gid| DumbHax(gid)), //guild_id
                &message_type_to_string(msg.kind), //kind
                &memb.is_some(), //member_is_some
                &memb.clone().map(|m| m.deaf), //member_deaf
                &memb.clone().map(|m| m.joined_at), //member_joined_at
                &memb.clone().map(|m| m.mute), //member_mute
                &member_roles_arr_id, //member_roles
                &msg.mention_everyone, //mention_everyone
                &mention_roles_arr_id, //mention_roles
                // &mentions_arr_id, //mentions
                &format!("{:?}",msg.nonce), //nonce_debug
                &msg.pinned, //pinned
                &msg.timestamp, //timestamp
                &msg.tts, //tts
                &msg.webhook_id.map(|whid| DumbHax(whid)), //webhook_id
                &recvd_at, //archive_recvd_at
            ]
        )?;

        //tx.commit()?;
        Ok(())
    }
    
    /// This func assumes it's already in its own thread, and will block until complete
    fn grab_archive<'a>(_: Context, rdy: Ready) -> Result<(), CetrizineError<'a>> {
        let mut conn = make_conn();
        //TODO: insert some tuple for the ready
        for guild_status in &rdy.guilds {
            let guild = match guild_status {
                serenity::model::guild::GuildStatus::OnlineGuild(g) => g,
                _ => {
                    println!("SKIPPING GUILD, dont have full info needed");
                    continue;
                }
            };
            println!("ARCHIVING GUILD {:?}", &guild);
            for (chan_id, chan) in guild.id.channels()? {
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
                println!("ARCHIVING CHAN {:?}", &chan);

                let last_msg_id = match chan.last_message_id {
                    Some(id) => id,
                    None => continue,
                };

                let mut maybe_res:Option<(u64,Option<String>)> = conn.query_row(
                    "SELECT start_message_id,after_message_id FROM message_archive_gets WHERE (
substr('00000000000000000000'||start_message_id, -20, 20)
<=
substr('00000000000000000000'||?1, -20, 20)
&&
substr('00000000000000000000'||?1, -20, 20)
<=
substr('00000000000000000000'||end_message_id, -20, 20) )
ORDER BY substr('00000000000000000000'||end_message_id, -20, 20) ASC LIMIT 1;",
                    &[&DumbHax(last_msg_id) as &ToSql],
                    |r| Ok((r.get::<_,String>(0)?.parse::<u64>().unwrap(),r.get::<_,Option<String>>(1)?))
                ).optional()?;

                let mut get_before = std::u64::MAX;
                //let before_id:u64 = last_msg_id.into();
                let mut before_message_id:Option<String>;// = Some(before_id.to_string());
                
                while let Some(res) = maybe_res {
                    get_before = res.0;//.get::<_,String>(0).parse::<u64>().unwrap(); //49
                    before_message_id = res.1;//.get::<_,Option<String>>(1); //null

                    maybe_res = conn.query_row(
                    "SELECT start_message_id,after_message_id FROM message_archive_gets WHERE (
substr('00000000000000000000'||start_message_id, -20, 20)
<
substr('00000000000000000000'||?1, -20, 20)
&&
substr('00000000000000000000'||?1, -20, 20)
<=
substr('00000000000000000000'||end_message_id, -20, 20) ) OR
before_message_id = ?1 OR
(?2 NOT NULL AND end_message_id = ?2)
ORDER BY substr('00000000000000000000'||start_message_id, -20, 20) ASC LIMIT 1;",
                        &[&DumbHax(get_before) as &ToSql,&before_message_id],
                        |r| Ok((r.get::<_,String>(0)?.parse::<u64>().unwrap(),r.get::<_,Option<String>>(1)?))
                    ).optional()?;
                }

                //we're "in" a gap, lets find where this gap ends.

                let gap_end = conn.query_row(
                    "SELECT end_message_id FROM message_archive_gets WHERE substr('00000000000000000000'||end_message_id, -20, 20) < substr('00000000000000000000'||?1, -20, 20) ORDER BY substr('00000000000000000000'||start_message_id, -20, 20) ASC LIMIT 1;",
                    &[&DumbHax(get_before) as &ToSql],
                    |r| r.get(0)
                ).optional()?.unwrap_or(String::from("0")).parse::<u64>().unwrap();

                let mut earliest_message_recvd = std::u64::MAX;

                while earliest_message_recvd > gap_end {
                    let asking_for = 100u8;
                    let mut msgs = chan_id.messages(|r| r.limit(asking_for.into()).before(earliest_message_recvd))?;
                    //messages seem to usually be ordered, largest ids first
                    //however, that's documented literally nowhere, so lets sort them
                    msgs.sort_unstable_by_key(|msg| {
                        let msg_id:u64 = msg.id.into();
                        std::u64::MAX - msg_id
                    });
                    let recvd_at = chrono::offset::Local::now();
                    let tx = conn.transaction()?;
                    for msg in &msgs {
                        println!("Message id {:?}", msg.id);
                        Self::archive_message(&tx, msg, recvd_at)?;
                    }
                    if msgs.len() == 0 { break }
                    tx.execute("INSERT INTO message_archive_gets (
channel_id,
start_message_id,
end_message_id,
message_count_requested,
message_count_received,
received_live
) VALUES (?,?,?,?,?,?)",
                               &[
                                   &DumbHax(chan_id) as &ToSql,
                                   &DumbHax(msgs.first().unwrap().id),
                                   &DumbHax(msgs.last().unwrap().id),
                                   &asking_for,
                                   &(msgs.len() as i64),
                                   &false
                               ]
                    )?;
                    tx.commit()?;

                    earliest_message_recvd = msgs.last().unwrap().id.into();
                } //while earliest_message_recvd > gap_end {
                //TODO: We've either come to the end of messages in this channel,
                // or the end of this gap. if it's the latter, we should continue.
            } // for (chan_id, chan) in guild.id.channels()? {
        } //for guild_status in &rdy.guilds {
                
        Ok(())
    } //fn grab_archive ...
    
    fn message_result(&self, _: Context, msg: Message) -> Result<(), CetrizineError> {
        let handler_start = chrono::prelude::Local::now();
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
        let mut conn = self.conn.lock()?;
        println!("CHAN: {:?}", msg.channel_id.to_channel().map(|c| get_name(c)));
        println!("USR: {:?}#{}", msg.author.name, msg.author.discriminator);
        println!("MSG: {:?}", msg.content);

        let tx = conn.transaction()?;
        Self::archive_message(&tx, &msg, handler_start)?;
        tx.commit()?;

        println!();
        Ok(())
    }
}   

impl EventHandler for Handler {
    fn ready(&self, ctx: Context, ready: Ready) {
        println!("READY: {:#?}",ready);
        std::thread::spawn(move || {
            Self::grab_archive(ctx, ready).unwrap();
        });
    }

    fn message(&self, ctx: Context, msg: Message) {
        match self.message_result(ctx, msg) {
            Ok(()) => (),
            Err(e) => eprintln!("ERROR: {:?}", e),
        }
    }
}

fn make_conn() -> Connection {
    let conn = Connection::open("messages2-blarg.db").expect("couldnt open connection");
    conn.set_db_config(
        sqlite::config::DbConfig::SQLITE_DBCONFIG_ENABLE_FKEY,
        true,
    ).unwrap();

    return conn;
}

fn main() {
    let token = env::var("DISCORD_TOKEN")
        .or_else(|_| std::fs::read_to_string("../discord.token"))
        .expect("Expected a token in the environment")
        .to_owned();

    let conn = make_conn();
    //conn.execute("PRAGMA foreign_keys = ON;").unwrap();
    conn.execute("CREATE TABLE IF NOT EXISTS message (
--implied rowid
discord_id text not null,
--attachments
author_id text not null,
author_avatar text,
author_is_bot int not null, --bool
author_discriminator int not null,
author_name text not null,
channel_id text not null,
content text not null,
edited_timestamp text, --datetime
guild_id text,
kind text,
member_is_some int not null, --bool
member_deaf int, --bool
member_joined_at text, --datetime
member_mute int, --bool
member_roles int REFERENCES id_arr(row_id),
mention_everyone int not null, --bool
mention_roles int not null REFERENCES id_arr(row_id),
nonce_debug text not null, --{:?}
pinned int not null, --bool
--reactions
timestamp text not null,
tts int not null, --bool
webhook_id text,
archive_recvd_at text not null --datetime
)", NO_PARAMS).unwrap();

    //create table user_mention
    
    /*conn.execute("CREATE TABLE IF NOT EXISTS reaction (
message_rowid int not null REFERENCES message(rowid),
count int not null,
me int not null, --bool
reaction_type --ah fuck*/
    
    conn.execute("CREATE TABLE IF NOT EXISTS id_arr (
row_id integer primary key
)", NO_PARAMS).unwrap();
    
    conn.execute("CREATE TABLE IF NOT EXISTS id (
id_arr_rowid int not null REFERENCES id_arr(row_id),
id text not null
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS attachment (
message_rowid int not null REFERENCES message(rowid),
discord_id text not null,
filename text not null,
height_text text, --technically the width & height is a u64 which can't 'fit' in sqlite
width_text text,
height int,
width int,
proxy_url text not null,
size_text text not null,
size int not null,
url text not null
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS embed (
message_rowid int not null REFERENCES message(rowid),
author_is_some int not null, --bool
author_icon_url text,
author_name text,
author_proxy_icon_url text,
author_url text,
colour_u32 int not null,
description text,
footer_is_some int not null, --bool
footer_icon_url text,
footer_proxy_icon_url text,
footer_text text,
image_is_some int not null, --bool
image_height_text text,
image_height int,
image_width_text text,
image_width int,
image_proxy_url text,
image_url text,
kind text,
provider_is_some int not null, --bool
provider_name text,
provider_url text,
thumbnail_is_some int not null, --bool
thumbnail_height_text text,
thumbnail_height int,
thumbnail_width_text text
thumbnail_width int,
thumbnail_url text,
thumbnail_proxy_url text,
timestamp text not null,
title text not null,
url text not null,
video_is_some int not null, --bool
video_height_text text,
video_height int,
video_width_text text,
video_width int,
video_url text
)
", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS embed_field (
embed_rowid int not null REFERENCES embed(rowid),
inline int not null, --bool
name text not null,
value text not null
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS message_archive_gets (
channel_id text not null,
after_message_id text,
around_message_id text,
before_message_id text,
start_message_id text not null,
end_message_id text not null,
message_count_requested int not null,
message_count_received int not null,
received_live int not null --bool
)", NO_PARAMS).unwrap();

    conn.execute("CREATE INDEX IF NOT EXISTS mag_start ON message_archive_gets (substr('00000000000000000000'||start_message_id, -20, 20))", NO_PARAMS).unwrap();
    conn.execute("CREATE INDEX IF NOT EXISTS mag_end   ON message_archive_gets (substr('00000000000000000000'||  end_message_id, -20, 20))", NO_PARAMS).unwrap();
    conn.execute("CREATE INDEX IF NOT EXISTS mag_start_plain ON message_archve_gets(start_message_id)", NO_PARAMS).unwrap();
    conn.execute("CREATE INDEX IF NOT EXISTS mag_start_plain ON message_archve_gets(end_message_id)", NO_PARAMS).unwrap();
        
    let handler = Handler{conn: Mutex::new(conn)};
    let mut client = Client::new(&token, handler).expect("Err creating client");

    if let Err(why) = client.start() {
        eprintln!("Client error: {:?}", why);
    }
}
