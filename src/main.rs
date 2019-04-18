#![feature(nll)]
#![feature(type_alias_enum_variants)]

extern crate serenity;
extern crate rusqlite as sqlite;
extern crate chrono;
extern crate time;

use sqlite::types::{ToSql,ToSqlOutput,FromSql,FromSqlResult,ValueRef};
use sqlite::{Connection, NO_PARAMS};
use sqlite::OptionalExtension;

use std::sync::Mutex;
use std::env;

const DISCORD_MAX_SNOWFLAKE:u64 = 9223372036854775807;

use serenity::{
    model::{gateway::Ready, channel::Message, channel::MessageType},
    model::id::*,
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

pub trait EnumIntoString : Sized {
    fn into_str(&self) -> &'static str;
    fn from_str<'a>(input: &'a str) -> Option<Self>;
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

        $db.execute(sql, values)
    }};
}

macro_rules! enum_stringify {
    ( $enum:ty => $( $var:ident ),+ ) => {
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

enum_stringify!{ serenity::model::guild::DefaultMessageNotificationLevel => All, Mentions }
enum_stringify!{ serenity::model::guild::ExplicitContentFilter => None, WithoutRole, All }
enum_stringify!{ serenity::model::guild::MfaLevel => None, Elevated }
enum_stringify!{ serenity::model::guild::VerificationLevel => None, Low, Medium, High, Higher }
enum_stringify!{ serenity::model::channel::ChannelType => Text, Private, Voice, Group, Category }
enum_stringify!{ serenity::model::gateway::GameType => Playing, Streaming, Listening }
enum_stringify!{ serenity::model::user::OnlineStatus => DoNotDisturb, Idle, Invisible, Offline, Online }

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

pub struct DumbHax<T>(pub T);

impl<T: Clone + Into<u64>> ToSql for DumbHax<T>{
    fn to_sql(&self) -> sqlite::Result<ToSqlOutput> {
        let val:u64 = self.0.clone().into();
        Ok(ToSqlOutput::from(format!("{}",val)))
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

        /*impl ToSql for $klass{
            fn to_sql(&self) -> sqlite::Result<ToSqlOutput> {
                Ok(ToSqlOutput::Owned(sqlite::types::Value::Integer(self.get_snowflake_i64())))
            }
        }*/
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
        let val_i:i64 = unsafe { std::mem::transmute(val_u) };
        //(val_i.clone()).to_sql()
        Ok(ToSqlOutput::Owned(sqlite::types::Value::Integer(val_i)))
    }
}

impl FromSql for PermsToSql{
    fn column_result(value: ValueRef) -> FromSqlResult<Self> {
        i64::column_result(value).map(|val_i| {
            let val_u:u64 = unsafe { std::mem::transmute(val_i) };
            Self(serenity::model::permissions::Permissions::from_bits_truncate(val_u))
        })
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

        let message_rowid = tx.last_insert_rowid();

        //attachments
        for attachment in &msg.attachments {
            insert_helper!(
                tx, "attachment",
                "message_rowid" => message_rowid,
                "discord_id" => attachment.id.parse::<i64>().unwrap(),
                "filename" => attachment.filename,
                "height" => attachment.height.map(|h| h as i64),
                "width" => attachment.width.map(|w| w as i64),
                "proxy_url" => attachment.proxy_url,
                "size" => (attachment.size as i64),
                "url" => attachment.url,
            )?;
        }

        //embeds
        for embed in &msg.embeds {
            insert_helper!(
                tx, "embed",
                "message_rowid" => message_rowid,
                "author_is_some" => embed.author.is_some(),
                "author_icon_url" => embed.author.clone().map(|a| a.icon_url),
                "author_name" => embed.author.clone().map(|a| a.name),
                "author_proxy_icon_url" => embed.author.clone().map(|a| a.proxy_icon_url),
                "author_url" => embed.author.clone().map(|a| a.url),
                "colour_u32" => embed.colour.0,
                "description" => embed.description,
                "footer_is_some" => embed.footer.is_some(),
                "footer_icon_url" => embed.footer.clone().map(|f| f.icon_url),
                "footer_proxy_icon_url" => embed.footer.clone().map(|f| f.proxy_icon_url),
                "footer_text" => embed.footer.clone().map(|f| f.text),
                "image_is_some" => embed.image.is_some(),
                "image_height" => embed.image.clone().map(|i| i.height as i64),
                "image_width" => embed.image.clone().map(|i| i.width as i64),
                "image_proxy_url" => embed.image.clone().map(|i| i.proxy_url),
                "image_url" => embed.image.clone().map(|i| i.url),
                "kind" => embed.kind,
                "provider_is_some" => embed.provider.is_some(),
                "provider_name" => embed.provider.clone().map(|p| p.name),
                "provider_url" => embed.provider.clone().map(|p| p.url),
                "thumbnail_is_some" => embed.thumbnail.is_some(),
                "thumbnail_height" => embed.thumbnail.clone().map(|t| t.height as i64),
                "thumbnail_width" => embed.thumbnail.clone().map(|t| t.width as i64),
                "thumbnail_url" => embed.thumbnail.clone().map(|t| t.url),
                "thumbnail_proxy_url" => embed.thumbnail.clone().map(|t| t.proxy_url),
                "timestamp" => embed.timestamp,
                "title" => embed.title,
                "url" => embed.url,
                "video_is_some" => embed.video.is_some(),
                "video_height" => embed.video.clone().map(|v| v.height as i64),
                "video_width" => embed.video.clone().map(|v| v.width as i64),
                "video_url" => embed.video.clone().map(|v| v.url),
            )?;

            let embed_rowid = tx.last_insert_rowid();

            //insert embed fields
            for embed_field in &embed.fields {
                insert_helper!(
                    tx, "embed_field",
                    "embed_rowid" => embed_rowid,
                    "inline" => embed_field.inline,
                    "name" => embed_field.name,
                    "value" => embed_field.value,
                )?;
            }
        }

        //mentions
        for user in &msg.mentions {
            insert_helper!(
                tx, "user_mention",
                "message_rowid" => message_rowid,
                "id" => SmartHax(user.id),
                "avatar" => user.avatar,
                "bot" => user.bot,
                "discriminator" => user.discriminator,
                "name" => user.name,
            )?;
        }

        //reactions
        for reaction in &msg.reactions {
            let (is_custom, animated, id, name, string) = match &reaction.reaction_type {
                serenity::model::channel::ReactionType::Custom{animated, id, name} =>
                    (true, Some(animated), Some(id), Some(name), None),
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
                "reaction_name" => name,
                "reaction_string" => string,
            )?;
        }
        
        Ok(())
    }
    
    /// This func assumes it's already in its own thread, and will block until complete
    fn grab_archive<'a>(_: Context, rdy: Ready) -> Result<(), CetrizineError<'a>> {
        let func_start = chrono::offset::Local::now();
        let mut conn = make_conn();
        //TODO: insert some tuple for the ready
        let tx = conn.transaction()?;
        insert_helper!(
            tx, "ready",
            "session_id" => rdy.session_id,
            "shard_0" => rdy.shard.clone().map(|a| a[0] as i64),
            "shard_1" => rdy.shard.clone().map(|a| a[1] as i64),
            "trace" => rdy.trace.join(","),
            "user_id" => SmartHax(rdy.user.id),
            "user_avatar" => rdy.user.avatar,
            "user_bot" => rdy.user.bot,
            "user_discriminator" => rdy.user.discriminator,
            "user_email" => rdy.user.email,
            "user_mfa_enabled" => rdy.user.mfa_enabled,
            "user_name" => rdy.user.name,
            "user_verified" => rdy.user.verified,
            "version" => (rdy.version as i64),
        )?;

        let ready_rowid = tx.last_insert_rowid();

        //presences
        for (_,presence) in &rdy.presences {            
            insert_helper!(
                tx, "user_presence",
                "ready_rowid" => ready_rowid,
                "game_is_some" => presence.game.is_some(),
                "game_type" => presence.game.clone().map(|g| g.kind.into_str()),
                "game_name" => presence.game.clone().map(|g| g.name),
                "game_url" => presence.game.clone().map(|g| g.url),
                "last_modified" => presence.last_modified.map(|v| v as i64),
                "nick" => presence.nick,
                "status" => presence.status.into_str(),
                "user_id" => SmartHax(presence.user_id),
            )?;
        }

        //private_channels TODO

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

            tx.execute("INSERT INTO guild (
ready_rowid,
discord_id,
afk_channel_id,
afk_timeout,
application_id,
default_message_notification_level,
explicit_content_filter,
features,
icon,
joined_at,
large,
member_count,
mfa_level,
name,
owner_id,
region,
splash,
system_channel_id,
verification_level,
archive_recvd_at ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
                       &[
                           &ready_rowid as &ToSql,
                           &SmartHax(guild.id) as &ToSql, //discord_id,
                           &SmartHax(guild.afk_channel_id), //afk_channel_id,
                           &(guild.afk_timeout as i64), //afk_timeout,
                           &SmartHax(guild.application_id), //application_id,
                           &guild.default_message_notifications.into_str(),//default_message_notification_level,
                           &guild.explicit_content_filter.into_str(),//explicit_content_filter,
                           &guild.features.join(","),//features,
                           &guild.icon,//icon,
                           &guild.joined_at,//joined_at,
                           &guild.large,//large,
                           &(guild.member_count as i64),//member_count,
                           &guild.mfa_level.into_str(),//mfa_level,
                           &guild.name,//name,
                           &SmartHax(guild.owner_id),//owner_id,
                           &guild.region,//region,
                           &guild.splash,//splash,
                           &SmartHax(guild.system_channel_id),//system_channel_id,
                           &guild.verification_level.into_str(),//verification_level,
                           &func_start,//archive_recvd_at
                       ]
            )?;

            let guild_rowid = tx.last_insert_rowid();

            let mut guild_channels = Vec::<(ChannelId,i64)>::new();

            //channels
            for (_id, chan_a_lock) in &guild.channels {
                let chan = chan_a_lock.read();
                tx.execute("INSERT INTO guild_channel (
discord_id,
guild_rowid,
guild_id,
bitrate,
category_id,
kind,
last_message_id,
last_pin_timestamp,
name,
position,
topic,
user_limit,
nsfw
) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)",
                           &[
                               &SmartHax(chan.id) as &ToSql,
                               &guild_rowid,//guild_rowid,
                               &SmartHax(chan.guild_id),//guild_id,
                               &chan.bitrate.map(|b| b as i64),//bitrate,
                               &SmartHax(chan.category_id),//category_id,
                               &chan.kind.into_str(),//kind,
                               &SmartHax(chan.last_message_id),//last_message_id,
                               &chan.last_pin_timestamp,//last_pin_timestamp,
                               &chan.name,//name,
                               &chan.position,//position,
                               &chan.topic,//topic,
                               &chan.user_limit.map(|u| u as i64),//user_limit,
                               &chan.nsfw,//nsfw
                           ]
                )?;

                let chan_rowid = tx.last_insert_rowid();

                guild_channels.push((chan.id.clone(), chan_rowid));

                for overwrite in &chan.permission_overwrites {
                    use serenity::model::channel::PermissionOverwriteType::*;
                    let (ov_type_str, ov_id) = match overwrite.kind {
                        Member(uid) => ("Member", uid.0),
                        Role(rid) => ("Role", rid.0),
                    };

                    tx.execute("INSERT INTO permission_overwrite (
guild_channel_rowid,
allow_bits,
deny_bits,
permission_overwrite_type,
permission_overwrite_id
) VALUES (?,?,?,?,?)",
                               &[
                                   &chan_rowid as &ToSql,
                                   &PermsToSql(overwrite.allow),
                                   &PermsToSql(overwrite.deny),
                                   &ov_type_str,
                                   &(ov_id as i64),
                               ]
                    )?;
                }
            }

            //emojis
            for (_, emoji) in &guild.emojis {
                tx.execute(
                    "INSERT INTO id_arr (row_id) VALUES (null)",
                    NO_PARAMS
                )?;

                let id_arr_rowid = tx.last_insert_rowid();

                for role_id in &emoji.roles {
                    tx.execute(
                        "INSERT INTO id (id_arr_rowid, id) VALUES (?,?)",
                        &[
                            &id_arr_rowid as &ToSql,
                            &DumbHax(role_id.clone()),
                        ]
                    )?;
                }

                tx.execute("INSERT INTO emoji (
discord_id,
guild_rowid,
animated,
name,
managed,
require_colons,
roles
) VALUES (?,?,?,?,?,?,?)",
                           &[
                               &SmartHax(emoji.id) as &ToSql,//discord_id,
                               &guild_rowid,//guild_rowid,
                               &emoji.animated,//animated,
                               &emoji.name,//name,
                               &emoji.managed,//managed,
                               &emoji.require_colons,//require_colons,
                               &id_arr_rowid,//roles
                           ]
                )?;
            }

            //members
            for (_, member) in &guild.members {
                tx.execute(
                    "INSERT INTO id_arr (row_id) VALUES (null)",
                    NO_PARAMS
                )?;

                let id_arr_rowid = tx.last_insert_rowid();

                for role_id in &member.roles {
                    tx.execute(
                        "INSERT INTO id (id_arr_rowid, id) VALUES (?,?)",
                        &[
                            &id_arr_rowid as &ToSql,
                            &DumbHax(role_id.clone()),
                        ]
                    )?;
                }

                let user = member.user.read();

                tx.execute("INSERT INTO member (
guild_rowid,
deaf,
guild_id,
joined_at,
mute,
nick,
roles,
user_id,
user_avatar,
user_is_bot,
user_discriminator,
user_name
) VALUES (?,?,?,?,?,?,?,?,?,?,?,?)",
                           &[
                               &guild_rowid as &ToSql,
                               &member.deaf,
                               &SmartHax(member.guild_id),
                               &member.joined_at,
                               &member.mute,
                               &member.nick,
                               &id_arr_rowid,
                               &SmartHax(user.id),
                               &user.avatar,
                               &user.bot,
                               &user.discriminator,
                               &user.name,
                           ]
                )?;
            }

            //presences
            for (_, presence) in &guild.presences {
                tx.execute("INSERT INTO user_presence (
guild_rowid,
game_is_some,
game_type,
game_name,
game_url,
last_modified,
nick,
status,
user_id
) VALUES (?,?,?,?,?,?,?,?,?)",
                           &[
                               &guild_rowid as &ToSql, //guild_rowid
                               &presence.game.is_some(), //game_is_some
                               &presence.game.clone().map(|g| g.kind.into_str()), //game_type
                               &presence.game.clone().map(|g| g.name), //game_name
                               &presence.game.clone().map(|g| g.url), //game_url
                               &presence.last_modified.map(|v| v as i64),
                               &presence.nick,
                               &presence.status.into_str(),
                               &SmartHax(presence.user_id),
                           ]
                )?;
            }

            //roles
            for (_, role) in &guild.roles {
                tx.execute("INSERT INTO guild_role (
discord_id,
guild_rowid,
colour_u32,
hoist,
managed,
mentionable,
name,
permission_bits,
position
) VALUES (?,?,?,?,?,?,?,?,?)",
                           &[
                               &SmartHax(role.id) as &ToSql, //discord_id
                               &guild_rowid, //guild_rowid
                               &role.colour.0, //colour_u32
                               &role.hoist,
                               &role.managed,
                               &role.mentionable,
                               &role.name,
                               &PermsToSql(role.permissions),
                               &role.position
                           ]
                )?;
            }

            //voice_states
            for (_, voice_state) in &guild.voice_states {
                tx.execute("INSERT INTO voice_state (
guild_rowid,
channel_id,
deaf,
mute,
self_deaf,
self_mute,
session_id,
suppress,
token,
user_id
) VALUES (?,?,?,?,?,?,?,?,?,?)",
                           &[
                               &guild_rowid as &ToSql,
                               &SmartHax(voice_state.channel_id),
                               &voice_state.deaf,
                               &voice_state.mute,
                               &voice_state.self_deaf,
                               &voice_state.self_mute,
                               &voice_state.session_id,
                               &voice_state.suppress,
                               &voice_state.token,
                               &SmartHax(voice_state.user_id),
                           ]
                )?;
            }
                               

            tx.commit()?;
            
            //for (chan_id, chan) in guild.id.channels()? {
            for (chan_id, _chan_rowid) in guild_channels {
                let chan = guild.channels[&chan_id].read();
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
                println!("ARCHIVING CHAN {} (id {})", &chan.name, &chan.id);

                let last_msg_id = match chan.last_message_id {
                    Some(id) => id,
                    None => continue,
                };

                let mut maybe_res:Option<(u64,Option<String>)> = conn.query_row(
                    "SELECT start_message_id,after_message_id FROM message_archive_gets WHERE (
substr('00000000000000000000'||start_message_id, -20, 20)
<=
substr('00000000000000000000'||?1, -20, 20)
AND
substr('00000000000000000000'||?1, -20, 20)
<=
substr('00000000000000000000'||end_message_id, -20, 20) )
ORDER BY substr('00000000000000000000'||end_message_id, -20, 20) ASC LIMIT 1;",
                    &[&DumbHax(last_msg_id) as &ToSql],
                    |r| Ok((r.get::<_,String>(0)?.parse::<u64>().unwrap(),r.get::<_,Option<String>>(1)?))
                ).optional()?;

                let mut get_before = DISCORD_MAX_SNOWFLAKE;
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
AND
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

                let mut earliest_message_recvd = DISCORD_MAX_SNOWFLAKE;

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
                        //println!("Message id {:?}", msg.id);
                        Self::archive_message(&tx, msg, recvd_at)?;
                    }
                    if msgs.len() == 0 { break }
                    //TODO: put channel_rowid in there somwhere
                    insert_helper!(
                        tx, "message_archive_gets",
                        "channel_id" => DumbHax(chan_id),
                        "before_message_id" => DumbHax(earliest_message_recvd),
                        "start_message_id" => DumbHax(msgs.first().unwrap().id),
                        "end_message_id" => DumbHax(msgs.last().unwrap().id),
                        "message_count_requested" => asking_for,
                        "message_count_received" => (msgs.len() as i64),
                        "received_live" => false,
                    )?;
                    /*tx.execute("INSERT INTO message_archive_gets (
channel_id,
before_message_id,
start_message_id,
end_message_id,
message_count_requested,
message_count_received,
received_live
) VALUES (?,?,?,?,?,?,?)",
                               &[
                                   &DumbHax(chan_id) as &ToSql,
                                   &DumbHax(earliest_message_recvd),
                                   &DumbHax(msgs.first().unwrap().id),
                                   &DumbHax(msgs.last().unwrap().id),
                                   &asking_for,
                                   &(msgs.len() as i64),
                                   &false
                               ]
                    )?;*/
                    tx.commit()?;
                    println!("Archived {} message(s), {} thru {} in {}#{}", msgs.len(), msgs.first().unwrap().id, msgs.last().unwrap().id, guild.name, chan.name);
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
            let res = Self::grab_archive(ctx, ready);
            if let Err(CetrizineError::Serenity(serenity::Error::Http(serenity::prelude::HttpError::UnsuccessfulRequest(mut http_response)))) = res {
                eprintln!("http response: {:?}", http_response);
                let mut body = String::new();
                std::io::Read::read_to_string(&mut http_response, &mut body).unwrap();
                eprintln!("body: {:?}", body);
            }else{ res.unwrap() }
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
--embeds
guild_id text,
kind text,
member_is_some int not null, --bool
member_deaf int, --bool
member_joined_at text, --datetime
member_mute int, --bool
member_roles int REFERENCES id_arr(row_id),
--mentions
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

    conn.execute("CREATE TABLE IF NOT EXISTS user_mention (
message_rowid int not null REFERENCES message(rowid),
id int not null,
avatar text,
bot int not null, --bool
discriminator int not null,
name text not null
)", NO_PARAMS).unwrap();
    
    conn.execute("CREATE TABLE IF NOT EXISTS reaction (
message_rowid int not null REFERENCES message(rowid),
count int not null,
me int not null, --bool
reaction_is_custom int not null, --bool

--if reaction is custom:
reaction_animated int, --bool
reaction_id int,
reaction_name text

--else
reaction_string text
)", NO_PARAMS).unwrap();
    
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
height int,
width int,
proxy_url text not null,
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
--fields
footer_is_some int not null, --bool
footer_icon_url text,
footer_proxy_icon_url text,
footer_text text,
image_is_some int not null, --bool
image_height int,
image_width int,
image_proxy_url text,
image_url text,
kind text,
provider_is_some int not null, --bool
provider_name text,
provider_url text,
thumbnail_is_some int not null, --bool
thumbnail_height int,
thumbnail_width int,
thumbnail_url text,
thumbnail_proxy_url text,
timestamp text,
title text,
url text,
video_is_some int not null, --bool
video_height int,
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

    conn.execute("CREATE TABLE IF NOT EXISTS ready (
--guilds
--presences
--private_channels
session_id text not null,
shard_0 int,
shart_1 int,
trace text not null,
user_id int not null,
user_avatar text,
user_bot int not null, --bool
user_discriminator int not null,
user_email text,
user_mfa_enabled int not null, --bool
user_name text,
user_verified int not null, --bool
version int not null
)", NO_PARAMS).unwrap();
    
    conn.execute("CREATE TABLE IF NOT EXISTS guild (
ready_rowid int REFERENCES ready(rowid), --possibly null
discord_id int not null,
afk_channel_id int,
afk_timeout int not null,
application_id int,
--channels
default_message_notification_level int not null,
--emojis
explicit_content_filter text not null,
features text not null, --comma separated
icon text,
joined_at text not null, --datetime
large int not null, --bool
member_count int not null,
--members
mfa_level text not null,
name text not null,
owner_id int not null,
--presences
region text not null,
--roles
splash text,
system_channel_id int,
verification_level text not null,
--voice states
archive_recvd_at text not null --datetime
)", NO_PARAMS).unwrap();
    
    conn.execute("CREATE TABLE IF NOT EXISTS guild_channel (
discord_id int not null,
guild_rowid int not null REFERENCES guild(rowid),
guild_id int not null,
bitrate int,
category_id int,
kind text not null,
last_message_id int,
last_pin_timestamp text, --datetime
name text not null,
--permission overwrites
position int not null, --can be negative!
topic text,
user_limit int,
nsfw int not null --bool
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS emoji (
discord_id int not null,
guild_rowid int not null REFERENCES guild(rowid),
animated int not null, --bool
name text not null,
managed int not null, --bool
require_colons int not null, --bool
roles int not null REFERENCES id_arr(row_id)
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS member (
guild_rowid int not null REFERENCES guild(rowid),
deaf int not null, --bool
guild_id int not null,
joined_at text, --datetime
mute int not null,
nick text,
roles int not null REFERENCES id_arr(row_id),
user_id int not null,
user_avatar text,
user_is_bot int not null, --bool
user_discriminator int not null,
user_name text not null
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS user_presence (
ready_rowid int REFERENCES ready(rowid),
guild_rowid int REFERENCES guild(rowid),
game_is_some int not null, --bool
game_type text,
game_name text,
game_url text,
last_modified int, --apparently might be null????
nick text,
status text,
user_id int not null,
CHECK ( (ready_rowid NOT NULL OR guild_rowid NOT NULL) AND (ready_rowid NULL OR guild_rowid NULL) )
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS voice_state (
guild_rowid int not null REFERENCES guild(rowid),
channel_id int,
deaf int not null, --bool
mute int not null, --bool
self_deaf int not null, --bool
self_mute int not null, --bool
session_id text not null,
suppress int not null, --bool
token text,
user_id int
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS guild_role (
discord_id int not null,
guild_rowid int not null REFERENCES guild(rowid),
colour_u32 int not null,
hoist int not null, --bool
managed int not null, --bool
mentionable int not null, --bool
name text not null,
permissions_bits int not null,
position int not null
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS permission_overwrite (
guild_channel_rowid int not null REFERENCES guild_channel(rowid),
allow_bits int not null,
deny_bits int not null,
permission_overwrite_type text not null,
permission_overwrite_id int not null
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
    conn.execute("CREATE INDEX IF NOT EXISTS mag_start_plain ON message_archive_gets(start_message_id)", NO_PARAMS).unwrap();
    conn.execute("CREATE INDEX IF NOT EXISTS mag_start_plain ON message_archive_gets(end_message_id)", NO_PARAMS).unwrap();
        
    let handler = Handler{conn: Mutex::new(conn)};
    let mut client = Client::new(&token, handler).expect("Err creating client");

    if let Err(why) = client.start() {
        eprintln!("Client error: {:?}", why);
    }
}
