use crate::serenity::model::prelude::*;
use crate::serenity::client::bridge::gateway::ShardId;
use crate::serenity::utils::Colour;
//use crate::pg::types::{IsNull,Type,ToSql,FromSql};
use crate::diesel::{
    sql_types::*,
    backend::Backend,
    serialize::{self,ToSql,Output},
    deserialize::{self,FromSql},
    pg::Pg,
};
//use crate::FilterExt;
//use crate::EnumIntoString;

use std::error::Error;
use std::fmt::Debug;
use std::io::Write;

trait StrExt {
    fn filter_null(&self) -> Self;
}

impl StrExt for String {
    fn filter_null(&self) -> String {
        self.clone() //BAD IMPL! for testing
    }
}

impl StrExt for Option<String> {
    fn filter_null(&self) -> Option<String> {
        self.clone() //BAD IMPL! for testing
    }
}

pub trait InnerSnowflake {
    fn get_snowflake(&self) -> u64;

    fn get_snowflake_i64(&self) -> i64 {
        self.get_snowflake() as i64
    }
}

macro_rules! snowflake_impl_old {
    ($klass:ty) => {
        impl InnerSnowflake for $klass {
            fn get_snowflake(&self) -> u64 {
                self.0
            }
        }

        impl InnerSnowflake for &$klass {
            fn get_snowflake(&self) -> u64 {
                self.0
            }
        }
    };
}

macro_rules! snowflake_impl {
    ($klass:ty) => {
        impl InnerSnowflake for $klass {
            fn get_snowflake(&self) -> u64 {
                *self.as_u64()
            }
        }

        impl InnerSnowflake for &$klass {
            fn get_snowflake(&self) -> u64 {
                *self.as_u64()
            }
        }
    };
}

snowflake_impl!{ApplicationId}
snowflake_impl!{AttachmentId}
snowflake_impl!{AuditLogEntryId}
snowflake_impl!{ChannelId}
snowflake_impl!{EmojiId}
snowflake_impl!{GuildId}
snowflake_impl!{IntegrationId}
snowflake_impl!{MessageId}
snowflake_impl!{RoleId}
snowflake_impl_old!{ShardId}
snowflake_impl!{UserId}
snowflake_impl!{WebhookId}

#[derive(Debug)]
pub struct SmartHax<T: Debug>(pub T);


impl<DB, T> ToSql<Int8, DB> for SmartHax<T>
where
    DB: Backend,
    i64: ToSql<Int8, DB>,
    T: InnerSnowflake + Debug,
{
    fn to_sql<W: Write>(&self, out: &mut Output<W, DB>) -> serialize::Result {
        (self.0.get_snowflake_i64()).to_sql(out)
    }
}

impl<DB, T> ToSql<Nullable<Int8>, DB> for SmartHax<Option<T>>
where
    DB: Backend,
    i64: ToSql<Int8, DB>,
    T: InnerSnowflake + Debug,
{
    fn to_sql<W: Write>(&self, out: &mut Output<W, DB>) -> serialize::Result {
        self.0.as_ref().map(|a| a.get_snowflake_i64()).to_sql(out)
    }
}

#[derive(Debug)]
pub struct PermsToSql(pub serenity::model::permissions::Permissions);

impl<DB> ToSql<Int8, DB> for PermsToSql
where
    DB: Backend,
    i64: ToSql<Int8, DB>,
{
    fn to_sql<W: Write>(&self, out: &mut Output<W, DB>) -> serialize::Result {
        i64::from_le_bytes(self.0.bits().to_le_bytes()).to_sql(out)
    }
}

// FromSql serenity::model::permissions::Permissions::from_bits_truncate(val_u)

/// Columns **must** be in the same order as database
// based on https://github.com/diesel-rs/diesel/issues/1732
macro_rules! composite_type {
    (
        $(#[$m:meta])+
        $sql_type_type:ident, $struct_name:ident,
        $( $rust_column_name:ident -> $sql_ty:ty : $rust_ty:ty, )+
    ) => {
        $(#[$m])+
        #[derive(FromSqlRow, AsExpression)]
        pub struct $struct_name {
            $( pub $rust_column_name: $rust_ty, )+
        }

        impl ::diesel::serialize::ToSql<$sql_type_type, ::diesel::pg::Pg> for $struct_name {
            fn to_sql<W: ::std::io::Write>(&self, out: &mut ::diesel::serialize::Output<W, ::diesel::pg::Pg>) -> ::diesel::serialize::Result {
                ::diesel::serialize::WriteTuple::<($($sql_ty,)+)>::write_tuple(
                    &( $( &self . $rust_column_name , )+ ),
                    out,
                )
            }
        }

        impl ::diesel::deserialize::FromSql<$sql_type_type, ::diesel::pg::Pg> for $struct_name {
            fn from_sql(bytes: ::std::option::Option<&[u8]>) -> ::diesel::deserialize::Result<Self> {
                let ($($rust_column_name,)+) = <($($rust_ty, )+) as ::diesel::deserialize::FromSql<::diesel::pg::types::sql_types::Record<($($sql_ty, )+)>, ::diesel::pg::Pg>>::from_sql(bytes)?;
                ::std::result::Result::Ok(Self{$($rust_column_name,)+})
            }
        }
    };
}

#[derive(Debug, SqlType)]
#[postgres(type_name = "snowflake")]
pub struct SQL_Snowflake;

#[derive(Debug, FromSqlRow, AsExpression, PartialEq, Eq, PartialOrd, Ord)]
#[sql_type = "SQL_Snowflake"]
pub struct Snowflake(pub i64);

impl ToSql<SQL_Snowflake, Pg> for Snowflake {
    fn to_sql<W: Write>(&self, out: &mut serialize::Output<W, Pg>) -> serialize::Result {
        <i64 as ToSql<Int8, Pg>>::to_sql(&self.0, out)
    }
}

impl FromSql<SQL_Snowflake, Pg> for Snowflake {
    fn from_sql(bytes: Option<&[u8]>) -> deserialize::Result<Self> {
        <i64 as FromSql::<Int8, Pg>>::from_sql(bytes).map(|v| Snowflake(v))
    }
}

impl<T> From<T> for Snowflake
where T: InnerSnowflake {
    fn from(val: T) -> Self {
        Self(val.get_snowflake_i64())
    }
}

#[derive(Debug, SqlType)]
#[postgres(type_name = "discord_colour")]
pub struct SQL_DiscordColour;

#[derive(Debug, FromSqlRow, AsExpression, PartialEq, Eq)]
#[sql_type = "SQL_DiscordColour"]
pub struct DiscordColour(pub i64);

impl ToSql<SQL_DiscordColour, Pg> for DiscordColour {
    fn to_sql<W: Write>(&self, out: &mut serialize::Output<W, Pg>) -> serialize::Result {
        <i64 as ToSql<Int8, Pg>>::to_sql(&self.0, out)
    }
}

impl FromSql<SQL_DiscordColour, Pg> for DiscordColour {
    fn from_sql(bytes: Option<&[u8]>) -> deserialize::Result<Self> {
        <i64 as FromSql::<Int8, Pg>>::from_sql(bytes).map(|v| DiscordColour(v))
    }
}

impl From<u32> for DiscordColour {
    fn from(val: u32) -> Self {
        Self(val.into():i64)
    }
}

#[derive(Debug, SqlType)]
#[postgres(type_name = "__t_discord_user")]
pub struct SQL_DiscordUser;

composite_type! {
    #[derive(PartialEq,Eq,Debug)]
    #[sql_type = "SQL_DiscordUser"]
    SQL_DiscordUser, DbDiscordUser,
    discord_id -> SQL_Snowflake : Snowflake,
    avatar -> Nullable<Text> : Option<String>,
    is_bot -> Bool : bool,
    discriminator -> Int2 : i16,
    name -> Text : String,
}

#[derive(Debug, SqlType)]
#[postgres(type_name = "__t_embed_author")]
pub struct SQL_EmbedAuthor;

composite_type! {
    #[derive(PartialEq,Eq,Debug)]
    #[sql_type = "SQL_EmbedAuthor"]
    SQL_EmbedAuthor, DbEmbedAuthor,
    icon_url -> Nullable<Text> : Option<String>,
    name -> Text : String,
    proxy_icon_url -> Nullable<Text> : Option<String>,
    url -> Nullable<Text> : Option<String>,
}

#[derive(Debug, SqlType)]
#[postgres(type_name = "__t_embed_footer")]
pub struct SQL_EmbedFooter;

composite_type! {
    #[derive(PartialEq,Eq,Debug)]
    #[sql_type = "SQL_EmbedFooter"]
    SQL_EmbedFooter, DbEmbedFooter,
    icon_url -> Nullable<Text> : Option<String>,
    proxy_icon_url -> Nullable<Text> : Option<String>,
    text -> Text : String,
}

#[derive(Debug, SqlType)]
#[postgres(type_name = "__t_embed_image")]
pub struct SQL_EmbedImage;

composite_type! {
    #[derive(PartialEq,Eq,Debug)]
    #[sql_type = "SQL_EmbedImage"]
    SQL_EmbedImage, DbEmbedImage,
    height -> Int8 : i64,
    width -> Int8 : i64,
    proxy_url -> Text : String,
    url -> Text : String,
}

#[derive(Debug, SqlType)]
#[postgres(type_name = "__t_embed_provider")]
pub struct SQL_EmbedProvider;

composite_type! {
    #[derive(PartialEq,Eq,Debug)]
    #[sql_type = "SQL_EmbedProvider"]
    SQL_EmbedProvider, DbEmbedProvider,
    name -> Text : String,
    url -> Nullable<Text> : Option<String>,
}

#[derive(Debug, SqlType)]
#[postgres(type_name = "__t_embed_video")]
pub struct SQL_EmbedVideo;

composite_type! {
    #[derive(PartialEq,Eq,Debug)]
    #[sql_type = "SQL_EmbedVideo"]
    SQL_EmbedVideo, DbEmbedVideo,
    height -> Int8 : i64,
    width -> Int8 : i64,
    url -> Nullable<Text> : Option<String>,
}

#[derive(Debug, SqlType)]
#[postgres(type_name = "__t_moment")]
pub struct SQL_Moment;

composite_type! {
    #[derive(PartialEq,Eq,Debug)]
    #[sql_type = "SQL_Moment"]
    SQL_Moment, DbMoment,
    session_id -> Int8 : i64,
    duration_secs -> Int8 : i64,
    duration_nanos -> Int8 : i64,
    datetime -> Timestamptz : chrono::DateTime<chrono::Utc>,
}

#[derive(Debug, SqlType)]
#[postgres(type_name = "__t_partial_member")]
pub struct SQL_PartialMember;

composite_type! {
    #[derive(PartialEq,Eq,Debug)]
    #[sql_type = "SQL_PartialMember"]
    SQL_PartialMember, DbPartialMember,
    deaf -> Bool : bool,
    joined_at -> Timestamptz : chrono::DateTime<chrono::Utc>,
    mute -> Bool : bool,
    roles -> Array<SQL_Snowflake> : Vec<Snowflake>,
}

#[derive(Debug, SqlType)]
#[postgres(type_name = "__t_serenity_current_user")]
pub struct SQL_SerenityCurrentUser;

composite_type! {
    #[derive(PartialEq,Eq,Debug)]
    #[sql_type = "SQL_SerenityCurrentUser"]
    SQL_SerenityCurrentUser, DbSerenityCurrentUser,
    inner_user -> SQL_DiscordUser : DbDiscordUser,
    email -> Nullable<Text> : Option<String>,
    mfa_enabled -> Bool : bool,
    verified -> Bool : bool,
}

#[derive(Debug, SqlType)]
#[postgres(type_name = "__t_user_presence_game")]
pub struct SQL_UserPresenceGame;

composite_type! {
    #[derive(PartialEq,Eq,Debug)]
    #[sql_type = "SQL_UserPresenceGame"]
    SQL_UserPresenceGame, DbUserPresenceGame,
    kind -> Text : String,
    name -> Text : String,
    url -> Nullable<Text> : Option<String>,
}

/*impl FromSql<SQL_DiscordUser, Pg> for DbDiscordUser {
    fn from_sql(&self, bytes: Option<&[u8]>) -> deserialize::Result<Self> {
        let (
        let bytes = not_none!(bytes);

        let fixed_types_len = PG_UUID_BYTE_LEN;
        let amount_len = bytes.len() - fixed_types_len;

        let amount = &bytes[0..(amount_len - 1)];
        let currency = &bytes[amount_len..];

        Ok(Entry {
            amount: FromSql::<sql_types::Decimal, Pg>::from_sql(Some(amount))?,
            currency: FromSql::<sql_types::Uuid, Pg>::from_sql(Some(currency))?,
        })
    }
}*/

/*#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "__t_discord_user")]
pub struct DbDiscordUserInner {
    pub discord_id: DbSnowflake,
    pub avatar: Option<String>,
    pub is_bot: bool,
    pub discriminator: i16,
    #[postgres(name = "user_name")]
    pub name: String,
}

#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "discord_user")]
pub struct DbDiscordUser(pub DbDiscordUserInner);*/

impl From<User> for DbDiscordUser {
    fn from(u:User) -> Self {
        DbDiscordUser{
            discord_id: Snowflake(u.id.into()),
            avatar: u.avatar.filter_null(),
            is_bot: u.bot,
            discriminator: u.discriminator as i16,
            name: u.name.filter_null(),
        }
    }
}
/*
#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "__t_partial_member")]
pub struct DbPartialMemberInner {
    pub deaf: bool,
    pub joined_at: Option<chrono::DateTime<chrono::Utc>>,
    pub mute: bool,
    pub roles: Vec<DbSnowflake>,
}

#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "partial_member")]
pub struct DbPartialMember(pub DbPartialMemberInner);

impl From<PartialMember> for DbPartialMember {
    fn from(pm:PartialMember) -> Self {
        DbPartialMember(DbPartialMemberInner{
            deaf: pm.deaf,
            joined_at: pm.joined_at.map(|t| t.with_timezone(&chrono::Utc)),
            mute: pm.mute,
            roles: pm.roles.iter().map(Into::into).collect(),
        })
    }
}

#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "__t_embed_author")]
pub struct DbEmbedAuthorInner{
    pub icon_url: Option<String>,
    pub name: String,
    pub proxy_icon_url: Option<String>,
    pub url: Option<String>,
}
#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "embed_author")]
pub struct DbEmbedAuthor(pub DbEmbedAuthorInner);

impl From<EmbedAuthor> for DbEmbedAuthor {
    fn from(ea: EmbedAuthor) -> Self {
        DbEmbedAuthor(DbEmbedAuthorInner{
            icon_url: ea.icon_url.filter_null(),
            name: ea.name.filter_null(),
            proxy_icon_url: ea.proxy_icon_url.filter_null(),
            url: ea.url.filter_null(),
        })
    }
}

#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "__t_embed_footer")]
pub struct DbEmbedFooterInner{
    pub icon_url: Option<String>,
    pub proxy_icon_url: Option<String>,
    pub text: String,
}
#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "embed_footer")]
pub struct DbEmbedFooter(pub DbEmbedFooterInner);

impl From<EmbedFooter> for DbEmbedFooter{
    fn from(ef: EmbedFooter) -> Self {
        DbEmbedFooter(DbEmbedFooterInner{
            icon_url: ef.icon_url.filter_null(),
            proxy_icon_url: ef.proxy_icon_url.filter_null(),
            text: ef.text.filter_null(),
        })
    }
}

#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "__t_embed_image")]
pub struct DbEmbedImageInner{
    pub height: i64,
    pub width: i64,
    pub proxy_url: String,
    pub url: String,
}
#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "embed_image")]
pub struct DbEmbedImage(pub DbEmbedImageInner);

impl From<EmbedImage> for DbEmbedImage {
    fn from(ei: EmbedImage) -> Self {
        DbEmbedImage(DbEmbedImageInner{
            height: ei.height as i64,
            width: ei.width as i64,
            proxy_url: ei.proxy_url.filter_null(),
            url: ei.url.filter_null(),
        })
    }
}

impl From<EmbedThumbnail> for DbEmbedImage {
    fn from(ei: EmbedThumbnail) -> Self {
        DbEmbedImage(DbEmbedImageInner{
            height: ei.height as i64,
            width: ei.width as i64,
            proxy_url: ei.proxy_url.filter_null(),
            url: ei.url.filter_null(),
        })
    }
}

#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "__t_embed_provider")]
pub struct DbEmbedProviderInner{
    pub name: String,
    pub url: Option<String>,
}
#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "embed_provider")]
pub struct DbEmbedProvider(pub DbEmbedProviderInner);

impl From<EmbedProvider> for DbEmbedProvider {
    fn from(ep: EmbedProvider) -> Self {
        DbEmbedProvider(DbEmbedProviderInner{
            name: ep.name.filter_null(),
            url: ep.url.filter_null(),
        })
    }
}

#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "__t_embed_video")]
pub struct DbEmbedVideoInner{
    pub height: i64,
    pub width: i64,
    pub url: String,
}
#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "embed_video")]
pub struct DbEmbedVideo(pub DbEmbedVideoInner);

impl From<EmbedVideo> for DbEmbedVideo {
    fn from(ev: EmbedVideo) -> Self {
        DbEmbedVideo(DbEmbedVideoInner{
            height: (ev.height as i64),
            width: (ev.width as i64),
            url: ev.url.filter_null(),
        })
    }
}

#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "discord_colour")]
pub struct DbDiscordColour(pub i64);

impl From<Colour> for DbDiscordColour {
    fn from(c: Colour) -> Self {
        DbDiscordColour(i64::from(c.0))
    }
}       

#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "__t_serenity_current_user")]
pub struct DbCurrentUserInner{
    pub inner_user: DbDiscordUser,
    pub email: Option<String>,
    pub mfa_enabled: bool,
    pub verified: bool,
}
#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "serenity_current_user")]
pub struct DbCurrentUser(pub DbCurrentUserInner);

impl From<CurrentUser> for DbCurrentUser {
    fn from(cu: CurrentUser) -> Self {
        DbCurrentUser(DbCurrentUserInner{
            inner_user: DbDiscordUser(DbDiscordUserInner{
                discord_id: cu.id.into(),
                avatar: cu.avatar.filter_null(),
                is_bot: cu.bot,
                discriminator: cu.discriminator as i16,
                name: cu.name.filter_null(),
            }),
            email: cu.email.filter_null(),
            mfa_enabled: cu.mfa_enabled,
            verified: cu.verified,
        })
    }
}

#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "__t_user_presence_game")]
pub struct DbUserPresenceGameInner{
    pub kind: String,
    pub name: String,
    pub url: Option<String>,
}
#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "user_presence_game")]
pub struct DbUserPresenceGame(pub DbUserPresenceGameInner);

impl From<Activity> for DbUserPresenceGame {
    fn from(g: Activity) -> Self {
        DbUserPresenceGame(DbUserPresenceGameInner{
            kind: g.kind.into_str().to_string(),
            name: g.name.filter_null(),
            url:  g.url.filter_null() ,
        })
    }
}
*/
#[derive(Debug,Clone,Copy)]
pub enum GuildParentId {
    Ready(i64),
    CreateEvent(i64),
}

impl GuildParentId {
    pub fn as_ready(&self) -> Option<i64> {
        match self {
            GuildParentId::Ready(v) => Some(*v),
            _ => None,
        }
    }
    
    pub fn as_create_event(&self) -> Option<i64> {
        match self {
            GuildParentId::CreateEvent(v) => Some(*v),
            _ => None,
        }
    }
}    
/*
#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "__t_moment")]
pub struct DbMomentInner{
    session_id: i64,
    duration_secs: i64,
    duration_nanos: i32,
    datetime: chrono::DateTime<chrono::Utc>,
}

#[derive(Clone,Debug,ToSql,FromSql)]
#[postgres(name = "moment")]
pub struct DbMoment(pub DbMomentInner);

impl DbMoment {
    #[inline]
    pub fn now(session_id: i64, beginning_of_time: std::time::Instant) -> Self {
        let duration = beginning_of_time.elapsed();
        let ts = chrono::Utc::now();
        Self::from_pieces(session_id, ts, duration)
    }

    pub fn from_pieces(session_id: i64, chrono: chrono::DateTime<chrono::Utc>, duration: std::time::Duration) -> Self {
        Self(DbMomentInner{
            session_id,
            duration_secs: duration.as_secs() as i64,
            duration_nanos: duration.subsec_nanos() as i32,
            datetime: chrono
        })
    }

    #[allow(dead_code)]
    pub fn from_rawevent(session_id: i64, beginning_of_time: std::time::Instant, ev: serenity::model::event::WsEvent) -> Self {
        let duration = ev.happened_at_instant.duration_since(beginning_of_time);
        Self::from_pieces(session_id, ev.happened_at_chrono, duration)
    }
}
*/
