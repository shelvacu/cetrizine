use crate::serenity::model::prelude::*;
use crate::serenity::client::bridge::gateway::ShardId;
use crate::serenity::utils::Colour;
use crate::diesel::{
    sql_types::*,
    backend::Backend,
    serialize::{self,ToSql,Output},
    deserialize::{self,FromSql},
    pg::Pg,
};
use crate::FilterExt;
use crate::EnumIntoString;

use std::error::Error;
use std::fmt::Debug;
use std::io::Write;

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

        impl From<Snowflake> for $klass {
            fn from(s: Snowflake) -> Self {
                use std::convert::TryInto;
                <Self as std::convert::From<u64>>::from(s.0.try_into().unwrap())
            }
        }

        /* impl FromSql<SQL_Snowflake, Pg> for $klass {
            fn from_sql(bytes: Option<&[u8]>) -> deserialize::Result<Self> {
                <Snowflake as FromSql::<SQL_Snowflake, Pg>>::from_sql(bytes).map(Self::from)
            }
        } */
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

#[derive(Debug,AsExpression)]
#[sql_type = "BigInt"]
#[sql_type = "SQL_Snowflake"]
pub struct SmartHaxO<T: Debug>(pub Option<T>);


impl<DB, T> ToSql<Int8, DB> for SmartHaxO<T>
where
    DB: Backend,
    i64: ToSql<Int8, DB>,
    T: InnerSnowflake + Debug,
{
    fn to_sql<W: Write>(&self, out: &mut Output<W, DB>) -> serialize::Result {
        (self.0.as_ref().map(|a| a.get_snowflake_i64())).to_sql(out)
    }
}

impl<DB, T> ToSql<SQL_Snowflake, DB> for SmartHaxO<T>
where
    DB: Backend,
    i64: ToSql<Int8, DB>,
    T: InnerSnowflake + Debug,
{
    fn to_sql<W: Write>(&self, out: &mut Output<W, DB>) -> serialize::Result {
        (self.0.as_ref().map(|a| a.get_snowflake_i64())).to_sql(out)
    }
}

#[derive(Debug,AsExpression)]
#[sql_type = "BigInt"]
#[sql_type = "SQL_Snowflake"]
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

impl<DB, T> ToSql<SQL_Snowflake, DB> for SmartHax<T>
where
    DB: Backend,
    i64: ToSql<Int8, DB>,
    T: InnerSnowflake + Debug,
{
    fn to_sql<W: Write>(&self, out: &mut Output<W, DB>) -> serialize::Result {
        (self.0.get_snowflake_i64()).to_sql(out)
    }
}

#[derive(Debug,AsExpression)]
#[sql_type = "BigInt"]
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
            fn to_sql<W: ::std::io::Write>(&self, their_out: &mut ::diesel::serialize::Output<W, ::diesel::pg::Pg>) -> ::diesel::serialize::Result {
                // let mut out_buf = ;
                // let mut our_out = diesel::serialize::Output::new(out_buf, their_out.metadata_lookup());
                let mut our_out = their_out.with_buffer(Vec::<u8>::new());
                let res = ::diesel::serialize::WriteTuple::<($($sql_ty,)+)>::write_tuple(
                    &( $( &self . $rust_column_name , )+ ),
                    &mut our_out,
                )?;
                let out_buf = our_out.into_inner();
                // debug!("ToSql'd {:?} into {:x?}, with res {:?}", self, out_buf, res);
                their_out.write_all(&out_buf)?;
                Ok(res)
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

#[derive(Debug, SqlType, QueryId)]
#[postgres(type_name = "snowflake")]
pub struct SQL_Snowflake;

#[derive(Debug, FromSqlRow, AsExpression, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
#[sql_type = "SQL_Snowflake"]
pub struct Snowflake(pub i64);

impl ToSql<SQL_Snowflake, Pg> for Snowflake {
    fn to_sql<W: Write>(&self, out: &mut serialize::Output<W, Pg>) -> serialize::Result {
        <i64 as ToSql<Int8, Pg>>::to_sql(&self.0, out)
    }
}

impl FromSql<SQL_Snowflake, Pg> for Snowflake {
    fn from_sql(bytes: Option<&[u8]>) -> deserialize::Result<Self> {
        <i64 as FromSql::<Int8, Pg>>::from_sql(bytes).map(Snowflake)
    }
}

impl<T> From<T> for Snowflake
where T: InnerSnowflake {
    fn from(val: T) -> Self {
        Self(val.get_snowflake_i64())
    }
}

impl From<Snowflake> for i64 {
    fn from(sf: Snowflake) -> Self {
        sf.0
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
        <i64 as FromSql::<Int8, Pg>>::from_sql(bytes).map(DiscordColour)
    }
}

impl From<u32> for DiscordColour {
    fn from(val: u32) -> Self {
        Self(val.into():i64)
    }
}

#[derive(Debug, SqlType)]
#[postgres(type_name = "discord_user")]
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
#[postgres(type_name = "embed_author")]
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
#[postgres(type_name = "embed_footer")]
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
#[postgres(type_name = "embed_image")]
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
#[postgres(type_name = "embed_provider")]
pub struct SQL_EmbedProvider;

composite_type! {
    #[derive(PartialEq,Eq,Debug)]
    #[sql_type = "SQL_EmbedProvider"]
    SQL_EmbedProvider, DbEmbedProvider,
    name -> Text : String,
    url -> Nullable<Text> : Option<String>,
}

#[derive(Debug, SqlType)]
#[postgres(type_name = "embed_video")]
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
#[postgres(type_name = "moment")]
pub struct SQL_Moment;

composite_type! {
    #[derive(PartialEq,Eq,Debug,Clone)]
    #[sql_type = "SQL_Moment"]
    SQL_Moment, DbMoment,
    session_id -> Int8 : i64,
    duration_secs -> Int8 : i64,
    duration_nanos -> Int4 : i32,
    datetime -> Timestamptz : chrono::DateTime<chrono::Utc>,
}

#[derive(Debug, SqlType)]
#[postgres(type_name = "partial_member")]
pub struct SQL_PartialMember;

composite_type! {
    #[derive(PartialEq,Eq,Debug)]
    #[sql_type = "SQL_PartialMember"]
    SQL_PartialMember, DbPartialMember,
    deaf -> Bool : bool,
    joined_at -> Nullable<Timestamptz> : Option<chrono::DateTime<chrono::Utc>>,
    mute -> Bool : bool,
    roles -> Array<SQL_Snowflake> : Vec<Snowflake>,
}

#[derive(Debug, SqlType, QueryId)]
#[postgres(type_name = "serenity_current_user")]
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
#[postgres(type_name = "user_presence_game")]
pub struct SQL_UserPresenceGame;

composite_type! {
    #[derive(PartialEq,Eq,Debug)]
    #[sql_type = "SQL_UserPresenceGame"]
    SQL_UserPresenceGame, DbUserPresenceGame,
    kind -> Text : String,
    name -> Text : String,
    url -> Nullable<Text> : Option<String>,
}

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

impl From<PartialMember> for DbPartialMember {
    fn from(pm:PartialMember) -> Self {
        DbPartialMember{
            deaf: pm.deaf,
            joined_at: pm.joined_at.map(|t| t.with_timezone(&chrono::Utc)),
            mute: pm.mute,
            roles: pm.roles.iter().map(Into::into).collect(),
        }
    }
}

impl From<EmbedAuthor> for DbEmbedAuthor {
    fn from(ea: EmbedAuthor) -> Self {
        DbEmbedAuthor{
            icon_url: ea.icon_url.filter_null(),
            name: ea.name.filter_null(),
            proxy_icon_url: ea.proxy_icon_url.filter_null(),
            url: ea.url.filter_null(),
        }
    }
}

impl From<EmbedFooter> for DbEmbedFooter{
    fn from(ef: EmbedFooter) -> Self {
        DbEmbedFooter{
            icon_url: ef.icon_url.filter_null(),
            proxy_icon_url: ef.proxy_icon_url.filter_null(),
            text: ef.text.filter_null(),
        }
    }
}

impl From<EmbedImage> for DbEmbedImage {
    fn from(ei: EmbedImage) -> Self {
        DbEmbedImage{
            height: ei.height as i64,
            width: ei.width as i64,
            proxy_url: ei.proxy_url.filter_null(),
            url: ei.url.filter_null(),
        }
    }
}

impl From<EmbedThumbnail> for DbEmbedImage {
    fn from(ei: EmbedThumbnail) -> Self {
        DbEmbedImage{
            height: ei.height as i64,
            width: ei.width as i64,
            proxy_url: ei.proxy_url.filter_null(),
            url: ei.url.filter_null(),
        }
    }
}

impl From<EmbedProvider> for DbEmbedProvider {
    fn from(ep: EmbedProvider) -> Self {
        DbEmbedProvider{
            name: ep.name.filter_null(),
            url: ep.url.filter_null(),
        }
    }
}

impl From<EmbedVideo> for DbEmbedVideo {
    fn from(ev: EmbedVideo) -> Self {
        DbEmbedVideo{
            height: (ev.height as i64),
            width: (ev.width as i64),
            url: Some(ev.url.filter_null()),
        }
    }
}

impl From<Colour> for DiscordColour {
    fn from(c: Colour) -> Self {
        DiscordColour(i64::from(c.0))
    }
}       

impl From<CurrentUser> for DbSerenityCurrentUser {
    fn from(cu: CurrentUser) -> Self {
        DbSerenityCurrentUser{
            inner_user: DbDiscordUser{
                discord_id: cu.id.into(),
                avatar: cu.avatar.filter_null(),
                is_bot: cu.bot,
                discriminator: cu.discriminator as i16,
                name: cu.name.filter_null(),
            },
            email: cu.email.filter_null(),
            mfa_enabled: cu.mfa_enabled,
            verified: cu.verified,
        }
    }
}

impl From<Activity> for DbUserPresenceGame {
    fn from(g: Activity) -> Self {
        DbUserPresenceGame{
            kind: g.kind.into_str().to_string(),
            name: g.name.filter_null(),
            url:  g.url.filter_null() ,
        }
    }
}

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

impl DbMoment {
    #[inline]
    pub fn now(session_id: i64, beginning_of_time: std::time::Instant) -> Self {
        let duration = beginning_of_time.elapsed();
        let ts = chrono::Utc::now();
        Self::from_pieces(session_id, ts, duration)
    }

    pub fn from_pieces(session_id: i64, chrono: chrono::DateTime<chrono::Utc>, duration: std::time::Duration) -> Self {
        Self{
            session_id,
            duration_secs: duration.as_secs() as i64,
            duration_nanos: duration.subsec_nanos() as i32,
            datetime: chrono
        }
    }

    #[allow(dead_code)]
    pub fn from_rawevent(session_id: i64, beginning_of_time: std::time::Instant, ev: serenity::model::event::WsEvent) -> Self {
        let duration = ev.happened_at_instant.duration_since(beginning_of_time);
        Self::from_pieces(session_id, ev.happened_at_chrono, duration)
    }
}
