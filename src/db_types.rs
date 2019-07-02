use crate::serenity::model::prelude::*;
use crate::serenity::client::bridge::gateway::ShardId;
use crate::serenity::utils::Colour;
use crate::pg::types::{IsNull,Type,ToSql,FromSql};
use crate::FilterExt;
use crate::EnumIntoString;

use std::error::Error;
use std::fmt::Debug;

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
snowflake_impl!{ShardId}

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
        let val_i:i64 = i64::from_le_bytes(val_u.to_le_bytes());
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
        let val_u:u64 = u64::from_le_bytes(val_i.to_le_bytes());
        Ok(Self(serenity::model::permissions::Permissions::from_bits_truncate(val_u)))
    }

    fn accepts(ty: &pg::types::Type) -> bool {
        <i64 as FromSql>::accepts(ty)
    }
}

#[derive(Copy,Clone,Debug,FromSql,ToSql)]
#[postgres(name = "snowflake")]
pub struct DbSnowflake(i64);

impl DbSnowflake {
    pub fn into_inner(self) -> i64 { self.0 }
}

impl From<i64> for DbSnowflake {
    fn from(i:i64) -> Self{
        assert!(i >= 0);
        Self(i)
    }
}

impl From<u64> for DbSnowflake {
    fn from(u:u64) -> Self{
        assert!(u <= (std::i64::MAX as u64));
        Self(u as i64)
    }
}

impl From<DbSnowflake> for i64 {
    fn from(s:DbSnowflake) -> Self{
        s.into_inner()
    }
}

impl<'a> From<String> for DbSnowflake {
    fn from(s: String) -> Self {
        DbSnowflake(s.parse().unwrap())
    }
}

impl<T:crate::Snowflake> From<T> for DbSnowflake {
    fn from(t: T) -> Self {
        t.get_snowflake_i64().into()
    }
}

#[derive(Clone,Debug,ToSql,FromSql)]
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
pub struct DbDiscordUser(pub DbDiscordUserInner);

impl From<User> for DbDiscordUser {
    fn from(u:User) -> Self {
        DbDiscordUser(DbDiscordUserInner{
            discord_id: u.id.into(),
            avatar: u.avatar.filter_null(),
            is_bot: u.bot,
            discriminator: u.discriminator as i16,
            name: u.name.filter_null(),
        })
    }
}

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

impl From<Game> for DbUserPresenceGame {
    fn from(g: Game) -> Self {
        DbUserPresenceGame(DbUserPresenceGameInner{
            kind: g.kind.into_str().to_string(),
            name: g.name.filter_null(),
            url:  g.url.filter_null() ,
        })
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
    pub fn from_rawevent(session_id: i64, beginning_of_time: std::time::Instant, ev: serenity::model::event::RawEvent) -> Self {
        let duration = ev.happened_at_instant.duration_since(beginning_of_time);
        Self::from_pieces(session_id, ev.happened_at_chrono, duration)
    }
}
