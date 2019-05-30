use crate::serenity::prelude::*;
use crate::serenity::model::prelude::*;
use crate::serenity::utils::Colour;
use crate::FilterExt;
use crate::EnumIntoString;

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
        DbDiscordColour(c.0 as i64)
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
