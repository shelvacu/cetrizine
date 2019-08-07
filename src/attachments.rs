use crate::sha2::{Sha256, Digest};
use crate::reqwest::{
    self,
    header::{self, HeaderMap},
    Client,
};
use crate::hex;
use crate::schema;
use crate::CetrizineError;
use crate::diesel::prelude::*;
use std::io::Read;
use std::sync::Arc;
use std::convert::TryInto;
use serde::de::Deserialize;
use serenity::model::channel::Channel;

#[derive(Queryable, Debug)]
pub struct RawMessage {
    rowid: i64,
    recvd_at_datetime: chrono::DateTime<chrono::Utc>,
    recvd_at_duration_secs: i64,
    recvd_at_duration_nanos: i32,
    session_rowid: i64,
    kind: String,
    content_text: Option<String>,
    content_binary: Option<Vec<u8>>,
    downloaded_any_files: bool,
}

pub fn build_client() -> Client {
    let mut headers = header::HeaderMap::new();
    headers.insert(
        header::USER_AGENT,
        header::HeaderValue::from_static(
            concat!(
                env!("CARGO_PKG_NAME"),
                " (https://github.com/shelvacu/cetrizine, ",
                env!("CARGO_PKG_VERSION"),
                ")",
            )
        )
    );
    Client::builder()
        .default_headers(headers)
        .gzip(true)
        .connect_timeout(Some(std::time::Duration::from_secs(30)))
        //.h2_prior_knowledge()
        .build()
        .unwrap()
}

fn urls_from_user(user: &serenity::model::user::User, urls: &mut Vec<String>) {
    urls.push(user.default_avatar_url());
}

fn urls_from_channel(chan: Channel, urls: &mut Vec<String>) {
    match chan {
        Channel::Group(group_lock) => {
            let group = group_lock.read();
            if let Some(url) = &group.icon {
                urls.push(url.clone());
            }
        },
        Channel::Guild(_guild_chan_lock) => (),
        Channel::Private(private_chan_lock) => {
            let private_chan = private_chan_lock.read();
            urls_from_user(&*private_chan.recipient.read(), urls);
        },
        Channel::Category(_category_lock) => (),
    }
}

fn urls_from_partial_guild(guild: &serenity::model::guild::PartialGuild, urls: &mut Vec<String>) {
    for emoji in guild.emojis.values() {
        urls.push(emoji.url());
    }
    if let Some(icon_url) = guild.icon_url() {
        urls.push(icon_url);
    }
    if let Some(splash_url) = guild.splash_url() {
        urls.push(splash_url);
    }
    if let Some(banner_url) = guild.banner_url() {
        urls.push(banner_url);
    }
}

fn urls_from_guild(guild: &serenity::model::guild::Guild, urls: &mut Vec<String>) {
    for chan in guild.channels.values() {
        urls_from_channel(Channel::Guild(Arc::clone(chan)), urls);
    }
    for member in guild.members.values() {
        urls_from_user(&*member.user.read(), urls);
    }
    urls_from_partial_guild(&guild.clone().into(), urls);
}

fn urls_from_embed(embed: &serenity::model::channel::Embed, urls: &mut Vec<String>) {
    if let Some(author) = embed.author {
        if let Some(url) = author.icon_url {
            urls.push(url);
        }
        if let Some(url) = author.url {
            urls.push(url);
        }
    }
    if let Some(Some(icon_url)) = embed.footer.map(|f| f.icon_url) {
        urls.push(icon_url);
    }
    if let Some(image) = embed.image {
        urls.push(image.url);
    }
    if let Some(provider_url) = embed.provider.map(|p| p.url).flatten() {
        urls.push(provider_url);
    }
    if let Some(thumbnail) = embed.thumbnail {
        urls.push(thumbnail.url);
    }
    if let Some(url) = embed.url {
        urls.push(url);
    }
    if let Some(video) = embed.video {
        urls.push(video.url);
    }
}

fn urls_from_message(msg: &serenity::model::channel::Message, urls: &mut Vec<String>) {
    for attachment in msg.attachments {
        urls.push(attachment.url.clone());
    }
    urls_from_user(&msg.author, urls);
    //todo: is there any scanning I want to do of the message contents?
    for embed in msg.embeds {
        urls_from_embed(&embed, urls);
    }
    for mention in msg.mentions {
        urls_from_user(&mention, urls);
    }
    //todo: download images for any reactions
}

fn urls_from_message_update_event(msg: &serenity::model::event::MessageUpdateEvent, urls: &mut Vec<String>) {
    if let Some(attachments) = msg.attachments {
        for attachment in attachments {
            urls.push(attachment.url.clone());
        }
    }
    if let Some(embeds) = msg.embeds{
        for embed in embeds {
            urls_from_embed(&embed, urls);
        }
    }
    if let Some(mentions) = msg.mentions {
        for mention in mentions {
            urls_from_user(&mention, urls);
        }
    }
}

fn urls_from_presence(presence: &serenity::model::gateway::Presence, urls: &mut Vec<String>) {
    if let Some(user) = presence.user {urls_from_user(&*user.read(), &mut urls);}
}

pub fn archive_all_from_ws_message(
    conn: &diesel::pg::PgConnection,
    client: &mut Client,
    session_id: i64,
    beginning_of_time: std::time::Instant,
    raw_message_rowid: i64,
    msg: &RawMessage,
) -> Result<(), crate::CetrizineError>{
    use crate::serenity::model::event::GatewayEvent;
    use serenity::model::event::Event::*;
    if msg.downloaded_any_files {
        warn!("got sent a rawmessage that is marked as already having downloaded messages???");
        return Ok(())
    }

    let ev:GatewayEvent = if let Some(bytes) = &msg.content_binary {
        serde_json::from_reader(flate2::bufread::ZlibDecoder::new(&bytes[..]))?
    } else if let Some(text) = &msg.content_text {
        serde_json::from_str(&text)?
    } else {
        panic!("expected either content_binary or content_text to be non-null")
    };

    let mut urls:Vec<String> = Vec::new();

    if let GatewayEvent::Dispatch(_, event) = ev {
        match event {
            Raw => (),
            ChannelCreate(ev) => urls_from_channel(ev.channel, &mut urls),
            ChannelDelete(ev) => urls_from_channel(ev.channel, &mut urls),
            ChannelPinsUpdate(_) => (),
            ChannelRecipientAdd(ev) => urls_from_user(&ev.user, &mut urls),
            ChannelRecipientRemove(ev) => urls_from_user(&ev.user, &mut urls),
            ChannelUpdate(ev) => urls_from_channel(ev.channel, &mut urls),
            GuildBanAdd(ev) => urls_from_user(&ev.user, &mut urls),
            GuildBanRemove(ev) => urls_from_user(&ev.user, &mut urls),
            GuildCreate(ev) => urls_from_guild(&ev.guild, &mut urls),
            GuildDelete(ev) => urls_from_partial_guild(&ev.guild, &mut urls),
            GuildEmojisUpdate(ev) => for emoji in ev.emojis.values() {urls.push(emoji.url());},
            GuildIntegrationsUpdate(_) => (),
            GuildMemberAdd(ev) => urls_from_user(&*ev.member.user.read(), &mut urls),
            GuildMemberRemove(ev) => urls_from_user(&ev.user, &mut urls),
            GuildMemberUpdate(ev) => urls_from_user(&ev.user, &mut urls),
            GuildMembersChunk(ev) => for member in ev.members.values() {urls_from_user(&*member.user.read(), &mut urls);},
            GuildRoleCreate(_) => (),
            GuildRoleDelete(_) => (),
            GuildRoleUpdate(_) => (),
            GuildUnavailable(_) => (),
            GuildUpdate(ev) => urls_from_partial_guild(&ev.guild, &mut urls),
            MessageCreate(ev) => urls_from_message(&ev.message, &mut urls),
            MessageDelete(_) => (),
            MessageDeleteBulk(_) => (),
            MessageUpdate(ev) => urls_from_message_update_event(&ev, &mut urls),
            PresenceUpdate(ev) => urls_from_presence(&ev.presence, &mut urls),
            PresencesReplace(ev) => for presence in ev.presences {urls_from_presence(&presence, &mut urls);},
            ReactionAdd(ev) => 
        }
    }

    Ok(())
}

pub fn archive_attachments(
    pool: &crate::ArcPool,
    session_id: i64,
    beginning_of_time: std::time::Instant,
) -> Result<(), crate::CetrizineError>{
    use schema::{
        download_data::dsl as dd_dsl,
        download::dsl as d_dsl,
        attachment::dsl as a_dsl,
        download_header::dsl as dh_dsl,
    };

    #[derive(Queryable)]
    //#[table_name = "attachment"]
    struct AttachmentData {
        rowid: i64,
        url: String,
    }

    let conn = pool.get()?;

    let client = build_client();

    loop {
        let res:Vec<AttachmentData> = a_dsl::attachment.select((
            a_dsl::rowid,
            a_dsl::url,
        )).filter(
            a_dsl::download_rowid.is_null()
        ).order(a_dsl::rowid.asc()).limit(100).get_results(&conn)?;
        for ad in &res {
            let maybe_download_rowid:Option<i64> = d_dsl::download.select(
                d_dsl::rowid,
            ).filter(
                (
                    d_dsl::url.eq(&ad.url)
                ).and(
                    d_dsl::has_headers.eq(true)
                )
            ).limit(1).get_result(&conn).optional()?;
            if let Some(download_rowid) = maybe_download_rowid {
                diesel::update(a_dsl::attachment.filter(
                    a_dsl::rowid.eq(&ad.rowid)
                )).set(
                    a_dsl::download_rowid.eq(download_rowid)
                ).execute(&conn)?;
            }else{
                let url:&str = &ad.url;
                let response_code:Option<i16>;
                let success:bool;
                let download_data_rowid:Option<i64>;
                let headers:HeaderMap;
                let downloaded_at = crate::db_types::DbMoment::now(session_id, beginning_of_time);
                match client.get(url).send() {
                    Ok(mut response) => {
                        response_code = Some(response.status().as_u16().try_into().unwrap());
                        success = true;
                        let mut data:Vec<u8> = Vec::new();
                        response.read_to_end(&mut data)?;
                        let sum = hex::encode_upper({
                            let mut s = Sha256::default();
                            s.input(&data);
                            s
                        }.result());
                        let maybe_existing_data_rowid:Option<i64> = dd_dsl::download_data.select(
                            dd_dsl::rowid
                        ).filter(
                            dd_dsl::sha256sum_hex.eq(&sum)
                        ).order(dd_dsl::rowid.asc()).limit(1).get_result(&*conn).optional()?;
                        if let Some(existing_rowid) = maybe_existing_data_rowid {
                            download_data_rowid = Some(existing_rowid);
                            info!("Downloaded and found    {}, size is {}, sum is {}", &url, data.len(), &sum);
                        } else {
                            download_data_rowid = Some(diesel::insert_into(dd_dsl::download_data).values((
                                dd_dsl::sha256sum_hex.eq(&sum),
                                dd_dsl::data.eq(&data),
                                dd_dsl::downloaded_at.eq(downloaded_at.clone()),
                            )).returning(dd_dsl::rowid).get_result(&conn)?);
                            info!("Downloaded and inserted {}, size is {}, sum is {}", &url, data.len(), &sum);
                        }
                        headers = response.headers().clone();
                    },
                    Err(err) => {
                        response_code = err.status().map(|s| s.as_u16().try_into().unwrap());
                        success = false;
                        download_data_rowid = None;
                        warn!("Failed to download {}, Reqwest err is {:?}", &url, err);
                        headers = HeaderMap::new();
                    }
                }
                conn.transaction(|| {
                    let maybe_content_type = headers.get(header::CONTENT_TYPE).map(|v| v.to_str().ok()).flatten();
                    let download_rowid:i64 = diesel::insert_into(d_dsl::download).values((
                        d_dsl::url.eq(url),
                        d_dsl::response_code.eq(response_code),
                        d_dsl::success.eq(success),
                        d_dsl::download_data_rowid.eq(download_data_rowid),
                        d_dsl::downloaded_at.eq(downloaded_at),
                        d_dsl::has_headers.eq(true),
                        d_dsl::content_type.eq(maybe_content_type)
                    )).returning(d_dsl::rowid).get_result(&conn)?;
                    for (key, val) in headers.iter() {
                        diesel::insert_into(dh_dsl::download_header).values((
                            dh_dsl::download_rowid.eq(download_rowid),
                            dh_dsl::header_name.eq(key.as_str()),
                            dh_dsl::header_value.eq(val.as_bytes()),
                        )).execute(&conn)?;
                    }
                    diesel::update(a_dsl::attachment.filter(
                        a_dsl::rowid.eq(&ad.rowid)
                    )).set(
                        a_dsl::download_rowid.eq(download_rowid)
                    ).execute(&conn)?;
                    Ok(()):Result<(),CetrizineError>
                })?;
            }
        } //for ad in &res
        if res.is_empty() { return Ok(()) }
    } //loop
    //Ok(()) //unreachable
}
