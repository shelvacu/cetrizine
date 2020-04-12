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
use diesel::prelude::*;
use schema::{raw_message_url, raw_message};

#[derive(Queryable, Identifiable, Debug)]
#[primary_key(rowid)]
#[table_name = "raw_message"]
pub struct RawMessage {
    rowid: i64,
    recvd_at_datetime: chrono::DateTime<chrono::Utc>,
    recvd_at_duration_secs: i64,
    recvd_at_duration_nanos: i32,
    session_rowid: i64,
    kind: String,
    content_text: Option<String>,
    content_binary: Option<Vec<u8>>,
    scanned_for_urls: bool,
}

#[derive(Queryable, Identifiable, Debug)]
#[primary_key(raw_message_rowid, url)]
#[table_name = "raw_message_url"]
pub struct RawMessageUrl {
    raw_message_rowid: i64,
    url: String,
    been_downloaded: bool,
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
        //.gzip(true)
        .connect_timeout(std::time::Duration::from_secs(30))
        //.h2_prior_knowledge()
        .build()
        .unwrap()
}

trait UrlsInto {
    fn urls_into(&self, urls: &mut Vec<String>);
}

impl<T: UrlsInto> UrlsInto for Arc<T> {
    fn urls_into(&self, urls: &mut Vec<String>) {
        std::ops::Deref::deref(self).urls_into(urls);
        // (&*self).urls_into(urls);
    }
}

impl<T: UrlsInto> UrlsInto for serenity::prelude::RwLock<T> {
    fn urls_into(&self, urls: &mut Vec<String>) {
        self.read().urls_into(urls);
    }
}

impl<T: UrlsInto> UrlsInto for Option<T> {
    fn urls_into(&self, urls: &mut Vec<String>) {
        if let Some(inner) = self.as_ref() {
            inner.urls_into(urls);
        }
    }
}

impl<T: UrlsInto> UrlsInto for Vec<T> {
    fn urls_into(&self, urls: &mut Vec<String>) {
        for inner in self {
            inner.urls_into(urls);
        }
    }
}

trait DefinitelyNotUrlsInto {
    fn urls_into(self, urls: &mut Vec<String>);
}

impl<T> DefinitelyNotUrlsInto for T
where
    T: Iterator,
    T::Item: UrlsInto,
{
    fn urls_into(self, urls: &mut Vec<String>) {
        for inner in self {
            inner.urls_into(urls);
        }
    }
}

impl UrlsInto for str {
    fn urls_into(&self, urls: &mut Vec<String>) {
        urls.push(self.to_owned());
    }
}

impl UrlsInto for String {
    fn urls_into(&self, urls: &mut Vec<String>) {
        urls.push(self.to_owned());
    }
}

impl UrlsInto for serenity::model::user::User {
    fn urls_into(&self, urls: &mut Vec<String>) {
        urls.push(self.avatar_url().unwrap_or_else(|| self.default_avatar_url()));
    }
}

impl UrlsInto for serenity::model::channel::Group {
    fn urls_into(&self, urls: &mut Vec<String>) {
        self.icon_url().urls_into(urls);
    }
}

impl UrlsInto for serenity::model::channel::GuildChannel {
    fn urls_into(&self, _urls: &mut Vec<String>) {
        //do nothing
    }
}

impl UrlsInto for serenity::model::channel::PrivateChannel {
    fn urls_into(&self, urls: &mut Vec<String>) {
        self.recipient.urls_into(urls);
    }
}

impl UrlsInto for serenity::model::channel::ChannelCategory {
    fn urls_into(&self, _urls: &mut Vec<String>) {
        //do nothing
    }
}

impl UrlsInto for Channel {
    fn urls_into(&self, urls: &mut Vec<String>) {
        match self {
            Channel::Group(a) => a.urls_into(urls),
            Channel::Guild(a) => a.urls_into(urls),
            Channel::Private(a) => a.urls_into(urls),
            Channel::Category(a) => a.urls_into(urls),
        }
    }
}

impl UrlsInto for serenity::model::guild::Emoji {
    fn urls_into(&self, urls: &mut Vec<String>) {
        urls.push(self.url());
    }
}

impl UrlsInto for serenity::model::guild::Member {
    fn urls_into(&self, urls: &mut Vec<String>) {
        self.user.urls_into(urls);
    }
}

impl UrlsInto for serenity::model::guild::PartialGuild {
    fn urls_into(&self, urls: &mut Vec<String>) {
        self.emojis.values().cloned().urls_into(urls);
        self.icon_url().urls_into(urls);
        self.splash_url().urls_into(urls);
        self.banner_url().urls_into(urls);
    }
}

impl UrlsInto for serenity::model::guild::Guild {
    fn urls_into(&self, urls: &mut Vec<String>) {
        self.channels.values().cloned().urls_into(urls);
        self.members.values().cloned().urls_into(urls);
        let pg:serenity::model::guild::PartialGuild = self.clone().into();
        pg.urls_into(urls);
    }
}

impl UrlsInto for serenity::model::channel::EmbedAuthor {
    fn urls_into(&self, urls: &mut Vec<String>) {
        self.icon_url.urls_into(urls);
        self.url.urls_into(urls);
    }
}

impl UrlsInto for serenity::model::channel::EmbedField {
    fn urls_into(&self, _urls: &mut Vec<String>) {
        //do nothing
    }
}

impl UrlsInto for serenity::model::channel::EmbedFooter {
    fn urls_into(&self, urls: &mut Vec<String>) {
        self.icon_url.urls_into(urls);
    }
}

impl UrlsInto for serenity::model::channel::EmbedImage {
    fn urls_into(&self, urls: &mut Vec<String>) {
        self.url.urls_into(urls);
    }
}

impl UrlsInto for serenity::model::channel::EmbedProvider {
    fn urls_into(&self, urls: &mut Vec<String>) {
        self.url.urls_into(urls);
    }
}

impl UrlsInto for serenity::model::channel::EmbedThumbnail {
    fn urls_into(&self, urls: &mut Vec<String>) {
        self.url.urls_into(urls);
    }
}

impl UrlsInto for serenity::model::channel::EmbedVideo {
    fn urls_into(&self, urls: &mut Vec<String>) {
        self.url.urls_into(urls);
    }
}

impl UrlsInto for serenity::model::channel::Embed {
    fn urls_into(&self, urls: &mut Vec<String>) {
        self.author.urls_into(urls);
        self.footer.urls_into(urls);
        self.image.urls_into(urls);
        self.provider.urls_into(urls);
        self.thumbnail.urls_into(urls);
        self.url.urls_into(urls);
        self.video.urls_into(urls);
    }
}

impl UrlsInto for serenity::model::channel::Attachment {
    fn urls_into(&self, urls: &mut Vec<String>) {
        self.url.urls_into(urls);
    }
}

impl UrlsInto for serenity::model::channel::Message {
    fn urls_into(&self, urls: &mut Vec<String>) {
        self.attachments.urls_into(urls);
        self.author.urls_into(urls);
        //scan message?
        self.embeds.urls_into(urls);
        self.mentions.urls_into(urls);
        //reactions?
    }
}

impl UrlsInto for serenity::model::event::MessageUpdateEvent {
    fn urls_into(&self, urls: &mut Vec<String>) {
        self.attachments.urls_into(urls);
        self.author.urls_into(urls);
        //scan message?
        self.embeds.urls_into(urls);
        self.mentions.urls_into(urls);
        //reactions?
    }
}

impl UrlsInto for serenity::model::channel::Reaction {
    fn urls_into(&self, urls: &mut Vec<String>) {
        self.emoji.urls_into(urls);
    }
}

impl UrlsInto for serenity::model::channel::ReactionType {
    fn urls_into(&self, urls: &mut Vec<String>) {
        use serenity::model::channel::ReactionType::*;
        match self {
            Custom{animated, id, ..} => urls.push(id.url(*animated)),
            Unicode(_) => (),
        }
    }
}

impl UrlsInto for serenity::model::gateway::Presence {
    fn urls_into(&self, urls: &mut Vec<String>) {
        self.user.urls_into(urls);
    }
}

impl UrlsInto for serenity::model::guild::GuildStatus {
    fn urls_into(&self, urls: &mut Vec<String>) {
        use serenity::model::guild::GuildStatus::*;
        match self {
            OnlinePartialGuild(g) => g.urls_into(urls),
            OnlineGuild(g) => g.urls_into(urls),
            Offline(_) => (),
        }
    }
}

impl UrlsInto for serenity::model::gateway::Ready {
    fn urls_into(&self, urls: &mut Vec<String>) {
        self.guilds.urls_into(urls);
        self.presences.values().cloned().urls_into(urls);
        self.private_channels.values().cloned().urls_into(urls);
        self.user.urls_into(urls);
    }
}

impl UrlsInto for serenity::model::user::CurrentUser {
    fn urls_into(&self, urls: &mut Vec<String>) {
        self.default_avatar_url().urls_into(urls);
    }
}



pub fn scan_urls_from_ws_message(
    conn: &diesel::pg::PgConnection,
    msg: &RawMessage,
) -> Result<(), crate::CetrizineError>{
    use crate::serenity::model::event::GatewayEvent;
    use serenity::model::event::Event::*;
    use schema::{
        raw_message::dsl as rm_dsl,
        raw_message_url::dsl as rmu_dsl,
        download::dsl as d_dsl,
        download_data::dsl as dd_dsl,
    };
    if msg.scanned_for_urls {
        warn!("got sent a rawmessage that is marked as already having downloaded messages???");
        return Ok(())
    }
    trace!("Scanning {:?}", msg);

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
            ChannelCreate(ev) => ev.channel.urls_into(&mut urls),
            ChannelDelete(ev) => ev.channel.urls_into(&mut urls),
            ChannelPinsUpdate(_) => (),
            ChannelRecipientAdd(ev) => ev.user.urls_into(&mut urls),
            ChannelRecipientRemove(ev) => ev.user.urls_into(&mut urls),
            ChannelUpdate(ev) => ev.channel.urls_into(&mut urls),
            GuildBanAdd(ev) => ev.user.urls_into(&mut urls),
            GuildBanRemove(ev) => ev.user.urls_into(&mut urls),
            GuildCreate(ev) => ev.guild.urls_into(&mut urls),
            GuildDelete(ev) => ev.guild.urls_into(&mut urls),
            GuildEmojisUpdate(ev) => ev.emojis.values().cloned().urls_into(&mut urls),
            GuildIntegrationsUpdate(_) => (),
            GuildMemberAdd(ev) => ev.member.urls_into(&mut urls),
            GuildMemberRemove(ev) => ev.user.urls_into(&mut urls),
            GuildMemberUpdate(ev) => ev.user.urls_into(&mut urls),
            GuildMembersChunk(ev) => ev.members.values().cloned().urls_into(&mut urls),
            GuildRoleCreate(_) => (),
            GuildRoleDelete(_) => (),
            GuildRoleUpdate(_) => (),
            GuildUnavailable(_) => (),
            GuildUpdate(ev) => ev.guild.urls_into(&mut urls),
            MessageCreate(ev) => ev.message.urls_into(&mut urls),
            MessageDelete(_) => (),
            MessageDeleteBulk(_) => (),
            MessageUpdate(ev) => ev.urls_into(&mut urls),
            PresenceUpdate(ev) => ev.presence.urls_into(&mut urls),
            PresencesReplace(ev) => ev.presences.urls_into(&mut urls),
            ReactionAdd(ev) => ev.reaction.urls_into(&mut urls),
            ReactionRemove(ev) => ev.reaction.urls_into(&mut urls),
            ReactionRemoveAll(_) => (),
            Ready(ev) => ev.ready.urls_into(&mut urls),
            Resumed(_) => (),
            TypingStart(_) => (),
            UserUpdate(ev) => ev.current_user.urls_into(&mut urls),
            VoiceStateUpdate(_) => (),
            VoiceServerUpdate(_) => (),
            WebhookUpdate(_) => (),
            Unknown(_) => (),
        }
    }

    urls.sort_unstable();
    urls.dedup();

    if !urls.is_empty() {
        info!("Found {} urls after dedup.", urls.len());
    }

    conn.transaction(||{
        diesel::insert_into(rmu_dsl::raw_message_url)
            .values(urls.iter().map(|url| 
                (
                    rmu_dsl::raw_message_rowid.eq(msg.rowid),
                    rmu_dsl::url.eq(url),
                    rmu_dsl::been_downloaded.eq(false),
                )
            ).collect():Vec<_>)
            .execute(conn)?;
        diesel::update(msg).set(rm_dsl::scanned_for_urls.eq(true)).execute(conn)?;
        Ok(()):Result<(), CetrizineError>
    })?;
    Ok(())
}

pub fn download_scanned_url(
    conn: &diesel::pg::PgConnection,
    client: &mut Client,
    session_id: i64,
    beginning_of_time: std::time::Instant,
    url_row: RawMessageUrl,
) -> Result<(), CetrizineError>{
    use schema::{
        raw_message::dsl as rm_dsl,
        raw_message_url::dsl as rmu_dsl,
        download::dsl as d_dsl,
        download_data::dsl as dd_dsl,
        download_header::dsl as dh_dsl,
    };
    let url = &url_row.url;
    info!("Downloading {}", url);
    let maybe_download_rowid:Option<i64> = d_dsl::download.select(
            d_dsl::rowid,
        ).filter(
            (
                d_dsl::url.eq(url)
            ).and(
                d_dsl::has_headers.eq(true)
            )
        ).limit(1).get_result(conn).optional()?;
    let filter_dsl = schema::raw_message_url::table.filter(
            rmu_dsl::raw_message_rowid.eq(&url_row.raw_message_rowid).and(
                rmu_dsl::url.eq(&url_row.url)
            )
        );
    if maybe_download_rowid.is_some() {
        info!("Url {} already downloaded, marking.", url);
        diesel::update(filter_dsl).set(rmu_dsl::been_downloaded.eq(true)).execute(conn)?;
    }else{
        let response_code:Option<i16>;
        let success:bool;
        let download_data_rowid:Option<i64>;
        let headers:HeaderMap;
        let downloaded_at = crate::db_types::DbMoment::now(session_id, beginning_of_time);
        match futures::executor::block_on(client.get(url).send()) {
            Ok(response) => {
                response_code = Some(response.status().as_u16().try_into().unwrap());
                success = true;
                headers = response.headers().clone();
                let data = futures::executor::block_on(response.bytes())?;
                let sum = hex::encode_upper({
                    let mut s = Sha256::default();
                    s.input(&data);
                    s
                }.result());
                let maybe_existing_data_rowid:Option<i64> = dd_dsl::download_data.select(
                    dd_dsl::rowid
                ).filter(
                    dd_dsl::sha256sum_hex.eq(&sum)
                ).order(dd_dsl::rowid.asc()).limit(1).get_result(conn).optional()?;
                if let Some(existing_rowid) = maybe_existing_data_rowid {
                    download_data_rowid = Some(existing_rowid);
                    info!("Downloaded and found    {}, size is {}, sum is {}", &url, data.len(), &sum);
                } else {
                    download_data_rowid = Some(diesel::insert_into(dd_dsl::download_data).values((
                        dd_dsl::sha256sum_hex.eq(&sum),
                        dd_dsl::data.eq(&*data),
                        dd_dsl::downloaded_at.eq(downloaded_at.clone()),
                    )).returning(dd_dsl::rowid).get_result(conn)?);
                    info!("Downloaded and inserted {}, size is {}, sum is {}", &url, data.len(), &sum);
                }
            },
            Err(err) => {
                response_code = err.status().map(|s| s.as_u16().try_into().unwrap());
                success = false;
                download_data_rowid = None;
                warn!("Failed to download {}, Reqwest err is {:?}", &url, err);
                headers = HeaderMap::new();
            },
        }
        conn.transaction(|| {
            info!("Response code {:?}", response_code);
            let maybe_content_type = headers.get(header::CONTENT_TYPE).map(|v| v.to_str().ok()).flatten();
            let download_rowid:i64 = diesel::insert_into(d_dsl::download).values((
                d_dsl::url.eq(url),
                d_dsl::response_code.eq(response_code),
                d_dsl::success.eq(success),
                d_dsl::download_data_rowid.eq(download_data_rowid),
                d_dsl::downloaded_at.eq(downloaded_at),
                d_dsl::has_headers.eq(true),
                d_dsl::content_type.eq(maybe_content_type)
            )).returning(d_dsl::rowid).get_result(conn)?;
            for (key, val) in headers.iter() {
                diesel::insert_into(dh_dsl::download_header).values((
                    dh_dsl::download_rowid.eq(download_rowid),
                    dh_dsl::header_name.eq(key.as_str()),
                    dh_dsl::header_value.eq(val.as_bytes()),
                )).execute(conn)?;
            }

            diesel::update(filter_dsl).set(rmu_dsl::been_downloaded.eq(true)).execute(conn)?;

            Ok(()):Result<(),CetrizineError>
        })?;
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
                match futures::executor::block_on(client.get(url).send()) {
                    Ok(response) => {
                        headers = response.headers().clone();
                        response_code = Some(response.status().as_u16().try_into().unwrap());
                        success = true;
                        let data = futures::executor::block_on(response.bytes())?;
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
                                dd_dsl::data.eq(&*data),
                                dd_dsl::downloaded_at.eq(downloaded_at.clone()),
                            )).returning(dd_dsl::rowid).get_result(&conn)?);
                            info!("Downloaded and inserted {}, size is {}, sum is {}", &url, data.len(), &sum);
                        }
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
