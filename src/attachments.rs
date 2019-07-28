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
use std::convert::TryInto;

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

    #[derive(Queryable,QueryId)]
    //#[table_name = "attachment"]
    struct AttachmentData {
        rowid: i64,
        url: String,
    }

    let conn = pool.get()?;
        
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
    let client = Client::builder()
        .default_headers(headers)
        .gzip(true)
        .connect_timeout(Some(std::time::Duration::from_secs(30)))
        //.h2_prior_knowledge()
        .build()
        .unwrap();

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
