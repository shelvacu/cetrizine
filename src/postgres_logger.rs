use std::convert::TryInto;
use log::{Record, Level, Metadata};
use chrono;
use crate::r2d2;
use r2d2::ConnectionManager;
use crate::diesel::prelude::*;
use super::EnumIntoString;
use super::SESSION_ID;

pub const INTERNAL_LOG_TARGET:&str = "postgres_logger_db";

use crate::DBPool;

pub struct PostgresLogger{
    pool: DBPool,
    level: Level,
    b_o_t: std::time::Instant,
}

impl PostgresLogger {
    pub fn new(
        pool: DBPool,
        level: Level,
        b_o_t: std::time::Instant
    ) -> Self {
        Self{pool, level, b_o_t}
    }
}

impl log::Log for PostgresLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= self.level && metadata.target() != INTERNAL_LOG_TARGET
    }

    fn log(&self, record: &Record) {
        use crate::schema::log_entry::dsl::*;
        if !self.enabled(record.metadata()) { return }
        let session_id = SESSION_ID.load(std::sync::atomic::Ordering::Relaxed);
        if session_id == 0 {
            debug!(target: INTERNAL_LOG_TARGET, "Skipping logging to database because session_id has not been set.");
            return
        }
        let duration = self.b_o_t.elapsed();
        let time = chrono::Utc::now();
        let the_conn;
        match self.pool.get() {
            Ok(c) => the_conn = c,
            Err(why) => {
                error!(target: INTERNAL_LOG_TARGET, "Could not allocate postgres connection: {:?}", why);
                return;
            },
        }
        let logging_result = diesel::insert_into(log_entry).values(
            &NewLogEntry{
                logged_at_datetime: time,
                logged_at_duration_secs: duration.as_secs().try_into().unwrap():i64,
                logged_at_duration_nanos: duration.subsec_nanos().try_into().unwrap():i32,
                session_rowid: session_id,
                log_level: record.level().into_str(),
                target: record.target(),
                module_path: record.module_path(),
                file: record.file(),
                line: record.line().map(i64::from),
                message_body: format!("{}",record.args()),
            }
        ).execute(&the_conn);
        match logging_result {
            Ok(_) => (),
            Err(why) => error!(target: INTERNAL_LOG_TARGET, "Failed to insert log entry: {:?}", why),
        }
    }

    fn flush(&self) {}
}

use crate::schema::log_entry;

#[derive(Insertable)]
#[table_name="log_entry"]
struct NewLogEntry<'a> {
    logged_at_datetime: chrono::DateTime<chrono::Utc>,
    logged_at_duration_secs: i64,
    logged_at_duration_nanos: i32,
    session_rowid: i64,
    log_level: &'a str,
    target: &'a str,
    module_path: Option<&'a str>,
    file: Option<&'a str>,
    line: Option<i64>,
    message_body: String,
}
    
