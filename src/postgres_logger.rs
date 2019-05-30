use log::{LogRecord, LogLevel, LogMetadata};
use chrono;
use r2d2_postgres::r2d2;
//use crate::chrono;
//use crate::r2d2;
use r2d2_postgres::PostgresConnectionManager;
use super::EnumIntoString;

pub const INTERNAL_LOG_TARGET:&str = "postgres_logger_db";

pub struct PostgresLogger{
    pool: r2d2::Pool<PostgresConnectionManager>,
    level: LogLevel,
    session_id: i64,
    b_o_t: std::time::Instant,
}

impl PostgresLogger {
    pub fn new(
        pool: r2d2::Pool<PostgresConnectionManager>,
        level: LogLevel,
        session_id: i64,
        b_o_t: std::time::Instant
    ) -> Self {
        Self{pool, level, session_id, b_o_t}
    }
}

impl log::Log for PostgresLogger {
    fn enabled(&self, metadata: &LogMetadata) -> bool {
        metadata.level() <= self.level && metadata.target() != INTERNAL_LOG_TARGET
    }

    fn log(&self, record: &LogRecord) {
        if !self.enabled(record.metadata()) { return }
        let duration = self.b_o_t.elapsed();
        let time = chrono::Utc::now();
        let the_conn;
        match self.pool.get() {
            Ok(c) => the_conn = c,
            Err(why) => {
                error!(target: INTERNAL_LOG_TARGET, "Could not allocate postgres connection");
                return;
            },
        }
        let logging_result:pg::Result<()> = (|| {
            the_conn.batch_execute("SET synchronous_commit TO 'off';")?;
            pg_insert_helper!(
                the_conn, "log_entry",
                logged_at_datetime => time,
                logged_at_duration_secs => (duration.as_secs() as i64),
                logged_at_duration_nanos => duration.subsec_nanos(),
                session_rowid => self.session_id,
                log_level => record.level().into_str(),
                target => record.target(),
                module_path => record.location().module_path(),
                file => record.location().file(),
                line => record.location().line(),
                message_body => format!("{}",record.args()),
            )?;
            the_conn.batch_execute("SET synchronous_commit TO DEFAULT")?;
            Ok(())
        })();
        match logging_result {
            Ok(_) => (),
            Err(why) => error!(target: INTERNAL_LOG_TARGET, "Failed to insert log entry: {:?}", why),
        }
    }
}
