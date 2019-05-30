CREATE TABLE run_session (
  rowid                   serial8 not null primary key,
  started_at              timestamptz not null,
  pkg_name                text,
  version                 text,
  target                  text,
  build_timestamp         text,
  git_sha_ref             text,
  git_commit_date         text
);

CREATE TABLE raw_message (
  rowid                   serial8 not null primary key,
  recvd_at_datetime       timestamptz not null,
  recvd_at_duration_secs  int8 not null,
  recvd_at_duration_nanos int4 not null,
  session_rowid           int8 not null REFERENCES run_session(rowid),
  kind                    text not null CHECK(kind in ('Text', 'Binary', 'Close', 'Ping', 'Pong')),
  content_text            text,
  content_binary          bytea,
  CHECK(
    (kind = 'Text' AND content_text IS NOT NULL AND content_binary IS NULL)
    OR
    (kind in ('Binary', 'Ping', 'Pong') AND content_text IS NULL AND content_binary IS NULL)
    OR
    (kind = 'Close' AND content_text IS NULL)
  ),
  UNIQUE(session_rowid, recvd_at_duration_secs, recvd_at_duration_nanos)
);


CREATE TABLE log_entry (
  rowid                   serial8 not null primary key,
  logged_at_datetime      timestamptz not null,
  logged_at_duration_secs int8 not null,
  logged_at_duration_nanos int4 not null,
  session_rowid           int8 not null REFERENCES run_session(rowid),
  log_level               text not null CHECK(log_level in ('Error', 'Warn', 'Info', 'Debug', 'Trace')),
  target                  text not null,
  module_path             text,
  file                    text,
  line                    int8,
  message_body            text not null
);
