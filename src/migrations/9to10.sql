--A guild with no prefix set does not have a row in this table.
CREATE TABLE guild_prefixes (
  guild_id                snowflake not null primary key,
  command_prefix          text not null
);

CREATE TABLE guild_create_event (
  rowid                   serial8 not null primary key,
  is_new                  bool not null,
  recvd_at_datetime       timestamptz not null,
  recvd_at_duration_secs  int8 not null,
  recvd_at_duration_nanos int4 not null,
  session_rowid           int8 not null REFERENCES run_session(rowid),
  FOREIGN KEY (session_rowid, recvd_at_duration_secs, recvd_at_duration_nanos) REFERENCES raw_message(session_rowid, recvd_at_duration_secs, recvd_at_duration_nanos),
  CHECK(
  (recvd_at_duration_secs IS NOT NULL AND recvd_at_duration_nanos IS NOT NULL AND session_rowid IS NOT NULL)
  OR
  (recvd_at_duration_secs IS NULL AND recvd_at_duration_nanos IS NULL AND session_rowid IS NULL)
  )
);

ALTER TABLE guild ADD COLUMN guild_create_event_rowid int8 REFERENCES guild_create_event(rowid);
ALTER TABLE guild ADD CONSTRAINT guild_has_exactly_one_parent CHECK(
  (ready_rowid IS NULL)::int +
  (guild_create_event_rowid IS NULL)::int = 1
);
