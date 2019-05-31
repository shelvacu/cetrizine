CREATE TYPE __t_moment AS (
  session_id              int8, --not null
  duration_secs           int8, --not null
  duration_nanos          int4, --not null
  datetime                timestamptz --not null
);

CREATE DOMAIN moment AS __t_moment
  CHECK(VALUE IS NULL OR (
    (VALUE).session_id IS NOT NULL AND
    (VALUE).duration_secs IS NOT NULL AND
    (VALUE).duration_nanos IS NOT NULL AND
    (VALUE).datetime IS NOT NULL AND
    (VALUE).duration_secs >= 0 AND
    0 <= (VALUE).duration_nanos AND (VALUE).duration_nanos < 1000000000
  ))
;

CREATE TABLE shard_stage_update_event (
  rowid                   serial8 not null primary key,
  happened_at             moment,
  new_stage               text not null CHECK(new_stage in ('Connected', 'Connecting', 'Disconnected', 'Handshake', 'Identifying', 'Resuming')),
  old_stage               text not null CHECK(new_stage in ('Connected', 'Connecting', 'Disconnected', 'Handshake', 'Identifying', 'Resuming')),
  shard_id                snowflake not null
  --If I could, I'd do this:
  --FOREIGN KEY ((happened_at).session_id) REFERENCES run_session(rowid)
);
