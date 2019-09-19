DROP TABLE chan_archivals;

CREATE TABLE chan_archival (
  rowid serial8 not null,
  from_cat_id snowflake, --A channel can have no category, which means it's at the top level. That's why these can be null
  to_cat_id snowflake,
  moved_chan_id snowflake not null,
  dir_is_archiving bool not null, --What "direction" the move was; true: this was a result of the "archive_channel" command; false: this was a result of the "unarchive_channel" command
  command_msg_id snowflake not null,
  --I would add a "happened at" date, but the message id suffices
  done bool not null default false, --Whether we have received a positive ACK from discord about the attempted move
  failed bool not null default false, --Whether we have received a negative ACK (error) from discord about the attempted move
  CHECK(NOT (done AND failed))
);

CREATE INDEX chan_archivals_to_chan_id_failed_command_msg_id ON chan_archival(moved_chan_id, failed, command_msg_id);