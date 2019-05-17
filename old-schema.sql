CREATE TABLE message (
--implied rowid
discord_id text not null,
--attachments
author_id text not null,
author_avatar text,
author_is_bot int not null, --bool
author_discriminator int not null,
author_name text not null,
channel_id text not null,
content text not null,
edited_timestamp text, --datetime
guild_id text,
kind text,
member_is_some int not null, --bool
member_deaf int, --bool
member_joined_at text, --datetime
member_mute int, --bool
member_roles int REFERENCES id_arr(row_id),
mention_everyone int not null, --bool
mention_roles int not null REFERENCES id_arr(row_id),
nonce_debug text not null, --{:?}
pinned int not null, --bool
--reactions
timestamp text not null,
tts int not null, --bool
webhook_id text,
archive_recvd_at text not null --datetime
);
CREATE TABLE id_arr (
row_id integer primary key
);
CREATE TABLE id (
id_arr_rowid int not null REFERENCES id_arr(row_id),
id text not null
);
CREATE TABLE attachment (
message_rowid int not null REFERENCES message(rowid),
discord_id text not null,
filename text not null,
height_text text, --technically the width & height is a u64 which can't 'fit' in sqlite
width_text text,
height int,
width int,
proxy_url text not null,
size_text text not null,
size int not null,
url text not null
);
CREATE TABLE embed_field (
embed_rowid int not null REFERENCES embed(rowid),
inline int not null, --bool
name text not null,
value text not null
);
CREATE TABLE message_archive_gets (
channel_id text not null,
after_message_id text,
around_message_id text,
before_message_id text,
start_message_id text not null,
end_message_id text not null,
message_count_requested int not null,
message_count_received int not null,
received_live int not null --bool
);
CREATE INDEX mag_start ON message_archive_gets (substr('00000000000000000000'||start_message_id, -20, 20));
CREATE INDEX mag_end   ON message_archive_gets (substr('00000000000000000000'||  end_message_id, -20, 20));
CREATE INDEX mag_start_plain ON message_archive_gets(start_message_id);
