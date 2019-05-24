CREATE OR REPLACE FUNCTION f_check_no_null (anyarray)
  RETURNS bool LANGUAGE sql IMMUTABLE AS
 'SELECT CASE WHEN $1 IS NOT NULL THEN array_position($1, NULL) IS NULL END';

CREATE DOMAIN snowflake AS int8
  CHECK (VALUE IS NULL OR VALUE >= 0);

CREATE DOMAIN discord_colour AS int8
  CHECK(VALUE IS NULL OR (VALUE >= 0 AND VALUE < 4294967296));

CREATE TYPE __t_discord_user AS (
  discord_id            snowflake,-- not null,
  avatar                text,
  is_bot                bool,-- not null,
  discriminator         int2,-- not null,
  user_name             text -- not null
);

CREATE DOMAIN discord_user AS __t_discord_user
  CHECK(VALUE IS NULL OR (
    (VALUE).discord_id IS NOT NULL AND
    (VALUE).is_bot IS NOT NULL AND
    (VALUE).discriminator IS NOT NULL AND
    (VALUE).user_name IS NOT NULL AND 
    (VALUE).discriminator >= 0 AND
    (VALUE).discriminator <= 9999
  ))
;

CREATE TYPE __t_partial_member AS (
  deaf                  bool,-- not null,
  joined_at             timestamptz,
  mute                  bool,-- not null,
  roles                 snowflake[] -- not null CHECK(f_check_no_null(roles))
);

CREATE DOMAIN partial_member AS __t_partial_member
  CHECK(VALUE IS NULL OR (
    (VALUE).deaf IS NOT NULL AND
    --joined_at can be null
    (VALUE).mute IS NOT NULL AND
    (VALUE).roles IS NOT NULL AND
    f_check_no_null((VALUE).roles)
  ))
;

CREATE TABLE message (
  rowid                 serial8 primary key,
  discord_id            snowflake not null,
  --attachments
  author                discord_user not null,
  channel_id            snowflake not null,
  content               text not null,
  edited_timestamp      timestamptz,
  --embeds
  guild_id              snowflake,
  kind                  text CHECK(kind in ('Regular','GroupRecipientAddition','GroupRecipientRemoval','GroupCallCreation','GroupNameUpdate','GroupIconUpdate','PinsAdd','MemberJoin')),
  member                partial_member,
  mention_everyone      bool not null,
  mention_roles         snowflake[] not null CHECK(f_check_no_null(mention_roles)),
  mentions              discord_user[] not null CHECK(f_check_no_null(mentions)),
  nonce_debug           text not null, -- {:?}
  pinned                bool not null,
  --reactions
  timestamp             timestamptz not null,
  tts                   bool not null,
  webhook_id            snowflake,
  archive_recvd_at      timestamptz not null
);
  
CREATE TABLE reaction (
  rowid                 serial8 primary key,
  message_rowid         int8 not null REFERENCES message(rowid),
  count                 int8 not null,
  me                    bool not null,
  reaction_is_custom    bool not null,

  reaction_animated     bool, --not null if reaction_is_custom
  reaction_id           snowflake, --not null if reaction_is_custom
  reaction_name         text,

  reaction_string       text, --not null if NOT reaction_is_custom
  CHECK(
    ( reaction_is_custom AND reaction_animated IS NOT NULL AND reaction_id IS NOT NULL AND reaction_string IS NULL )
    OR
    ( NOT(reaction_is_custom) AND reaction_string IS NOT NULL AND reaction_animated IS NULL AND reaction_id IS NULL AND reaction_name IS NULL )
  )
);

CREATE TABLE attachment (
  rowid                 serial8 primary key,
  message_rowid         int8 not null REFERENCES message(rowid),
  discord_id            snowflake not null,
  filename              text not null,
  height                int8,
  width                 int8,
  proxy_url             text not null,
  size                  int8 not null,
  url                   text not null
);

CREATE TYPE __t_embed_author AS (
  icon_url              text,
  name                  text,-- not null,
  proxy_icon_url        text,
  url                   text
);
CREATE DOMAIN embed_author AS __t_embed_author
  CHECK(VALUE IS NULL OR(
    (VALUE).name IS NOT NULL
  ))
;
CREATE TYPE __t_embed_footer AS (
  icon_url              text,
  proxy_icon_url        text,
  text                  text -- not null
);
CREATE DOMAIN embed_footer AS __t_embed_footer
  CHECK(VALUE IS NULL OR (
    (VALUE).text IS NOT NULL
  ))
;
CREATE TYPE __t_embed_image AS (
  height                int8,-- not null,
  width                 int8,-- not null,
  proxy_url             text,-- not null,
  url                   text -- not null
);
CREATE DOMAIN embed_image AS __t_embed_image
  CHECK(VALUE IS NULL OR(
    (VALUE).height IS NOT NULL AND
    (VALUE).width IS NOT NULL AND
    (VALUE).proxy_url IS NOT NULL AND
    (VALUE).url IS NOT NULL
  ))
;
CREATE TYPE __t_embed_provider AS (
  name                  text,-- not null,
  url                   text
);
CREATE DOMAIN embed_provider AS __t_embed_provider
  CHECK(VALUE IS NULL OR (
    (VALUE).name IS NOT NULL
  ))
;
/* --this type is exactly the same as embed_image, so use that
CREATE TYPE __t_embed_thumbnail (
  height                int8,-- not null,
  width                 int8,-- not null,
  proxy_url             text,-- not null,
  url                   text -- not null
);
CREATE DOMAIN embed_thumbnail AS __t_embed_thumbnail (
  CHECK(
    (VALUE).height IS NOT NULL AND
    (VALUE).width IS NOT NULL AND
    (VALUE).proxy_url IS NOT NULL AND
    (VALUE).url IS NOT NULL
  )
;*/
CREATE TYPE __t_embed_video AS (
  height                int8,-- not null,
  width                 int8,-- not null,
  url                   text
);
CREATE DOMAIN embed_video AS __t_embed_video
  CHECK(VALUE IS NULL OR (
    (VALUE).height IS NOT NULL AND
    (VALUE).width IS NOT NULL
  ))
;

CREATE TABLE embed (
  rowid                 serial8 primary key,
  message_rowid         int8 not null REFERENCES message(rowid),
  author                embed_author,
  colour_u32            discord_colour,
  description           text,
  --fields
  footer                embed_footer,
  image                 embed_image,
  kind                  text not null,
  provider              embed_provider,
  thumbnail             embed_image,
  timestamp             text,
  title                 text,
  url                   text,
  video                 embed_video
);
CREATE TABLE embed_field (
  rowid                 serial8 primary key,
  embed_rowid           int8 not null REFERENCES embed(rowid),
  inline                bool not null,
  name                  text not null,
  value                 text not null
);

-- current_user is a postgres keyword apparently
CREATE TYPE __t_serenity_current_user AS (
  inner_user            discord_user,-- not null, --id, avatar, bot, discriminator, name
  email                 text,
  mfa_enabled           bool,-- not null,
  verified              bool -- not null
);
CREATE DOMAIN serenity_current_user AS __t_serenity_current_user
  CHECK(VALUE IS NULL OR(
    (VALUE).inner_user IS NOT NULL AND
    (VALUE).mfa_enabled IS NOT NULL AND
    (VALUE).verified IS NOT NULL
  ))
;
CREATE TABLE ready (
  rowid                 serial8 primary key,
  --guilds
  --presences
  --private_channels
  session_id            text not null,
  shard                 int8[2] CHECK(shard IS NULL OR array_length(shard,1) = 2),
  trace                 text[] not null CHECK(f_check_no_null(trace)),
  user_info             serenity_current_user not null,
  version               int8 not null
);

CREATE TABLE group_channel (
  rowid                 serial8 primary key,
  ready_rowid           int8 not null REFERENCES ready(rowid),
  discord_id            snowflake not null,
  icon                  text,
  last_message_id       snowflake,
  last_pin_timestamp    timestamptz,
  name                  text,
  owner_id              snowflake not null,
  recipients            discord_user[] not null CHECK(f_check_no_null(recipients))
);

CREATE TABLE private_channel (
  rowid                 serial8 primary key,
  ready_rowid           int8 not null REFERENCES ready(rowid),
  discord_id            snowflake not null,
  last_message_id       snowflake,
  last_pin_timestamp    timestamptz,
  kind                  text not null,
  recipient             discord_user not null
);  

CREATE TABLE guild (
  rowid                 serial8 primary key,
  ready_rowid           int8 REFERENCES ready(rowid), --possibly null
  discord_id            snowflake not null,
  afk_channel_id        snowflake,
  afk_timeout           int8,
  application_id        snowflake,
  --channels
  default_message_notification_level text not null CHECK(default_message_notification_level in ('All', 'Mentions')),
  --emojis
  explicit_content_filter text not null CHECK(explicit_content_filter in ('None', 'WithoutRole', 'All')),
  features              text[] not null CHECK(f_check_no_null(features)),
  icon                  text,
  joined_at             timestamptz,
  large                 bool not null,
  member_count          int8 not null,
  --members
  mfa_level             text not null CHECK(mfa_level in ('None', 'Elevated')),
  name                  text not null,
  owner_id              snowflake not null,
  --presences
  region                text not null,
  --roles
  splash                text,
  system_channel_id     snowflake,
  verification_level    text not null CHECK(verification_level IN ('None', 'Low', 'Medium', 'High', 'Higher')),
  --voice states
  archive_recvd_at      timestamptz not null
);

CREATE TABLE guild_channel (
  rowid                 serial8 primary key,
  discord_id            snowflake not null,
  guild_rowid           int8 REFERENCES guild(rowid), --possibly null
  bitrate               int8,
  category_id           snowflake,
  guild_id              snowflake not null,
  kind                  text not null CHECK(kind in ('Text','Private','Voice','Group','Category')),
  last_message_id       snowflake,
  last_pin_timestamp    timestamptz,
  name                  text not null,
  --permission_overwrites
  position              int8 not null,
  topic                 text,
  user_limit            int8,
  nsfw                  bool not null
);

CREATE TABLE permission_overwrite (
  rowid                 serial8 primary key,
  guild_channel_rowid   int8 not null REFERENCES guild_channel(rowid),
  allow_bits            int8 not null,
  deny_bits             int8 not null,
  permission_overwrite_type text not null CHECK(permission_overwrite_type in ('Member', 'Role')),
  permission_overwrite_id snowflake not null
);
  
CREATE TABLE emoji (
  rowid                 serial8 primary key,
  guild_rowid           int8 not null REFERENCES guild(rowid),
  discord_id            snowflake not null,
  animated              bool not null,
  name                  text not null,
  managed               bool not null,
  require_colons        bool not null,
  roles                 snowflake[] not null CHECK(f_check_no_null(roles))
);

CREATE TABLE member (
  rowid                 serial8 primary key,
  guild_rowid           int8 not null REFERENCES guild(rowid),
  guild_id              snowflake not null,
  deaf                  bool not null,
  joined_at             timestamptz,
  mute                  bool not null,
  nick                  text,
  roles                 snowflake[] not null CHECK(f_check_no_null(roles)),
  user_info             discord_user not null
);

CREATE TYPE __t_user_presence_game AS (
  kind                  text,-- not null CHECK(kind in ('Playing','Streaming','Listening')),
  name                  text,-- not null,
  url                   text
);
CREATE DOMAIN user_presence_game as __t_user_presence_game
  CHECK(VALUE IS NULL OR (
    (VALUE).kind IS NOT NULL AND
    (VALUE).kind in ('Playing','Streaming','Listening') AND
    (VALUE).name IS NOT NULL
  ))
;

CREATE TABLE user_presence (
  rowid                 serial8 primary key,
  ready_rowid           int8 REFERENCES ready(rowid),
  guild_rowid           int8 REFERENCES guild(rowid),
  CHECK((ready_rowid IS NULL AND guild_rowid IS NOT NULL) OR (ready_rowid IS NOT NULL AND guild_rowid IS NULL)),

  game                  user_presence_game,
  last_modified         int8,
  nick                  text,
  status                text not null CHECK(status in ('DoNotDisturb','Idle','Invisible','Offline','Online')),
  user_id               snowflake not null
);

CREATE TABLE guild_role (
  rowid                 serial8 primary key,
  guild_rowid           int8 not null REFERENCES guild(rowid),
  discord_id            snowflake not null,
  colour_u32            discord_colour,
  hoist                 bool not null,
  managed               bool not null,
  mentionable           bool not null,
  name                  text not null,
  permissions_bits      int8 not null,
  position              int8 not null
);

CREATE TABLE voice_state (
  rowid                 serial8 primary key,
  guild_rowid           int8 not null REFERENCES guild(rowid),
  channel_id            snowflake,
  deaf                  bool not null,
  mute                  bool not null,
  self_deaf             bool not null,
  self_mute             bool not null,
  session_id            text not null,
  suppress              bool not null,
  token                 text,
  user_id               snowflake not null
);

CREATE TABLE migration_version (
  version int8 not null CHECK(version >= 7), --versions 5 and below used SQLite. version number 6 is purposefully unused.
  enforce_single_row bool NOT NULL UNIQUE DEFAULT true CHECK(enforce_single_row)
);

CREATE TABLE message_archive_gets (
  rowid                 serial8 primary key,
  channel_id            snowflake not null,
  ready_rowid           int8 REFERENCES ready(rowid), --possibly null
  after_message_id      snowflake,
  around_message_id     snowflake,
  before_message_id     snowflake,
  start_message_id      snowflake not null,
  end_message_id        snowflake not null,
  message_count_requested int8 not null,
  message_count_received int8 not null,
  CHECK(
    (
      after_message_id IS NULL
     AND
      around_message_id IS NULL
     AND
      before_message_id IS NOT NULL
    )
    OR
    (
      after_message_id IS NULL
     AND
      around_message_id IS NOT NULL
     AND
      before_message_id IS NULL
    )
    OR
    (
      after_message_id IS NOT NULL
     AND
      around_message_id IS NULL
     AND
      before_message_id IS NULL
    )
  )
);

CREATE INDEX mag_channel_start  ON message_archive_gets(channel_id, start_message_id);
CREATE INDEX mag_channel_end    ON message_archive_gets(channel_id,   end_message_id);
CREATE INDEX mag_channel_after  ON message_archive_gets(channel_id, after_message_id);
CREATE INDEX mag_channel_before ON message_archive_gets(channel_id,before_message_id);
CREATE INDEX mag_channel_around ON message_archive_gets(channel_id,around_message_id);

CREATE TABLE sqlite_migration_progress (
  progress_counter int8 not null,
  enforce_single_row bool NOT NULL UNIQUE DEFAULT true CHECK(enforce_single_row)
);

INSERT INTO sqlite_migration_progress (progress_counter) VALUES (0);
