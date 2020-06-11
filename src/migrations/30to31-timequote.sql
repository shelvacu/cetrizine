CREATE OR REPLACE FUNCTION idify(u discord_user)
 RETURNS text AS $$
BEGIN
 RETURN (u).user_name || '#' || lpad((u).discriminator::text,4,'0');
END;
$$
LANGUAGE plpgsql;

CREATE INDEX IF NOT EXISTS guild_discord_id_rowid ON guild(discord_id, rowid);
CREATE INDEX IF NOT EXISTS guild_channel_discord_id_rowid ON guild_channel(discord_id, rowid);
CREATE INDEX IF NOT EXISTS group_channel_discord_id_rowid ON group_channel(discord_id, rowid);
CREATE INDEX IF NOT EXISTS private_channel_discord_id_rowid ON private_channel(discord_id, rowid);
CREATE INDEX IF NOT EXISTS message_discord_id_rowid ON message(discord_id, rowid);


CREATE OR REPLACE VIEW guild_view AS
  SELECT * FROM guild WHERE rowid IN (select max(rowid) from guild group by discord_id)
;

CREATE OR REPLACE VIEW channel_view AS 
  SELECT
    rowid,
    'guild' as chan_kind,
    discord_id,
    guild_id,
    kind,
    name
  FROM
    guild_channel
  WHERE
    rowid IN (SELECT max(rowid) from guild_channel group by discord_id)
  UNION ALL
  SELECT
    rowid,
    'group' as chan_kind,
    discord_id,
    NULL::snowflake as guild_id,
    'GroupDM' as kind,
    name
  FROM
    group_channel
  WHERE
    rowid IN (SELECT max(rowid) from group_channel group by discord_id)
  UNION ALL
  SELECT
    rowid,
    'private' as chan_kind,
    discord_id,
    NULL::snowflake as guild_id,
    'PrivateDM' as kind,
    idify(recipient) as name
  FROM
    private_channel
  WHERE
    rowid IN (SELECT max(rowid) from private_channel group by discord_id)
;
  

CREATE OR REPLACE VIEW message_view AS SELECT 
  m.rowid,
  m.discord_id,
  m.author,
  m.channel_id,
  m.content,
  m.edited_timestamp,
  --guild_id
  m.kind,
  m.member,
  m.mention_everyone,
  m.mention_roles,
  m.mentions,
  m.nonce_debug,
  m.pinned,
  m.timestamp,
  m.tts,
  m.webhook_id,
  m.archive_recvd_at,
  m.guild_id,
  (
    CASE 
    WHEN c.guild_id IS NULL THEN 
      ( 'https://discordapp.com/channels/' || m.channel_id || '/' || m.discord_id )
    ELSE 
      ( 'https://discordapp.com/channels/' || c.guild_id || '/' ||  m.channel_id || '/' || m.discord_id )
    END
  ) as url,
  idify(m.author) as author_id,
  g.name as guild_name,
  c.name as channel_name,
  m.guild_id as bad_guild_id
  FROM (
      select * from message where rowid in (select max(rowid) from message group by discord_id)
  ) m LEFT JOIN channel_view c ON c.discord_id = m.channel_id LEFT JOIN guild_view g ON c.guild_id = g.discord_id
;

--fix old message rows missing guild_id when they shouldn't
UPDATE message SET guild_id = c.guild_id from channel_view c where message.channel_id = c.discord_id and message.guild_id IS NULL and c.guild_id IS NOT NULL;

--CREATE INDEX IF NOT EXISTS message_guild_id_channel_id_timestamp_timetz ON message(guild_id, channel_id, ("timestamp"::timetz));
CREATE INDEX IF NOT EXISTS matt_index ON message(guild_id, channel_id, ((author).discord_id), (timezone('UTC', "timestamp")::time));
--CREATE INDEX IF NOT EXISTS message_timestamp_timetz ON message(("timestamp"::timetz));