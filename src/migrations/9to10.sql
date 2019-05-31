CREATE INDEX message_discord_id ON message (discord_id);
CREATE INDEX message_channel_id ON message (channel_id,timestamp);
CREATE INDEX message_guild_id ON message (guild_id,timestamp);
--TODO: probably index mentions and mention_roles

CREATE INDEX reaction_message_rowid ON reaction(message_rowid);
CREATE INDEX reaction_reaction_id ON reaction(reaction_id);
CREATE INDEX reaction_reaction_string ON reaction(reaction_string);

CREATE INDEX attachment_message_rowid ON attachment(message_rowid);
CREATE INDEX attachment_discord_id ON attachment(discord_id);

CREATE INDEX embed_message_rowid ON embed(message_rowid);

CREATE INDEX embed_field_embed_rowid ON embed_field(embed_rowid, name);

CREATE INDEX group_channel_ready_rowid ON group_channel(ready_rowid);
CREATE INDEX group_channel_discord_id_rowid ON group_channel(discord_id, rowid);
CREATE INDEX group_channel_owner_id ON group_channel(owner_id);

CREATE INDEX private_channel_ready_rowid ON private_channel(ready_rowid);
CREATE INDEX private_channel_discord_id ON private_channel(discord_id,rowid);
CREATE INDEX private_channel_recipient ON private_channel(recipient,rowid);

CREATE INDEX guild_ready_rowid ON guild(ready_rowid);
CREATE INDEX guild_discord_id ON guild(discord_id, rowid);
CREATE INDEX guild_name ON guild(name, rowid);
CREATE INDEX guild_owner_id ON guild(owner_id);

CREATE INDEX guild_channel_guild_rowid_category_id_position ON guild_channel(guild_rowid,category_id,position);
CREATE INDEX guild_channel_discord_id ON guild_channel(discord_id);
CREATE INDEX guild_channel_guild_id_category_id_position ON guild_channel(guild_id, category_id, position);
CREATE INDEX guild_channel_name ON guild_channel(name);

CREATE INDEX emoji_guild_rowid ON emoji(guild_rowid);
CREATE INDEX emoji_discord_id ON emoji(discord_id);
CREATE INDEX emoji_name ON emoji(name);

CREATE INDEX guild_role_guild_rowid_name ON guild_role(guild_rowid, name);
