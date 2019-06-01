-- This is the foreign key against raw_message. It doesn't work because the relevant row in raw_message is inserted concurrently, so the row in guild_create_event may reference a row in raw_message that doesn't exist yet.
ALTER TABLE guild_create_event DROP CONSTRAINT guild_create_event_session_rowid_fkey1;
