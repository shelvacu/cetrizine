-- https://stackoverflow.com/questions/22763151/check-if-a-postgres-composite-field-is-null-empty
-- "One may think that `!(x IS NULL) = x IS NOT NULL` is true in all cases. But there is an exception - composite types. When one field of a composite value is `NULL` and another field is `NOT NULL`, then result of both operators is false. `IS NULL` is true, only when all fields are `NULL`. `IS NOT NULL` is true, only when all fields are `NOT NULL`. For any case in between, then both operators return false."
-- Madness, absolute madness.

-- ALTER DOMAIN serenity_current_user
--   DROP CONSTRAINT serenity_current_user_check_inner_user;
-- ALTER DOMAIN serenity_current_user
--   ADD CONSTRAINT serenity_current_user_check_inner_user CHECK(VALUE::text IS NULL OR ((VALUE).inner_user)::text IS NOT NULL);

CREATE TYPE __t_discord_user_new AS (
  discord_id            snowflake,-- not null,
  avatar                text,
  is_bot                bool,-- not null,
  discriminator         int2,-- not null,
  user_name             text -- not null
);

CREATE DOMAIN discord_user_new AS __t_discord_user_new
  CHECK(VALUE::text IS NULL OR (
    (VALUE).discord_id IS NOT NULL AND
    (VALUE).is_bot IS NOT NULL AND
    (VALUE).discriminator IS NOT NULL AND
    (VALUE).user_name IS NOT NULL AND 
    (VALUE).discriminator >= 0 AND
    (VALUE).discriminator <= 9999
  ))
;

CREATE TYPE __t_serenity_current_user_new AS (
  inner_user            discord_user_new,-- not null, --id, avatar, bot, discriminator, name
  email                 text,
  mfa_enabled           bool,-- not null,
  verified              bool -- not null
);
CREATE DOMAIN serenity_current_user_new AS __t_serenity_current_user_new
  CHECK(VALUE::text IS NULL OR(
    ((VALUE).inner_user)::text IS NOT NULL AND
    (VALUE).mfa_enabled IS NOT NULL AND
    (VALUE).verified IS NOT NULL
  ))
;

ALTER TABLE ready ALTER COLUMN user_info TYPE serenity_current_user_new USING (user_info::text)::serenity_current_user_new;
ALTER TABLE message ALTER COLUMN author TYPE discord_user_new USING (author::text)::discord_user_new;
ALTER TABLE message ALTER COLUMN mentions TYPE discord_user_new[] USING (mentions::text[])::discord_user_new[];
ALTER TABLE group_channel ALTER COLUMN recipients TYPE discord_user_new[] USING (recipients::text[])::discord_user_new[];
ALTER TABLE private_channel ALTER COLUMN recipient TYPE discord_user_new USING (recipient::text)::discord_user_new;
ALTER TABLE member ALTER COLUMN user_info TYPE discord_user_new USING (user_info::text)::discord_user_new;


DROP DOMAIN serenity_current_user;
ALTER DOMAIN serenity_current_user_new RENAME TO serenity_current_user;
DROP TYPE __t_serenity_current_user;
ALTER TYPE __t_serenity_current_user_new RENAME TO __t_serenity_current_user;
DROP DOMAIN discord_user;
ALTER DOMAIN discord_user_new RENAME TO discord_user;
DROP TYPE __t_discord_user;
ALTER TYPE __t_discord_user_new RENAME TO __t_discord_user;


-- ALTER DOMAIN discord_user
--   DROP CONSTRAINT discord_user_check;
-- ALTER DOMAIN discord_user
--   ADD CONSTRAINT discord_user_check CHECK(VALUE::text IS NULL OR (
--     (VALUE).discord_id IS NOT NULL AND
--     (VALUE).is_bot IS NOT NULL AND
--     (VALUE).discriminator IS NOT NULL AND
--     (VALUE).user_name IS NOT NULL AND 
--     (VALUE).discriminator >= 0 AND
--     (VALUE).discriminator <= 9999
--   ))
-- ;

ALTER DOMAIN partial_member
  DROP CONSTRAINT partial_member_check;
ALTER DOMAIN partial_member
  ADD CONSTRAINT partial_member_check CHECK(VALUE::text IS NULL OR (
    (VALUE).deaf IS NOT NULL AND
    --joined_at can be null
    (VALUE).mute IS NOT NULL AND
    (VALUE).roles IS NOT NULL AND
    f_check_no_null((VALUE).roles)
  ))
;

ALTER DOMAIN embed_author
  DROP CONSTRAINT embed_author_check;
ALTER DOMAIN embed_author
  ADD CONSTRAINT embed_author_check CHECK(VALUE::text IS NULL OR(
    (VALUE).name IS NOT NULL
  ))
;

ALTER DOMAIN embed_footer
  DROP CONSTRAINT embed_footer_check;
ALTER DOMAIN embed_footer
  ADD CONSTRAINT embed_footer_check CHECK(VALUE::text IS NULL OR (
    (VALUE).text IS NOT NULL
  ))
;

ALTER DOMAIN embed_image
  DROP CONSTRAINT embed_image_check;
ALTER DOMAIN embed_image
  ADD CONSTRAINT embed_image_check CHECK(VALUE::text IS NULL OR(
    (VALUE).height IS NOT NULL AND
    (VALUE).width IS NOT NULL AND
    (VALUE).proxy_url IS NOT NULL AND
    (VALUE).url IS NOT NULL
  ))
;

ALTER DOMAIN embed_provider
  DROP CONSTRAINT embed_provider_check;
ALTER DOMAIN embed_provider
  ADD CONSTRAINT embed_provider_check CHECK(VALUE::text IS NULL OR (
    (VALUE).name IS NOT NULL
  ))
;

ALTER DOMAIN embed_video
  DROP CONSTRAINT embed_video_check;
ALTER DOMAIN embed_video
  ADD CONSTRAINT embed_video_check CHECK(VALUE::text IS NULL OR (
    (VALUE).height IS NOT NULL AND
    (VALUE).width IS NOT NULL
  ))
;

ALTER DOMAIN user_presence_game
  DROP CONSTRAINT user_presence_game_check;
ALTER DOMAIN user_presence_game
  ADD CONSTRAINT user_presence_game_check CHECK(VALUE::text IS NULL OR (
    (VALUE).kind IS NOT NULL AND
    (VALUE).kind in ('Playing','Streaming','Listening') AND
    (VALUE).name IS NOT NULL
  ))
;