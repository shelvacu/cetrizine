ALTER DOMAIN user_presence_game
  DROP CONSTRAINT user_presence_game_check;
ALTER DOMAIN user_presence_game
  ADD CONSTRAINT user_presence_game_check CHECK(VALUE::text IS NULL OR (
    (VALUE).kind IS NOT NULL AND
    (VALUE).kind in ('Playing','Streaming','Listening','Custom') AND
    (VALUE).name IS NOT NULL
  ))
;