ALTER TABLE rps_game
  ADD COLUMN gameover_message_id snowflake,
  ADD COLUMN challenger_wants_rematch bool not null default false,
  ADD COLUMN receiver_wants_rematch bool not null default false;

CREATE UNIQUE INDEX rps_game_gmi ON rps_game(gameover_message_id) WHERE gameover_message_id IS NOT NULL;