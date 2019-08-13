CREATE TABLE rps_game (
    rowid serial8 primary key not null,
    game_location_channel_id snowflake not null,
    challenger_user_id snowflake not null,
    receiver_user_id snowflake not null,
    challenger_private_message_id snowflake not null,
    receiver_private_message_id snowflake not null,
    challenger_choice text default null CHECK(challenger_choice IS NULL OR challenger_choice IN ('Rock', 'Paper', 'Scissors')),
    receiver_choice text default null CHECK(receiver_choice IS NULL OR receiver_choice IN ('Rock', 'Paper', 'Scissors'))
);

CREATE UNIQUE INDEX rps_game_cpmi ON rps_game(challenger_private_message_id);
CREATE UNIQUE INDEX rps_game_rpmi ON rps_game(receiver_private_message_id);