table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    attachment (rowid) {
        rowid -> Int8,
        message_rowid -> Int8,
        discord_id -> SQL_Snowflake,
        filename -> Text,
        height -> Nullable<Int8>,
        width -> Nullable<Int8>,
        proxy_url -> Text,
        size -> Int8,
        url -> Text,
        download_rowid -> Nullable<Int8>,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    embed (rowid) {
        rowid -> Int8,
        message_rowid -> Int8,
        author -> Nullable<SQL_EmbedAuthor>,
        colour_u32 -> Nullable<SQL_DiscordColour>,
        description -> Nullable<Text>,
        footer -> Nullable<SQL_EmbedFooter>,
        image -> Nullable<SQL_EmbedImage>,
        kind -> Text,
        provider -> Nullable<SQL_EmbedProvider>,
        thumbnail -> Nullable<SQL_EmbedImage>,
        timestamp -> Nullable<Text>,
        title -> Nullable<Text>,
        url -> Nullable<Text>,
        video -> Nullable<SQL_EmbedVideo>,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    embed_field (rowid) {
        rowid -> Int8,
        embed_rowid -> Int8,
        inline -> Bool,
        name -> Text,
        value -> Text,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    emoji (rowid) {
        rowid -> Int8,
        guild_rowid -> Int8,
        discord_id -> SQL_Snowflake,
        animated -> Bool,
        name -> Text,
        managed -> Bool,
        require_colons -> Bool,
        roles -> Array<SQL_Snowflake>,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    group_channel (rowid) {
        rowid -> Int8,
        ready_rowid -> Int8,
        discord_id -> SQL_Snowflake,
        icon -> Nullable<Text>,
        last_message_id -> Nullable<SQL_Snowflake>,
        last_pin_timestamp -> Nullable<Timestamptz>,
        name -> Nullable<Text>,
        owner_id -> SQL_Snowflake,
        recipients -> Array<SQL_DiscordUser>,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    guild (rowid) {
        rowid -> Int8,
        ready_rowid -> Nullable<Int8>,
        discord_id -> SQL_Snowflake,
        afk_channel_id -> Nullable<SQL_Snowflake>,
        afk_timeout -> Nullable<Int8>,
        application_id -> Nullable<Int8>,
        default_message_notification_level -> Text,
        explicit_content_filter -> Text,
        features -> Array<Text>,
        icon -> Nullable<Text>,
        joined_at -> Nullable<Timestamptz>,
        large -> Bool,
        member_count -> Int8,
        mfa_level -> Text,
        name -> Text,
        owner_id -> SQL_Snowflake,
        region -> Text,
        splash -> Nullable<Text>,
        system_channel_id -> Nullable<SQL_Snowflake>,
        verification_level -> Text,
        archive_recvd_at -> Timestamptz,
        guild_create_event_rowid -> Nullable<Int8>,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    guild_channel (rowid) {
        rowid -> Int8,
        discord_id -> SQL_Snowflake,
        guild_rowid -> Nullable<Int8>,
        bitrate -> Nullable<Int8>,
        category_id -> Nullable<SQL_Snowflake>,
        guild_id -> SQL_Snowflake,
        kind -> Text,
        last_message_id -> Nullable<SQL_Snowflake>,
        last_pin_timestamp -> Nullable<Timestamptz>,
        name -> Text,
        position -> Int8,
        topic -> Nullable<Text>,
        user_limit -> Nullable<Int8>,
        nsfw -> Bool,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    guild_create_event (rowid) {
        rowid -> Int8,
        is_new -> Bool,
        recvd_at_datetime -> Timestamptz,
        recvd_at_duration_secs -> Int8,
        recvd_at_duration_nanos -> Int4,
        session_rowid -> Int8,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    guild_prefixes (guild_id) {
        guild_id -> SQL_Snowflake,
        command_prefix -> Text,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    guild_role (rowid) {
        rowid -> Int8,
        guild_rowid -> Int8,
        discord_id -> SQL_Snowflake,
        colour_u32 -> Nullable<SQL_DiscordColour>,
        hoist -> Bool,
        managed -> Bool,
        mentionable -> Bool,
        name -> Text,
        permissions_bits -> Int8,
        position -> Int8,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    log_entry (rowid) {
        rowid -> Int8,
        logged_at_datetime -> Timestamptz,
        logged_at_duration_secs -> Int8,
        logged_at_duration_nanos -> Int4,
        session_rowid -> Int8,
        log_level -> Text,
        target -> Text,
        module_path -> Nullable<Text>,
        file -> Nullable<Text>,
        line -> Nullable<Int8>,
        message_body -> Text,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    member (rowid) {
        rowid -> Int8,
        guild_rowid -> Int8,
        guild_id -> SQL_Snowflake,
        deaf -> Bool,
        joined_at -> Nullable<Timestamptz>,
        mute -> Bool,
        nick -> Nullable<Text>,
        roles -> Array<SQL_Snowflake>,
        user_info -> SQL_DiscordUser,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    message (rowid) {
        rowid -> Int8,
        discord_id -> SQL_Snowflake,
        author -> SQL_DiscordUser,
        channel_id -> SQL_Snowflake,
        content -> Text,
        edited_timestamp -> Nullable<Timestamptz>,
        guild_id -> Nullable<SQL_Snowflake>,
        kind -> Nullable<Text>,
        member -> Nullable<SQL_PartialMember>,
        mention_everyone -> Bool,
        mention_roles -> Array<SQL_Snowflake>,
        mentions -> Array<SQL_DiscordUser>,
        nonce_debug -> Text,
        pinned -> Bool,
        timestamp -> Timestamptz,
        tts -> Bool,
        webhook_id -> Nullable<SQL_Snowflake>,
        archive_recvd_at -> Timestamptz,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    message_archive_gets (rowid) {
        rowid -> Int8,
        channel_id -> SQL_Snowflake,
        ready_rowid -> Nullable<Int8>,
        after_message_id -> Nullable<SQL_Snowflake>,
        around_message_id -> Nullable<SQL_Snowflake>,
        before_message_id -> Nullable<SQL_Snowflake>,
        start_message_id -> Nullable<SQL_Snowflake>,
        end_message_id -> Nullable<SQL_Snowflake>,
        message_count_requested -> Nullable<Int8>,
        message_count_received -> Nullable<Int8>,
        legacy -> Bool,
        synthetic -> Bool,
        finished -> Bool,
        session_id -> Nullable<Int8>,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    migration_version (enforce_single_row) {
        version -> Int8,
        enforce_single_row -> Bool,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    permission_overwrite (rowid) {
        rowid -> Int8,
        guild_channel_rowid -> Int8,
        allow_bits -> Int8,
        deny_bits -> Int8,
        permission_overwrite_type -> Text,
        permission_overwrite_id -> SQL_Snowflake,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    private_channel (rowid) {
        rowid -> Int8,
        ready_rowid -> Int8,
        discord_id -> SQL_Snowflake,
        last_message_id -> Nullable<SQL_Snowflake>,
        last_pin_timestamp -> Nullable<Timestamptz>,
        kind -> Text,
        recipient -> SQL_DiscordUser,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    raw_message (rowid) {
        rowid -> Int8,
        recvd_at_datetime -> Timestamptz,
        recvd_at_duration_secs -> Int8,
        recvd_at_duration_nanos -> Int4,
        session_rowid -> Int8,
        kind -> Text,
        content_text -> Nullable<Text>,
        content_binary -> Nullable<Bytea>,
        scanned_for_urls -> Bool,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    reaction (rowid) {
        rowid -> Int8,
        message_rowid -> Int8,
        count -> Int8,
        me -> Bool,
        reaction_is_custom -> Bool,
        reaction_animated -> Nullable<Bool>,
        reaction_id -> Nullable<SQL_Snowflake>,
        reaction_name -> Nullable<Text>,
        reaction_string -> Nullable<Text>,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    ready (rowid) {
        rowid -> Int8,
        session_id -> Text,
        shard -> Nullable<Array<Int8>>,
        trace -> Array<Text>,
        user_info -> SQL_SerenityCurrentUser,
        version -> Int8,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    run_session (rowid) {
        rowid -> Int8,
        started_at -> Timestamptz,
        pkg_name -> Nullable<Text>,
        version -> Nullable<Text>,
        target -> Nullable<Text>,
        build_timestamp -> Nullable<Text>,
        git_sha_ref -> Nullable<Text>,
        git_commit_date -> Nullable<Text>,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    shard_stage_update_event (rowid) {
        rowid -> Int8,
        happened_at -> Nullable<SQL_Moment>,
        new_stage -> Text,
        old_stage -> Text,
        shard_id -> SQL_Snowflake,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    sqlite_migration_progress (enforce_single_row) {
        progress_counter -> Int8,
        enforce_single_row -> Bool,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    user_presence (rowid) {
        rowid -> Int8,
        ready_rowid -> Nullable<Int8>,
        guild_rowid -> Nullable<Int8>,
        game -> Nullable<SQL_UserPresenceGame>,
        last_modified -> Nullable<Int8>,
        nick -> Nullable<Text>,
        status -> Text,
        user_id -> SQL_Snowflake,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    voice_state (rowid) {
        rowid -> Int8,
        guild_rowid -> Int8,
        channel_id -> Nullable<SQL_Snowflake>,
        deaf -> Bool,
        mute -> Bool,
        self_deaf -> Bool,
        self_mute -> Bool,
        session_id -> Text,
        suppress -> Bool,
        token -> Nullable<Text>,
        user_id -> SQL_Snowflake,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    download_data (rowid) {
        rowid -> Int8,
        sha256sum_hex -> Text,
        data -> Bytea,
        downloaded_at -> SQL_Moment,
    }
}
        
table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    download (rowid) {
        rowid -> Int8,
        url -> Text,
        response_code -> Nullable<Int2>,
        success -> Bool,
        download_data_rowid -> Nullable<Int8>,
        downloaded_at -> SQL_Moment,
        has_headers -> Bool,
        content_type -> Nullable<Text>,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    download_header (rowid) {
        rowid -> Int8,
        download_rowid -> Int8,
        header_name -> Text,
        header_value -> Bytea,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    raw_message_url (url, raw_message_rowid) {
        raw_message_rowid -> Int8,
        url -> Text,
        been_downloaded -> Bool,
    }
}

table! {
    use diesel::sql_types::*;
    use crate::db_types::*;
    rps_game (rowid) {
        rowid -> Int8,
        game_location_channel_id -> SQL_Snowflake,
        challenger_user_id -> SQL_Snowflake,
        receiver_user_id -> SQL_Snowflake,
        challenger_private_message_id -> SQL_Snowflake,
        receiver_private_message_id -> SQL_Snowflake,
        challenger_choice -> Nullable<Text>,
        receiver_choice -> Nullable<Text>,
    }
}

joinable!(attachment -> message (message_rowid));
joinable!(embed -> message (message_rowid));
joinable!(embed_field -> embed (embed_rowid));
joinable!(emoji -> guild (guild_rowid));
joinable!(group_channel -> ready (ready_rowid));
joinable!(guild -> guild_create_event (guild_create_event_rowid));
joinable!(guild -> ready (ready_rowid));
joinable!(guild_channel -> guild (guild_rowid));
joinable!(guild_create_event -> run_session (session_rowid));
joinable!(guild_role -> guild (guild_rowid));
joinable!(log_entry -> run_session (session_rowid));
joinable!(member -> guild (guild_rowid));
joinable!(message_archive_gets -> ready (ready_rowid));
joinable!(message_archive_gets -> run_session (session_id));
joinable!(permission_overwrite -> guild_channel (guild_channel_rowid));
joinable!(private_channel -> ready (ready_rowid));
joinable!(raw_message -> run_session (session_rowid));
joinable!(reaction -> message (message_rowid));
joinable!(user_presence -> guild (guild_rowid));
joinable!(user_presence -> ready (ready_rowid));
joinable!(voice_state -> guild (guild_rowid));
joinable!(download -> download_data (download_data_rowid));
joinable!(attachment -> download (download_rowid));
joinable!(download_header -> download (download_rowid));
joinable!(raw_message_url -> raw_message (raw_message_rowid));

allow_tables_to_appear_in_same_query!(
    attachment,
    embed,
    embed_field,
    emoji,
    group_channel,
    guild,
    guild_channel,
    guild_create_event,
    guild_prefixes,
    guild_role,
    log_entry,
    member,
    message,
    message_archive_gets,
    migration_version,
    permission_overwrite,
    private_channel,
    raw_message,
    reaction,
    ready,
    run_session,
    shard_stage_update_event,
    sqlite_migration_progress,
    user_presence,
    voice_state,

    download_data,
    download,
    download_header,
    raw_message_url,
);
