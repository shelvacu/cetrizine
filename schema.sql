--
-- PostgreSQL database dump
--

-- Dumped from database version 11.4 (Ubuntu 11.4-1.pgdg18.04+1)
-- Dumped by pg_dump version 11.4 (Ubuntu 11.4-1.pgdg18.04+1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: snowflake; Type: DOMAIN; Schema: public; Owner: shelvacu
--

CREATE DOMAIN public.snowflake AS bigint
	CONSTRAINT snowflake_check CHECK (((VALUE IS NULL) OR (VALUE >= 0)));


ALTER DOMAIN public.snowflake OWNER TO shelvacu;

--
-- Name: __t_discord_user; Type: TYPE; Schema: public; Owner: shelvacu
--

CREATE TYPE public.__t_discord_user AS (
	discord_id public.snowflake,
	avatar text,
	is_bot boolean,
	discriminator smallint,
	user_name text
);


ALTER TYPE public.__t_discord_user OWNER TO shelvacu;

--
-- Name: __t_embed_author; Type: TYPE; Schema: public; Owner: shelvacu
--

CREATE TYPE public.__t_embed_author AS (
	icon_url text,
	name text,
	proxy_icon_url text,
	url text
);


ALTER TYPE public.__t_embed_author OWNER TO shelvacu;

--
-- Name: __t_embed_footer; Type: TYPE; Schema: public; Owner: shelvacu
--

CREATE TYPE public.__t_embed_footer AS (
	icon_url text,
	proxy_icon_url text,
	text text
);


ALTER TYPE public.__t_embed_footer OWNER TO shelvacu;

--
-- Name: __t_embed_image; Type: TYPE; Schema: public; Owner: shelvacu
--

CREATE TYPE public.__t_embed_image AS (
	height bigint,
	width bigint,
	proxy_url text,
	url text
);


ALTER TYPE public.__t_embed_image OWNER TO shelvacu;

--
-- Name: __t_embed_provider; Type: TYPE; Schema: public; Owner: shelvacu
--

CREATE TYPE public.__t_embed_provider AS (
	name text,
	url text
);


ALTER TYPE public.__t_embed_provider OWNER TO shelvacu;

--
-- Name: __t_embed_video; Type: TYPE; Schema: public; Owner: shelvacu
--

CREATE TYPE public.__t_embed_video AS (
	height bigint,
	width bigint,
	url text
);


ALTER TYPE public.__t_embed_video OWNER TO shelvacu;

--
-- Name: __t_moment; Type: TYPE; Schema: public; Owner: shelvacu
--

CREATE TYPE public.__t_moment AS (
	session_id bigint,
	duration_secs bigint,
	duration_nanos integer,
	datetime timestamp with time zone
);


ALTER TYPE public.__t_moment OWNER TO shelvacu;

--
-- Name: __t_partial_member; Type: TYPE; Schema: public; Owner: shelvacu
--

CREATE TYPE public.__t_partial_member AS (
	deaf boolean,
	joined_at timestamp with time zone,
	mute boolean,
	roles public.snowflake[]
);


ALTER TYPE public.__t_partial_member OWNER TO shelvacu;

--
-- Name: discord_user; Type: DOMAIN; Schema: public; Owner: shelvacu
--

CREATE DOMAIN public.discord_user AS public.__t_discord_user
	CONSTRAINT discord_user_check CHECK (((VALUE IS NULL) OR (((VALUE).discord_id IS NOT NULL) AND ((VALUE).is_bot IS NOT NULL) AND ((VALUE).discriminator IS NOT NULL) AND ((VALUE).user_name IS NOT NULL) AND ((VALUE).discriminator >= 0) AND ((VALUE).discriminator <= 9999))));


ALTER DOMAIN public.discord_user OWNER TO shelvacu;

--
-- Name: __t_serenity_current_user; Type: TYPE; Schema: public; Owner: shelvacu
--

CREATE TYPE public.__t_serenity_current_user AS (
	inner_user public.discord_user,
	email text,
	mfa_enabled boolean,
	verified boolean
);


ALTER TYPE public.__t_serenity_current_user OWNER TO shelvacu;

--
-- Name: __t_user_presence_game; Type: TYPE; Schema: public; Owner: shelvacu
--

CREATE TYPE public.__t_user_presence_game AS (
	kind text,
	name text,
	url text
);


ALTER TYPE public.__t_user_presence_game OWNER TO shelvacu;

--
-- Name: discord_colour; Type: DOMAIN; Schema: public; Owner: shelvacu
--

CREATE DOMAIN public.discord_colour AS bigint
	CONSTRAINT discord_colour_check CHECK (((VALUE IS NULL) OR ((VALUE >= 0) AND (VALUE < '4294967296'::bigint))));


ALTER DOMAIN public.discord_colour OWNER TO shelvacu;

--
-- Name: embed_author; Type: DOMAIN; Schema: public; Owner: shelvacu
--

CREATE DOMAIN public.embed_author AS public.__t_embed_author
	CONSTRAINT embed_author_check CHECK (((VALUE IS NULL) OR ((VALUE).name IS NOT NULL)));


ALTER DOMAIN public.embed_author OWNER TO shelvacu;

--
-- Name: embed_footer; Type: DOMAIN; Schema: public; Owner: shelvacu
--

CREATE DOMAIN public.embed_footer AS public.__t_embed_footer
	CONSTRAINT embed_footer_check CHECK (((VALUE IS NULL) OR ((VALUE).text IS NOT NULL)));


ALTER DOMAIN public.embed_footer OWNER TO shelvacu;

--
-- Name: embed_image; Type: DOMAIN; Schema: public; Owner: shelvacu
--

CREATE DOMAIN public.embed_image AS public.__t_embed_image
	CONSTRAINT embed_image_check CHECK (((VALUE IS NULL) OR (((VALUE).height IS NOT NULL) AND ((VALUE).width IS NOT NULL) AND ((VALUE).proxy_url IS NOT NULL) AND ((VALUE).url IS NOT NULL))));


ALTER DOMAIN public.embed_image OWNER TO shelvacu;

--
-- Name: embed_provider; Type: DOMAIN; Schema: public; Owner: shelvacu
--

CREATE DOMAIN public.embed_provider AS public.__t_embed_provider
	CONSTRAINT embed_provider_check CHECK (((VALUE IS NULL) OR ((VALUE).name IS NOT NULL)));


ALTER DOMAIN public.embed_provider OWNER TO shelvacu;

--
-- Name: embed_video; Type: DOMAIN; Schema: public; Owner: shelvacu
--

CREATE DOMAIN public.embed_video AS public.__t_embed_video
	CONSTRAINT embed_video_check CHECK (((VALUE IS NULL) OR (((VALUE).height IS NOT NULL) AND ((VALUE).width IS NOT NULL))));


ALTER DOMAIN public.embed_video OWNER TO shelvacu;

--
-- Name: moment; Type: DOMAIN; Schema: public; Owner: shelvacu
--

CREATE DOMAIN public.moment AS public.__t_moment
	CONSTRAINT moment_check CHECK (((VALUE IS NULL) OR (((VALUE).session_id IS NOT NULL) AND ((VALUE).duration_secs IS NOT NULL) AND ((VALUE).duration_nanos IS NOT NULL) AND ((VALUE).datetime IS NOT NULL) AND ((VALUE).duration_secs >= 0) AND (0 <= (VALUE).duration_nanos) AND ((VALUE).duration_nanos < 1000000000))));


ALTER DOMAIN public.moment OWNER TO shelvacu;

--
-- Name: f_check_no_null(anyarray); Type: FUNCTION; Schema: public; Owner: shelvacu
--

CREATE FUNCTION public.f_check_no_null(anyarray) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT CASE WHEN $1 IS NOT NULL THEN array_position($1, NULL) IS NULL END$_$;


ALTER FUNCTION public.f_check_no_null(anyarray) OWNER TO shelvacu;

--
-- Name: partial_member; Type: DOMAIN; Schema: public; Owner: shelvacu
--

CREATE DOMAIN public.partial_member AS public.__t_partial_member
	CONSTRAINT partial_member_check CHECK (((VALUE IS NULL) OR (((VALUE).deaf IS NOT NULL) AND ((VALUE).mute IS NOT NULL) AND ((VALUE).roles IS NOT NULL) AND public.f_check_no_null((VALUE).roles))));


ALTER DOMAIN public.partial_member OWNER TO shelvacu;

--
-- Name: serenity_current_user; Type: DOMAIN; Schema: public; Owner: shelvacu
--

CREATE DOMAIN public.serenity_current_user AS public.__t_serenity_current_user
	CONSTRAINT serenity_current_user_check CHECK (((VALUE IS NULL) OR (((VALUE).inner_user IS NOT NULL) AND ((VALUE).mfa_enabled IS NOT NULL) AND ((VALUE).verified IS NOT NULL))));


ALTER DOMAIN public.serenity_current_user OWNER TO shelvacu;

--
-- Name: user_presence_game; Type: DOMAIN; Schema: public; Owner: shelvacu
--

CREATE DOMAIN public.user_presence_game AS public.__t_user_presence_game
	CONSTRAINT user_presence_game_check CHECK (((VALUE IS NULL) OR (((VALUE).kind IS NOT NULL) AND ((VALUE).kind = ANY (ARRAY['Playing'::text, 'Streaming'::text, 'Listening'::text])) AND ((VALUE).name IS NOT NULL))));


ALTER DOMAIN public.user_presence_game OWNER TO shelvacu;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: attachment; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.attachment (
    rowid bigint NOT NULL,
    message_rowid bigint NOT NULL,
    discord_id public.snowflake NOT NULL,
    filename text NOT NULL,
    height bigint,
    width bigint,
    proxy_url text NOT NULL,
    size bigint NOT NULL,
    url text NOT NULL
);


ALTER TABLE public.attachment OWNER TO shelvacu;

--
-- Name: attachment_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.attachment_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.attachment_rowid_seq OWNER TO shelvacu;

--
-- Name: attachment_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.attachment_rowid_seq OWNED BY public.attachment.rowid;


--
-- Name: embed; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.embed (
    rowid bigint NOT NULL,
    message_rowid bigint NOT NULL,
    author public.embed_author,
    colour_u32 public.discord_colour,
    description text,
    footer public.embed_footer,
    image public.embed_image,
    kind text NOT NULL,
    provider public.embed_provider,
    thumbnail public.embed_image,
    "timestamp" text,
    title text,
    url text,
    video public.embed_video
);


ALTER TABLE public.embed OWNER TO shelvacu;

--
-- Name: embed_field; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.embed_field (
    rowid bigint NOT NULL,
    embed_rowid bigint NOT NULL,
    inline boolean NOT NULL,
    name text NOT NULL,
    value text NOT NULL
);


ALTER TABLE public.embed_field OWNER TO shelvacu;

--
-- Name: embed_field_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.embed_field_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.embed_field_rowid_seq OWNER TO shelvacu;

--
-- Name: embed_field_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.embed_field_rowid_seq OWNED BY public.embed_field.rowid;


--
-- Name: embed_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.embed_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.embed_rowid_seq OWNER TO shelvacu;

--
-- Name: embed_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.embed_rowid_seq OWNED BY public.embed.rowid;


--
-- Name: emoji; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.emoji (
    rowid bigint NOT NULL,
    guild_rowid bigint NOT NULL,
    discord_id public.snowflake NOT NULL,
    animated boolean NOT NULL,
    name text NOT NULL,
    managed boolean NOT NULL,
    require_colons boolean NOT NULL,
    roles public.snowflake[] NOT NULL,
    CONSTRAINT emoji_roles_check CHECK (public.f_check_no_null(roles))
);


ALTER TABLE public.emoji OWNER TO shelvacu;

--
-- Name: emoji_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.emoji_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.emoji_rowid_seq OWNER TO shelvacu;

--
-- Name: emoji_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.emoji_rowid_seq OWNED BY public.emoji.rowid;


--
-- Name: group_channel; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.group_channel (
    rowid bigint NOT NULL,
    ready_rowid bigint NOT NULL,
    discord_id public.snowflake NOT NULL,
    icon text,
    last_message_id public.snowflake,
    last_pin_timestamp timestamp with time zone,
    name text,
    owner_id public.snowflake NOT NULL,
    recipients public.discord_user[] NOT NULL,
    CONSTRAINT group_channel_recipients_check CHECK (public.f_check_no_null(recipients))
);


ALTER TABLE public.group_channel OWNER TO shelvacu;

--
-- Name: group_channel_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.group_channel_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.group_channel_rowid_seq OWNER TO shelvacu;

--
-- Name: group_channel_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.group_channel_rowid_seq OWNED BY public.group_channel.rowid;


--
-- Name: guild; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.guild (
    rowid bigint NOT NULL,
    ready_rowid bigint,
    discord_id public.snowflake NOT NULL,
    afk_channel_id public.snowflake,
    afk_timeout bigint,
    application_id public.snowflake,
    default_message_notification_level text NOT NULL,
    explicit_content_filter text NOT NULL,
    features text[] NOT NULL,
    icon text,
    joined_at timestamp with time zone,
    large boolean NOT NULL,
    member_count bigint NOT NULL,
    mfa_level text NOT NULL,
    name text NOT NULL,
    owner_id public.snowflake NOT NULL,
    region text NOT NULL,
    splash text,
    system_channel_id public.snowflake,
    verification_level text NOT NULL,
    archive_recvd_at timestamp with time zone NOT NULL,
    guild_create_event_rowid bigint,
    CONSTRAINT guild_default_message_notification_level_check CHECK ((default_message_notification_level = ANY (ARRAY['All'::text, 'Mentions'::text]))),
    CONSTRAINT guild_explicit_content_filter_check CHECK ((explicit_content_filter = ANY (ARRAY['None'::text, 'WithoutRole'::text, 'All'::text]))),
    CONSTRAINT guild_features_check CHECK (public.f_check_no_null(features)),
    CONSTRAINT guild_has_exactly_one_parent CHECK (((((ready_rowid IS NULL))::integer + ((guild_create_event_rowid IS NULL))::integer) = 1)),
    CONSTRAINT guild_mfa_level_check CHECK ((mfa_level = ANY (ARRAY['None'::text, 'Elevated'::text]))),
    CONSTRAINT guild_verification_level_check CHECK ((verification_level = ANY (ARRAY['None'::text, 'Low'::text, 'Medium'::text, 'High'::text, 'Higher'::text])))
);


ALTER TABLE public.guild OWNER TO shelvacu;

--
-- Name: guild_channel; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.guild_channel (
    rowid bigint NOT NULL,
    discord_id public.snowflake NOT NULL,
    guild_rowid bigint,
    bitrate bigint,
    category_id public.snowflake,
    guild_id public.snowflake NOT NULL,
    kind text NOT NULL,
    last_message_id public.snowflake,
    last_pin_timestamp timestamp with time zone,
    name text NOT NULL,
    "position" bigint NOT NULL,
    topic text,
    user_limit bigint,
    nsfw boolean NOT NULL,
    CONSTRAINT guild_channel_kind_check CHECK ((kind = ANY (ARRAY['Text'::text, 'Private'::text, 'Voice'::text, 'Group'::text, 'Category'::text])))
);


ALTER TABLE public.guild_channel OWNER TO shelvacu;

--
-- Name: guild_channel_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.guild_channel_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.guild_channel_rowid_seq OWNER TO shelvacu;

--
-- Name: guild_channel_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.guild_channel_rowid_seq OWNED BY public.guild_channel.rowid;


--
-- Name: guild_create_event; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.guild_create_event (
    rowid bigint NOT NULL,
    is_new boolean NOT NULL,
    recvd_at_datetime timestamp with time zone NOT NULL,
    recvd_at_duration_secs bigint NOT NULL,
    recvd_at_duration_nanos integer NOT NULL,
    session_rowid bigint NOT NULL,
    CONSTRAINT guild_create_event_check CHECK ((((recvd_at_duration_secs IS NOT NULL) AND (recvd_at_duration_nanos IS NOT NULL) AND (session_rowid IS NOT NULL)) OR ((recvd_at_duration_secs IS NULL) AND (recvd_at_duration_nanos IS NULL) AND (session_rowid IS NULL))))
);


ALTER TABLE public.guild_create_event OWNER TO shelvacu;

--
-- Name: guild_create_event_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.guild_create_event_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.guild_create_event_rowid_seq OWNER TO shelvacu;

--
-- Name: guild_create_event_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.guild_create_event_rowid_seq OWNED BY public.guild_create_event.rowid;


--
-- Name: guild_prefixes; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.guild_prefixes (
    guild_id public.snowflake NOT NULL,
    command_prefix text NOT NULL
);


ALTER TABLE public.guild_prefixes OWNER TO shelvacu;

--
-- Name: guild_role; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.guild_role (
    rowid bigint NOT NULL,
    guild_rowid bigint NOT NULL,
    discord_id public.snowflake NOT NULL,
    colour_u32 public.discord_colour,
    hoist boolean NOT NULL,
    managed boolean NOT NULL,
    mentionable boolean NOT NULL,
    name text NOT NULL,
    permissions_bits bigint NOT NULL,
    "position" bigint NOT NULL
);


ALTER TABLE public.guild_role OWNER TO shelvacu;

--
-- Name: guild_role_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.guild_role_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.guild_role_rowid_seq OWNER TO shelvacu;

--
-- Name: guild_role_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.guild_role_rowid_seq OWNED BY public.guild_role.rowid;


--
-- Name: guild_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.guild_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.guild_rowid_seq OWNER TO shelvacu;

--
-- Name: guild_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.guild_rowid_seq OWNED BY public.guild.rowid;


--
-- Name: log_entry; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.log_entry (
    rowid bigint NOT NULL,
    logged_at_datetime timestamp with time zone NOT NULL,
    logged_at_duration_secs bigint NOT NULL,
    logged_at_duration_nanos integer NOT NULL,
    session_rowid bigint NOT NULL,
    log_level text NOT NULL,
    target text NOT NULL,
    module_path text,
    file text,
    line bigint,
    message_body text NOT NULL,
    CONSTRAINT log_entry_log_level_check CHECK ((log_level = ANY (ARRAY['Error'::text, 'Warn'::text, 'Info'::text, 'Debug'::text, 'Trace'::text])))
);


ALTER TABLE public.log_entry OWNER TO shelvacu;

--
-- Name: log_entry_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.log_entry_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.log_entry_rowid_seq OWNER TO shelvacu;

--
-- Name: log_entry_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.log_entry_rowid_seq OWNED BY public.log_entry.rowid;


--
-- Name: member; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.member (
    rowid bigint NOT NULL,
    guild_rowid bigint NOT NULL,
    guild_id public.snowflake NOT NULL,
    deaf boolean NOT NULL,
    joined_at timestamp with time zone,
    mute boolean NOT NULL,
    nick text,
    roles public.snowflake[] NOT NULL,
    user_info public.discord_user NOT NULL,
    CONSTRAINT member_roles_check CHECK (public.f_check_no_null(roles))
);


ALTER TABLE public.member OWNER TO shelvacu;

--
-- Name: member_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.member_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.member_rowid_seq OWNER TO shelvacu;

--
-- Name: member_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.member_rowid_seq OWNED BY public.member.rowid;


--
-- Name: message; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.message (
    rowid bigint NOT NULL,
    discord_id public.snowflake NOT NULL,
    author public.discord_user NOT NULL,
    channel_id public.snowflake NOT NULL,
    content text NOT NULL,
    edited_timestamp timestamp with time zone,
    guild_id public.snowflake,
    kind text,
    member public.partial_member,
    mention_everyone boolean NOT NULL,
    mention_roles public.snowflake[] NOT NULL,
    mentions public.discord_user[] NOT NULL,
    nonce_debug text NOT NULL,
    pinned boolean NOT NULL,
    "timestamp" timestamp with time zone NOT NULL,
    tts boolean NOT NULL,
    webhook_id public.snowflake,
    archive_recvd_at timestamp with time zone NOT NULL,
    CONSTRAINT message_kind_check CHECK ((kind = ANY (ARRAY['Regular'::text, 'GroupRecipientAddition'::text, 'GroupRecipientRemoval'::text, 'GroupCallCreation'::text, 'GroupNameUpdate'::text, 'GroupIconUpdate'::text, 'PinsAdd'::text, 'MemberJoin'::text]))),
    CONSTRAINT message_mention_roles_check CHECK (public.f_check_no_null(mention_roles)),
    CONSTRAINT message_mentions_check CHECK (public.f_check_no_null(mentions))
);


ALTER TABLE public.message OWNER TO shelvacu;

--
-- Name: message_archive_gets; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.message_archive_gets (
    rowid bigint NOT NULL,
    channel_id public.snowflake NOT NULL,
    ready_rowid bigint,
    after_message_id public.snowflake,
    around_message_id public.snowflake,
    before_message_id public.snowflake,
    start_message_id public.snowflake NOT NULL,
    end_message_id public.snowflake NOT NULL,
    message_count_requested bigint NOT NULL,
    message_count_received bigint NOT NULL,
    legacy boolean DEFAULT false NOT NULL,
    CONSTRAINT message_archive_gets_check CHECK ((((after_message_id IS NULL) AND (around_message_id IS NULL) AND (before_message_id IS NOT NULL)) OR ((after_message_id IS NULL) AND (around_message_id IS NOT NULL) AND (before_message_id IS NULL)) OR ((after_message_id IS NOT NULL) AND (around_message_id IS NULL) AND (before_message_id IS NULL)) OR (legacy AND (after_message_id IS NULL) AND (around_message_id IS NULL) AND (before_message_id IS NULL))))
);


ALTER TABLE public.message_archive_gets OWNER TO shelvacu;

--
-- Name: message_archive_gets_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.message_archive_gets_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.message_archive_gets_rowid_seq OWNER TO shelvacu;

--
-- Name: message_archive_gets_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.message_archive_gets_rowid_seq OWNED BY public.message_archive_gets.rowid;


--
-- Name: message_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.message_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.message_rowid_seq OWNER TO shelvacu;

--
-- Name: message_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.message_rowid_seq OWNED BY public.message.rowid;


--
-- Name: migration_version; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.migration_version (
    version bigint NOT NULL,
    enforce_single_row boolean DEFAULT true NOT NULL,
    CONSTRAINT migration_version_enforce_single_row_check CHECK (enforce_single_row),
    CONSTRAINT migration_version_version_check CHECK ((version >= 7))
);


ALTER TABLE public.migration_version OWNER TO shelvacu;

--
-- Name: permission_overwrite; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.permission_overwrite (
    rowid bigint NOT NULL,
    guild_channel_rowid bigint NOT NULL,
    allow_bits bigint NOT NULL,
    deny_bits bigint NOT NULL,
    permission_overwrite_type text NOT NULL,
    permission_overwrite_id public.snowflake NOT NULL,
    CONSTRAINT permission_overwrite_permission_overwrite_type_check CHECK ((permission_overwrite_type = ANY (ARRAY['Member'::text, 'Role'::text])))
);


ALTER TABLE public.permission_overwrite OWNER TO shelvacu;

--
-- Name: permission_overwrite_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.permission_overwrite_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.permission_overwrite_rowid_seq OWNER TO shelvacu;

--
-- Name: permission_overwrite_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.permission_overwrite_rowid_seq OWNED BY public.permission_overwrite.rowid;


--
-- Name: private_channel; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.private_channel (
    rowid bigint NOT NULL,
    ready_rowid bigint NOT NULL,
    discord_id public.snowflake NOT NULL,
    last_message_id public.snowflake,
    last_pin_timestamp timestamp with time zone,
    kind text NOT NULL,
    recipient public.discord_user NOT NULL
);


ALTER TABLE public.private_channel OWNER TO shelvacu;

--
-- Name: private_channel_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.private_channel_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.private_channel_rowid_seq OWNER TO shelvacu;

--
-- Name: private_channel_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.private_channel_rowid_seq OWNED BY public.private_channel.rowid;


--
-- Name: raw_message; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.raw_message (
    rowid bigint NOT NULL,
    recvd_at_datetime timestamp with time zone NOT NULL,
    recvd_at_duration_secs bigint NOT NULL,
    recvd_at_duration_nanos integer NOT NULL,
    session_rowid bigint NOT NULL,
    kind text NOT NULL,
    content_text text,
    content_binary bytea,
    CONSTRAINT raw_message_check CHECK ((((kind = 'Text'::text) AND (content_text IS NOT NULL) AND (content_binary IS NULL)) OR ((kind = ANY (ARRAY['Binary'::text, 'Ping'::text, 'Pong'::text])) AND (content_text IS NULL) AND (content_binary IS NULL)) OR ((kind = 'Close'::text) AND (content_text IS NULL)))),
    CONSTRAINT raw_message_kind_check CHECK ((kind = ANY (ARRAY['Text'::text, 'Binary'::text, 'Close'::text, 'Ping'::text, 'Pong'::text])))
);


ALTER TABLE public.raw_message OWNER TO shelvacu;

--
-- Name: raw_message_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.raw_message_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.raw_message_rowid_seq OWNER TO shelvacu;

--
-- Name: raw_message_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.raw_message_rowid_seq OWNED BY public.raw_message.rowid;


--
-- Name: reaction; Type: TABLE; Schema: public; Owner: shelvacu
--
--LEFTOFF
CREATE TABLE public.reaction (
    rowid bigint NOT NULL,
    message_rowid bigint NOT NULL,
    count bigint NOT NULL,
    me boolean NOT NULL,
    reaction_is_custom boolean NOT NULL,
    reaction_animated boolean,
    reaction_id public.snowflake,
    reaction_name text,
    reaction_string text,
    CONSTRAINT reaction_check CHECK (((reaction_is_custom AND (reaction_animated IS NOT NULL) AND (reaction_id IS NOT NULL) AND (reaction_string IS NULL)) OR ((NOT reaction_is_custom) AND (reaction_string IS NOT NULL) AND (reaction_animated IS NULL) AND (reaction_id IS NULL) AND (reaction_name IS NULL))))
);


ALTER TABLE public.reaction OWNER TO shelvacu;

--
-- Name: reaction_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.reaction_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.reaction_rowid_seq OWNER TO shelvacu;

--
-- Name: reaction_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.reaction_rowid_seq OWNED BY public.reaction.rowid;


--
-- Name: ready; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.ready (
    rowid bigint NOT NULL,
    session_id text NOT NULL,
    shard bigint[],
    trace text[] NOT NULL,
    user_info public.serenity_current_user NOT NULL,
    version bigint NOT NULL,
    CONSTRAINT ready_shard_check CHECK (((shard IS NULL) OR (array_length(shard, 1) = 2))),
    CONSTRAINT ready_trace_check CHECK (public.f_check_no_null(trace))
);


ALTER TABLE public.ready OWNER TO shelvacu;

--
-- Name: ready_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.ready_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.ready_rowid_seq OWNER TO shelvacu;

--
-- Name: ready_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.ready_rowid_seq OWNED BY public.ready.rowid;


--
-- Name: run_session; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.run_session (
    rowid bigint NOT NULL,
    started_at timestamp with time zone NOT NULL,
    pkg_name text,
    version text,
    target text,
    build_timestamp text,
    git_sha_ref text,
    git_commit_date text
);


ALTER TABLE public.run_session OWNER TO shelvacu;

--
-- Name: run_session_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.run_session_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.run_session_rowid_seq OWNER TO shelvacu;

--
-- Name: run_session_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.run_session_rowid_seq OWNED BY public.run_session.rowid;


--
-- Name: shard_stage_update_event; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.shard_stage_update_event (
    rowid bigint NOT NULL,
    happened_at public.moment,
    new_stage text NOT NULL,
    old_stage text NOT NULL,
    shard_id public.snowflake NOT NULL,
    CONSTRAINT shard_stage_update_event_new_stage_check CHECK ((new_stage = ANY (ARRAY['Connected'::text, 'Connecting'::text, 'Disconnected'::text, 'Handshake'::text, 'Identifying'::text, 'Resuming'::text]))),
    CONSTRAINT shard_stage_update_event_new_stage_check1 CHECK ((new_stage = ANY (ARRAY['Connected'::text, 'Connecting'::text, 'Disconnected'::text, 'Handshake'::text, 'Identifying'::text, 'Resuming'::text])))
);


ALTER TABLE public.shard_stage_update_event OWNER TO shelvacu;

--
-- Name: shard_stage_update_event_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.shard_stage_update_event_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.shard_stage_update_event_rowid_seq OWNER TO shelvacu;

--
-- Name: shard_stage_update_event_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.shard_stage_update_event_rowid_seq OWNED BY public.shard_stage_update_event.rowid;


--
-- Name: sqlite_migration_progress; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.sqlite_migration_progress (
    progress_counter bigint NOT NULL,
    enforce_single_row boolean DEFAULT true NOT NULL,
    CONSTRAINT sqlite_migration_progress_enforce_single_row_check CHECK (enforce_single_row)
);


ALTER TABLE public.sqlite_migration_progress OWNER TO shelvacu;

--
-- Name: user_presence; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.user_presence (
    rowid bigint NOT NULL,
    ready_rowid bigint,
    guild_rowid bigint,
    game public.user_presence_game,
    last_modified bigint,
    nick text,
    status text NOT NULL,
    user_id public.snowflake NOT NULL,
    CONSTRAINT user_presence_check CHECK ((((ready_rowid IS NULL) AND (guild_rowid IS NOT NULL)) OR ((ready_rowid IS NOT NULL) AND (guild_rowid IS NULL)))),
    CONSTRAINT user_presence_status_check CHECK ((status = ANY (ARRAY['DoNotDisturb'::text, 'Idle'::text, 'Invisible'::text, 'Offline'::text, 'Online'::text])))
);


ALTER TABLE public.user_presence OWNER TO shelvacu;

--
-- Name: user_presence_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.user_presence_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_presence_rowid_seq OWNER TO shelvacu;

--
-- Name: user_presence_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.user_presence_rowid_seq OWNED BY public.user_presence.rowid;


--
-- Name: voice_state; Type: TABLE; Schema: public; Owner: shelvacu
--

CREATE TABLE public.voice_state (
    rowid bigint NOT NULL,
    guild_rowid bigint NOT NULL,
    channel_id public.snowflake,
    deaf boolean NOT NULL,
    mute boolean NOT NULL,
    self_deaf boolean NOT NULL,
    self_mute boolean NOT NULL,
    session_id text NOT NULL,
    suppress boolean NOT NULL,
    token text,
    user_id public.snowflake NOT NULL
);


ALTER TABLE public.voice_state OWNER TO shelvacu;

--
-- Name: voice_state_rowid_seq; Type: SEQUENCE; Schema: public; Owner: shelvacu
--

CREATE SEQUENCE public.voice_state_rowid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.voice_state_rowid_seq OWNER TO shelvacu;

--
-- Name: voice_state_rowid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shelvacu
--

ALTER SEQUENCE public.voice_state_rowid_seq OWNED BY public.voice_state.rowid;


--
-- Name: attachment rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.attachment ALTER COLUMN rowid SET DEFAULT nextval('public.attachment_rowid_seq'::regclass);


--
-- Name: embed rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.embed ALTER COLUMN rowid SET DEFAULT nextval('public.embed_rowid_seq'::regclass);


--
-- Name: embed_field rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.embed_field ALTER COLUMN rowid SET DEFAULT nextval('public.embed_field_rowid_seq'::regclass);


--
-- Name: emoji rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.emoji ALTER COLUMN rowid SET DEFAULT nextval('public.emoji_rowid_seq'::regclass);


--
-- Name: group_channel rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.group_channel ALTER COLUMN rowid SET DEFAULT nextval('public.group_channel_rowid_seq'::regclass);


--
-- Name: guild rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.guild ALTER COLUMN rowid SET DEFAULT nextval('public.guild_rowid_seq'::regclass);


--
-- Name: guild_channel rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.guild_channel ALTER COLUMN rowid SET DEFAULT nextval('public.guild_channel_rowid_seq'::regclass);


--
-- Name: guild_create_event rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.guild_create_event ALTER COLUMN rowid SET DEFAULT nextval('public.guild_create_event_rowid_seq'::regclass);


--
-- Name: guild_role rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.guild_role ALTER COLUMN rowid SET DEFAULT nextval('public.guild_role_rowid_seq'::regclass);


--
-- Name: log_entry rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.log_entry ALTER COLUMN rowid SET DEFAULT nextval('public.log_entry_rowid_seq'::regclass);


--
-- Name: member rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.member ALTER COLUMN rowid SET DEFAULT nextval('public.member_rowid_seq'::regclass);


--
-- Name: message rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.message ALTER COLUMN rowid SET DEFAULT nextval('public.message_rowid_seq'::regclass);


--
-- Name: message_archive_gets rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.message_archive_gets ALTER COLUMN rowid SET DEFAULT nextval('public.message_archive_gets_rowid_seq'::regclass);


--
-- Name: permission_overwrite rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.permission_overwrite ALTER COLUMN rowid SET DEFAULT nextval('public.permission_overwrite_rowid_seq'::regclass);


--
-- Name: private_channel rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.private_channel ALTER COLUMN rowid SET DEFAULT nextval('public.private_channel_rowid_seq'::regclass);


--
-- Name: raw_message rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.raw_message ALTER COLUMN rowid SET DEFAULT nextval('public.raw_message_rowid_seq'::regclass);


--
-- Name: reaction rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.reaction ALTER COLUMN rowid SET DEFAULT nextval('public.reaction_rowid_seq'::regclass);


--
-- Name: ready rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.ready ALTER COLUMN rowid SET DEFAULT nextval('public.ready_rowid_seq'::regclass);


--
-- Name: run_session rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.run_session ALTER COLUMN rowid SET DEFAULT nextval('public.run_session_rowid_seq'::regclass);


--
-- Name: shard_stage_update_event rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.shard_stage_update_event ALTER COLUMN rowid SET DEFAULT nextval('public.shard_stage_update_event_rowid_seq'::regclass);


--
-- Name: user_presence rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.user_presence ALTER COLUMN rowid SET DEFAULT nextval('public.user_presence_rowid_seq'::regclass);


--
-- Name: voice_state rowid; Type: DEFAULT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.voice_state ALTER COLUMN rowid SET DEFAULT nextval('public.voice_state_rowid_seq'::regclass);


--
-- Name: attachment attachment_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.attachment
    ADD CONSTRAINT attachment_pkey PRIMARY KEY (rowid);


--
-- Name: embed_field embed_field_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.embed_field
    ADD CONSTRAINT embed_field_pkey PRIMARY KEY (rowid);


--
-- Name: embed embed_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.embed
    ADD CONSTRAINT embed_pkey PRIMARY KEY (rowid);


--
-- Name: emoji emoji_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.emoji
    ADD CONSTRAINT emoji_pkey PRIMARY KEY (rowid);


--
-- Name: group_channel group_channel_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.group_channel
    ADD CONSTRAINT group_channel_pkey PRIMARY KEY (rowid);


--
-- Name: guild_channel guild_channel_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.guild_channel
    ADD CONSTRAINT guild_channel_pkey PRIMARY KEY (rowid);


--
-- Name: guild_create_event guild_create_event_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.guild_create_event
    ADD CONSTRAINT guild_create_event_pkey PRIMARY KEY (rowid);


--
-- Name: guild guild_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.guild
    ADD CONSTRAINT guild_pkey PRIMARY KEY (rowid);


--
-- Name: guild_prefixes guild_prefixes_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.guild_prefixes
    ADD CONSTRAINT guild_prefixes_pkey PRIMARY KEY (guild_id);


--
-- Name: guild_role guild_role_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.guild_role
    ADD CONSTRAINT guild_role_pkey PRIMARY KEY (rowid);


--
-- Name: log_entry log_entry_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.log_entry
    ADD CONSTRAINT log_entry_pkey PRIMARY KEY (rowid);


--
-- Name: member member_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.member
    ADD CONSTRAINT member_pkey PRIMARY KEY (rowid);


--
-- Name: message_archive_gets message_archive_gets_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.message_archive_gets
    ADD CONSTRAINT message_archive_gets_pkey PRIMARY KEY (rowid);


--
-- Name: message message_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.message
    ADD CONSTRAINT message_pkey PRIMARY KEY (rowid);


--
-- Name: migration_version migration_version_enforce_single_row_key; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.migration_version
    ADD CONSTRAINT migration_version_enforce_single_row_key UNIQUE (enforce_single_row);


--
-- Name: permission_overwrite permission_overwrite_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.permission_overwrite
    ADD CONSTRAINT permission_overwrite_pkey PRIMARY KEY (rowid);


--
-- Name: private_channel private_channel_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.private_channel
    ADD CONSTRAINT private_channel_pkey PRIMARY KEY (rowid);


--
-- Name: raw_message raw_message_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.raw_message
    ADD CONSTRAINT raw_message_pkey PRIMARY KEY (rowid);


--
-- Name: raw_message raw_message_session_rowid_recvd_at_duration_secs_recvd_at_d_key; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.raw_message
    ADD CONSTRAINT raw_message_session_rowid_recvd_at_duration_secs_recvd_at_d_key UNIQUE (session_rowid, recvd_at_duration_secs, recvd_at_duration_nanos);


--
-- Name: reaction reaction_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.reaction
    ADD CONSTRAINT reaction_pkey PRIMARY KEY (rowid);


--
-- Name: ready ready_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.ready
    ADD CONSTRAINT ready_pkey PRIMARY KEY (rowid);


--
-- Name: run_session run_session_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.run_session
    ADD CONSTRAINT run_session_pkey PRIMARY KEY (rowid);


--
-- Name: shard_stage_update_event shard_stage_update_event_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.shard_stage_update_event
    ADD CONSTRAINT shard_stage_update_event_pkey PRIMARY KEY (rowid);


--
-- Name: sqlite_migration_progress sqlite_migration_progress_enforce_single_row_key; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.sqlite_migration_progress
    ADD CONSTRAINT sqlite_migration_progress_enforce_single_row_key UNIQUE (enforce_single_row);


--
-- Name: user_presence user_presence_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.user_presence
    ADD CONSTRAINT user_presence_pkey PRIMARY KEY (rowid);


--
-- Name: voice_state voice_state_pkey; Type: CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.voice_state
    ADD CONSTRAINT voice_state_pkey PRIMARY KEY (rowid);


--
-- Name: mag_channel_after; Type: INDEX; Schema: public; Owner: shelvacu
--

CREATE INDEX mag_channel_after ON public.message_archive_gets USING btree (channel_id, after_message_id);


--
-- Name: mag_channel_around; Type: INDEX; Schema: public; Owner: shelvacu
--

CREATE INDEX mag_channel_around ON public.message_archive_gets USING btree (channel_id, around_message_id);


--
-- Name: mag_channel_before; Type: INDEX; Schema: public; Owner: shelvacu
--

CREATE INDEX mag_channel_before ON public.message_archive_gets USING btree (channel_id, before_message_id);


--
-- Name: mag_channel_end; Type: INDEX; Schema: public; Owner: shelvacu
--

CREATE INDEX mag_channel_end ON public.message_archive_gets USING btree (channel_id, end_message_id);


--
-- Name: mag_channel_start; Type: INDEX; Schema: public; Owner: shelvacu
--

CREATE INDEX mag_channel_start ON public.message_archive_gets USING btree (channel_id, start_message_id);


--
-- Name: attachment attachment_message_rowid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.attachment
    ADD CONSTRAINT attachment_message_rowid_fkey FOREIGN KEY (message_rowid) REFERENCES public.message(rowid);


--
-- Name: embed_field embed_field_embed_rowid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.embed_field
    ADD CONSTRAINT embed_field_embed_rowid_fkey FOREIGN KEY (embed_rowid) REFERENCES public.embed(rowid);


--
-- Name: embed embed_message_rowid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.embed
    ADD CONSTRAINT embed_message_rowid_fkey FOREIGN KEY (message_rowid) REFERENCES public.message(rowid);


--
-- Name: emoji emoji_guild_rowid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.emoji
    ADD CONSTRAINT emoji_guild_rowid_fkey FOREIGN KEY (guild_rowid) REFERENCES public.guild(rowid);


--
-- Name: group_channel group_channel_ready_rowid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.group_channel
    ADD CONSTRAINT group_channel_ready_rowid_fkey FOREIGN KEY (ready_rowid) REFERENCES public.ready(rowid);


--
-- Name: guild_channel guild_channel_guild_rowid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.guild_channel
    ADD CONSTRAINT guild_channel_guild_rowid_fkey FOREIGN KEY (guild_rowid) REFERENCES public.guild(rowid);


--
-- Name: guild_create_event guild_create_event_session_rowid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.guild_create_event
    ADD CONSTRAINT guild_create_event_session_rowid_fkey FOREIGN KEY (session_rowid) REFERENCES public.run_session(rowid);


--
-- Name: guild guild_guild_create_event_rowid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.guild
    ADD CONSTRAINT guild_guild_create_event_rowid_fkey FOREIGN KEY (guild_create_event_rowid) REFERENCES public.guild_create_event(rowid);


--
-- Name: guild guild_ready_rowid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.guild
    ADD CONSTRAINT guild_ready_rowid_fkey FOREIGN KEY (ready_rowid) REFERENCES public.ready(rowid);


--
-- Name: guild_role guild_role_guild_rowid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.guild_role
    ADD CONSTRAINT guild_role_guild_rowid_fkey FOREIGN KEY (guild_rowid) REFERENCES public.guild(rowid);


--
-- Name: log_entry log_entry_session_rowid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.log_entry
    ADD CONSTRAINT log_entry_session_rowid_fkey FOREIGN KEY (session_rowid) REFERENCES public.run_session(rowid);


--
-- Name: member member_guild_rowid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.member
    ADD CONSTRAINT member_guild_rowid_fkey FOREIGN KEY (guild_rowid) REFERENCES public.guild(rowid);


--
-- Name: message_archive_gets message_archive_gets_ready_rowid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.message_archive_gets
    ADD CONSTRAINT message_archive_gets_ready_rowid_fkey FOREIGN KEY (ready_rowid) REFERENCES public.ready(rowid);


--
-- Name: permission_overwrite permission_overwrite_guild_channel_rowid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.permission_overwrite
    ADD CONSTRAINT permission_overwrite_guild_channel_rowid_fkey FOREIGN KEY (guild_channel_rowid) REFERENCES public.guild_channel(rowid);


--
-- Name: private_channel private_channel_ready_rowid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.private_channel
    ADD CONSTRAINT private_channel_ready_rowid_fkey FOREIGN KEY (ready_rowid) REFERENCES public.ready(rowid);


--
-- Name: raw_message raw_message_session_rowid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.raw_message
    ADD CONSTRAINT raw_message_session_rowid_fkey FOREIGN KEY (session_rowid) REFERENCES public.run_session(rowid);


--
-- Name: reaction reaction_message_rowid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.reaction
    ADD CONSTRAINT reaction_message_rowid_fkey FOREIGN KEY (message_rowid) REFERENCES public.message(rowid);


--
-- Name: user_presence user_presence_guild_rowid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.user_presence
    ADD CONSTRAINT user_presence_guild_rowid_fkey FOREIGN KEY (guild_rowid) REFERENCES public.guild(rowid);


--
-- Name: user_presence user_presence_ready_rowid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.user_presence
    ADD CONSTRAINT user_presence_ready_rowid_fkey FOREIGN KEY (ready_rowid) REFERENCES public.ready(rowid);


--
-- Name: voice_state voice_state_guild_rowid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: shelvacu
--

ALTER TABLE ONLY public.voice_state
    ADD CONSTRAINT voice_state_guild_rowid_fkey FOREIGN KEY (guild_rowid) REFERENCES public.guild(rowid);


--
-- PostgreSQL database dump complete
--

