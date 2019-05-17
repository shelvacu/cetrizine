use super::pg;
use super::pg::TlsMode;
use super::sqlite;
use super::sqlite::{NO_PARAMS, OptionalExtension};

#[derive(Debug)]
enum MTWeirdHack<T> {
    Arr(Vec<MTWeirdHack<T>>),
    Val(T)
}

impl<T> MTWeirdHack<T> {
    //I could do this with borrows, but the way this is to be used it should only be used once.
    pub fn append_to(self, result_vec: &mut Vec<T>) -> () {
        match self {
            MTWeirdHack::Arr(inner_vec) => {
                for thing in inner_vec {
                    thing.append_to(result_vec)
                }
            },
            MTWeirdHack::Val(inner_item) => result_vec.push(inner_item),
        }
    }
    pub fn flatten(self) -> Vec<T> {
        let mut res:Vec<T> = Vec::new();
        self.append_to(&mut res);
        return res;
    }
}

pub fn filter_string(_:sqlite::Connection, v: &[u8]) -> String {
    let s:String = std::str::from_utf8(v).unwrap().into();
    if s.chars().all(|c| c != '\0' && c != '$') {
        s
    } else {
        //+4 is a guess, if we're wrong sometimes that's okay, it'll just reallocate
        let mut res = String::with_capacity(s.len() + 4);
        for c in s.chars() {
            match c {
                _ if c == '\0' => {
                    res.push('$');
                    res.push('0');
                },
                _ if c == '$' => {
                    res.push('$');
                    res.push('$');
                },
                c => {
                    res.push(c);
                },
            }
        }
        res
    }
}

pub fn id_arr(conn:sqlite::Connection, i:i64) -> Vec<i64> {
    conn.prepare_cached("SELECT id FROM id WHERE id_arr_rowid = ?1").unwrap().query_map(
        &[&i as &sqlite::ToSql],
        |r| {
            let i:i64 = r.get::<_,String>(0).unwrap().parse().unwrap();
            Ok(i)
        }
    ).unwrap().map(Result::unwrap).collect()
}

macro_rules! __mt_pg_column_names {
    ( $prefix:expr, $pg_name:ident => $func:ident () -> $ret:ty => !, $($token:tt)* ) => {
        MTWeirdHack::Arr(vec!(MTWeirdHack::Val(format!("{}{}",$prefix,stringify!($pg_name)).into()), __mt_pg_column_names!($prefix,$($token)*)))
    };
    ( $prefix:expr, $pg_name:ident => $conv:ty => !, $($token:tt)* ) => {
        MTWeirdHack::Arr(vec!(MTWeirdHack::Val(format!("{}{}",$prefix,stringify!($pg_name)).into()), __mt_pg_column_names!($prefix,$($token)*)))
    };
    ( $prefix:expr, $pg_name:ident => $func:ident () -> $ret:ty => $sql_name:ident, $($token:tt)* ) => {
        MTWeirdHack::Arr(vec!(MTWeirdHack::Val(format!("{}{}",$prefix,stringify!($pg_name)).into()), __mt_pg_column_names!($prefix,$($token)*)))
    };
    ( $prefix:expr, $pg_name:ident => $conv:ty => $sql_name:ident, $($token:tt)* ) => {
        MTWeirdHack::Arr(vec!(MTWeirdHack::Val(format!("{}{}",$prefix,stringify!($pg_name)).into()), __mt_pg_column_names!($prefix,$($token)*)))
    };
    ( $prefix:expr, $pg_name:ident => $cname:ident $subtree:tt, $($token:tt)* ) => {
        MTWeirdHack::Arr(vec!(MTWeirdHack::Val(format!("{}{}",$prefix,stringify!($pg_name)).into()), __mt_pg_column_names!($prefix,$($token)*)))
    };
    ( $prefix:expr, $pg_name:ident => $cname:ident option $subtree:tt, $($token:tt)* ) => {
        MTWeirdHack::Arr(
            vec!(
                MTWeirdHack::Val(
                    format!(
                        "{}{}",
                        $prefix,
                        stringify!($pg_name)
                    ),
                ),
                __mt_pg_column_names!(
                    $prefix,
                    $($token)*
                )
            )
        )
    };
    ($prefix:expr, ) => {MTWeirdHack::Arr(Vec::new())};
}    

macro_rules! __mt_deconstruct_sql_column_names {
    ( $prefix:expr, { $($token:tt)* } ) => {
        __mt_sql_column_names!($prefix, $($token)*)
    }
}

macro_rules! __mt_sql_column_names {
    ( $prefix:expr, $pg_name:ident => $func:ident () -> $ret:ty => !, $($token:tt)* ) => {
        MTWeirdHack::Arr(vec!(MTWeirdHack::Val(format!("{}{}",$prefix,stringify!($pg_name)).into()), __mt_sql_column_names!($prefix,$($token)*)))
    };
    ( $prefix:expr, $pg_name:ident => $conv:ty => !, $($token:tt)* ) => {
        MTWeirdHack::Arr(vec!(MTWeirdHack::Val(format!("{}{}",$prefix,stringify!($pg_name)).into()), __mt_sql_column_names!($prefix,$($token)*)))
    };
    ( $prefix:expr, $pg_name:ident => $func:ident () -> $ret:ty => $sql_name:ident, $($token:tt)* ) => {
        MTWeirdHack::Arr(vec!(MTWeirdHack::Val(format!("{}{}",$prefix,stringify!($sql_name)).into()), __mt_sql_column_names!($prefix,$($token)*)))
    };
    ( $prefix:expr, $pg_name:ident => $conv:ty => $sql_name:ident, $($token:tt)* ) => {
        MTWeirdHack::Arr(vec!(MTWeirdHack::Val(format!("{}{}",$prefix,stringify!($sql_name)).into()), __mt_sql_column_names!($prefix,$($token)*)))
    };
    ( $prefix:expr, $pg_name:ident => $cname:ident $subtree:tt, $($token:tt)* ) => {
        MTWeirdHack::Arr(vec!(__mt_deconstruct_sql_column_names!(format!("{}{}_",$prefix,stringify!($pg_name)),$subtree), __mt_sql_column_names!($prefix,$($token)*)))
    };
    ( $prefix:expr, $pg_name:ident => $cname:ident option $subtree:tt, $($token:tt)* ) => {
        MTWeirdHack::Arr(vec!(MTWeirdHack::Val(format!("{}{}_is_some ",$prefix,stringify!($pg_name))),__mt_deconstruct_sql_column_names!(format!("{}{}_",$prefix,stringify!($pg_name)),$subtree), __mt_sql_column_names!($prefix,$($token)*)))
    };
    ($prefix:expr, ) => {MTWeirdHack::Arr(Vec::new())};
}

macro_rules! __mt_struct_attrs {
    ( $pg_name:ident => $func:ident () -> $ret:ty => !, $($token:tt)* ) => {
        $pg_name:$ret,
        __mt_struct_attrs($($token)*)
    };
    ( $pg_name:ident => $conv:ty => !, $($token:tt)* ) => {
        $pg_name:$conv,
        __mt_struct_attrs($($token)*)
    };
    ( $pg_name:ident => $func:ident () -> $ret:ty => $sql_name:ident, $($token:tt)* ) => {
        $pg_name:$ret,
        __mt_struct_attrs($($token)*)
    };
    ( $pg_name:ident => $conv:ty => $sql_name:ident, $($token:tt)* ) => {
        $pg_name:$conv,
        __mt_struct_attrs($($token)*)
    };
    ( $pg_name:ident => $cname:ident $subtree:tt, $($token:tt)* ) => {
        $pg_name:$cname,
        __mt_struct_attrs($($token)*)
    };
    ( $pg_name:ident => $cname:ident option $subtree:tt, $($token:tt)* ) => {
        $pg_name:Option<$cname>,
        __mt_struct_attrs($($token)*)
    };
    () => {};
}

macro_rules! __mt_define_types_unwrap {
    ( ( $($token:tt)* ) ) => { __mt_define_types($($token)*) };
}

macro_rules! __mt_define_types {
    ( $pg_name:ident => $func:ident () -> $ret:ty => !, $($token:tt)* ) => {
        __mt_define_types($($token)*)
    };
    ( $pg_name:ident => $conv:ty => !, $($token:tt)* ) => {
        __mt_define_types($($token)*)
    };
    ( $pg_name:ident => $func:ident () -> $ret:ty => $sql_name:ident, $($token:tt)* ) => {
        __mt_define_types($($token)*)
    };
    ( $pg_name:ident => $conv:ty => $sql_name:ident, $($token:tt)* ) => {
        __mt_define_types($($token)*)
    };
    ( $pg_name:ident => $cname:ident $subtree:tt, $($token:tt)* ) => {
        __mt_define_types_unwrap{$subtree}
        
        #[derive(Debug,ToSql)]
        struct $cname {
            __mt_struct_attrs($subtree)
        }

        __mt_define_types($($token)*)
    };
    ( $pg_name:ident => $cname:ident option $subtree:tt, $($token:tt)* ) => {
        __mt_define_types_unwrap{$subtree}
        
        #[derive(Debug,ToSql)]
        struct $cname {
            __mt_struct_attrs($subtree)
        }

        __mt_define_types($($token)*)
    };
    () => {};
}

//macro_rules! __mt_values_arr

macro_rules! migrate_table {
    (
        ( $pg_conn:ident, $sql_conn:ident, $table_name:ident ) $( $token:tt )*
    ) => {{
        //__mt_define_types!{$($token)*}
        let table_name = stringify!($table_name);
        println!("Migrating table {}",table_name);
        let sql_columns = __mt_sql_column_names!("",$($token)*).flatten();
        println!("{:?}", &sql_columns);
        let sqlite_select_str = format!("SELECT {} FROM {}",sql_columns.join(","),table_name);
        let mut sqlite_select_stmt = $sql_conn.prepare_cached(&sqlite_select_str).unwrap();

        let pg_column_names = __mt_pg_column_names!("",$($token)*).flatten();
        let question_marks = std::iter::repeat("?").take(pg_column_names.len()).collect::<Vec<&str>>().join(",");
        let postgres_insert_str = format!("INSERT INTO {} ({}) VALUES ({})",table_name,pg_column_names.join(","),question_marks);
        


        println!("pg insert str {:?}", postgres_insert_str);





        let count_str = format!("SELECT COUNT(*) FROM {}",table_name);
        let count:i64 = $sql_conn.query_row(&count_str,NO_PARAMS,|r| r.get(0)).unwrap();
        println!("{} rows",count);
        //let mut rows = sqlite_select_stmt.query(NO_PARAMS).unwrap();

        //while let Some(row) = rows.next().unwrap() {
        //    __mt_values_arr!(row
    }};
}

pub fn migrate_sqlite_to_postgres(/*pg_conn: &mut pg::Transaction*/) -> () {
    //TODO: Ensure postgres database is *empty*
    let mut pg_conn = pg::Connection::connect("postgres://shelvacu@%2Fvar%2Frun%2Fpostgresql:5434/detroit", TlsMode::None).unwrap();
    let mut pg_trans = pg_conn.transaction().unwrap();
    //let mut pg_conn = pg::Connection::connect("postgres://shelvacu@localhost:5434/detroit", TlsMode::None).unwrap();
    //let sql_conn = 5;
    let mut sql_conn = make_sqlite_connection().expect("could not establish database connection");

    trace_macros!(true);
    migrate_table!{
    //__mt_define_types!{
        (pg_trans, sql_conn, message)
        rowid => i64 => !,
        discord_id => i64 => !,
        author => MTAuthor {
            id => i64 => !,
            avatar => String => !,
            is_bot => bool => !,
            discriminator => i64 => !,
            name => String => !,
        },
        channel_id => i64 => !,
        content => filter_string() -> String => content_binary,
        edited_timestamp => Option<chrono::DateTime<chrono::FixedOffset>> => !,
        guild_id => i64 => !,
        kind => String => !,
        member => MTMember option{
            deaf => bool => !,
            joined_at => Option<chrono::DateTime<chrono::FixedOffset>> => !,
            mute => bool => !,
            roles => id_arr() -> Vec<i64> => !,
        },
        mention_everyone => bool => !,
        mention_roles => id_arr() -> Vec<i64> => !,
        nonce_debug => String => !,
        pinned => bool => !,
        timestamp => chrono::DateTime<chrono::FixedOffset> => !,
        tts => bool => !,
        webhook_id => Option<i64> => !,
        archive_recvd_at => chrono::DateTime<chrono::FixedOffset> => !,
    }
    trace_macros!(false);
    
    pg_trans.commit().expect("Failed to commit txn");
    //sql_conn.prepare_cached("SELECT");
}

pub fn make_sqlite_connection() -> Result<sqlite::Connection, sqlite::Error> {
    let db_fn = std::env::var("DATABASE_FILENAME").expect("Must provide database filename in DATABASE_FILENAME environment variable. File will be created if it does not already exist");
    let conn = sqlite::Connection::open(db_fn).expect("couldnt open connection");

    conn.set_db_config(
        sqlite::config::DbConfig::SQLITE_DBCONFIG_ENABLE_FKEY,
        true,
    )?;//.expect("could not set db config to enable foreign keys");
    conn.set_prepared_statement_cache_capacity(30);//.expect("could not set cache capacity");

    return Ok(conn);
}

pub fn sqlite_migrate() -> () {
    let mut conn = make_sqlite_connection().expect("could not establish database connection");

    conn.execute("CREATE TABLE IF NOT EXISTS message (
rowid integer primary key autoincrement,
discord_id text not null,
--attachments
author_id text not null,
author_avatar text,
author_is_bot int not null, --bool
author_discriminator int not null,
author_name text not null,
channel_id text not null,
content text not null,
edited_timestamp text, --datetime
--embeds
guild_id text,
kind text,
member_is_some int not null, --bool
member_deaf int, --bool
member_joined_at text, --datetime
member_mute int, --bool
member_roles int REFERENCES id_arr(row_id),
--mentions
mention_everyone int not null, --bool
mention_roles int not null REFERENCES id_arr(row_id),
nonce_debug text not null, --{:?}
pinned int not null, --bool
--reactions
timestamp text not null,
tts int not null, --bool
webhook_id text,
archive_recvd_at text not null --datetime
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS user_mention (
rowid integer primary key autoincrement,
message_rowid int not null REFERENCES message(rowid),
id int not null,
avatar text,
bot int not null, --bool
discriminator int not null,
name text not null
)", NO_PARAMS).unwrap();
    
    conn.execute("CREATE TABLE IF NOT EXISTS reaction (
rowid integer primary key autoincrement,
message_rowid int not null REFERENCES message(rowid),
count int not null,
me int not null, --bool
reaction_is_custom int not null, --bool

--if reaction is custom:
reaction_animated int, --bool
reaction_id int,
reaction_name text,

--else
reaction_string text
)", NO_PARAMS).unwrap();
    
    conn.execute("CREATE TABLE IF NOT EXISTS id_arr (
row_id integer primary key
)", NO_PARAMS).unwrap();
    
    conn.execute("CREATE TABLE IF NOT EXISTS id (
id_arr_rowid int not null REFERENCES id_arr(row_id),
id text not null
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS attachment (
rowid integer primary key autoincrement,
message_rowid int not null REFERENCES message(rowid),
discord_id text not null,
filename text not null,
height int,
width int,
proxy_url text not null,
size int not null,
url text not null
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS embed (
rowid integer primary key autoincrement,
message_rowid int not null REFERENCES message(rowid),
author_is_some int not null, --bool
author_icon_url text,
author_name text,
author_proxy_icon_url text,
author_url text,
colour_u32 int not null,
description text,
--fields
footer_is_some int not null, --bool
footer_icon_url text,
footer_proxy_icon_url text,
footer_text text,
image_is_some int not null, --bool
image_height int,
image_width int,
image_proxy_url text,
image_url text,
kind text,
provider_is_some int not null, --bool
provider_name text,
provider_url text,
thumbnail_is_some int not null, --bool
thumbnail_height int,
thumbnail_width int,
thumbnail_url text,
thumbnail_proxy_url text,
timestamp text,
title text,
url text,
video_is_some int not null, --bool
video_height int,
video_width int,
video_url text
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS embed_field (
rowid integer primary key autoincrement,
embed_rowid int not null REFERENCES embed(rowid),
inline int not null, --bool
name text not null,
value text not null
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS ready (
rowid integer primary key autoincrement,
--guilds
--presences
--private_channels
session_id text not null,
shard_0 int,
shard_1 int,
trace text not null,
user_id int not null,
user_avatar text,
user_bot int not null, --bool
user_discriminator int not null,
user_email text,
user_mfa_enabled int not null, --bool
user_name text,
user_verified int not null, --bool
version int not null
)", NO_PARAMS).unwrap();
    
    conn.execute("CREATE TABLE IF NOT EXISTS guild (
rowid integer primary key autoincrement,
ready_rowid int REFERENCES ready(rowid), --possibly null
discord_id int not null,
afk_channel_id int,
afk_timeout int not null,
application_id int,
--channels
default_message_notification_level int not null,
--emojis
explicit_content_filter text not null,
features text not null, --comma separated
icon text,
joined_at text not null, --datetime
large int not null, --bool
member_count int not null,
--members
mfa_level text not null,
name text not null,
owner_id int not null,
--presences
region text not null,
--roles
splash text,
system_channel_id int,
verification_level text not null,
--voice states
archive_recvd_at text not null --datetime
)", NO_PARAMS).unwrap();
    
    conn.execute("CREATE TABLE IF NOT EXISTS guild_channel (
rowid integer primary key autoincrement,
discord_id int not null,
guild_rowid int not null REFERENCES guild(rowid),
guild_id int not null,
bitrate int,
category_id int,
kind text not null,
last_message_id int,
last_pin_timestamp text, --datetime
name text not null,
--permission overwrites
position int not null, --can be negative!
topic text,
user_limit int,
nsfw int not null --bool
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS emoji (
rowid integer primary key autoincrement,
discord_id int not null,
guild_rowid int not null REFERENCES guild(rowid),
animated int not null, --bool
name text not null,
managed int not null, --bool
require_colons int not null, --bool
roles int not null REFERENCES id_arr(row_id)
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS member (
rowid integer primary key autoincrement,
guild_rowid int not null REFERENCES guild(rowid),
deaf int not null, --bool
guild_id int not null,
joined_at text, --datetime
mute int not null,
nick text,
roles int not null REFERENCES id_arr(row_id),
user_id int not null,
user_avatar text,
user_is_bot int not null, --bool
user_discriminator int not null,
user_name text not null
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS user_presence (
rowid integer primary key autoincrement,
ready_rowid int REFERENCES ready(rowid),
guild_rowid int REFERENCES guild(rowid),
game_is_some int not null, --bool
game_type text,
game_name text,
game_url text,
last_modified int, --apparently might be null????
nick text,
status text,
user_id int not null,
CHECK ( (ready_rowid NOT NULL OR guild_rowid NOT NULL) AND (ready_rowid IS NULL OR guild_rowid IS NULL) )
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS voice_state (
rowid integer primary key autoincrement,
guild_rowid int not null REFERENCES guild(rowid),
channel_id int,
deaf int not null, --bool
mute int not null, --bool
self_deaf int not null, --bool
self_mute int not null, --bool
session_id text not null,
suppress int not null, --bool
token text,
user_id int
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS guild_role (
rowid integer primary key autoincrement,
discord_id int not null,
guild_rowid int not null REFERENCES guild(rowid),
colour_u32 int not null,
hoist int not null, --bool
managed int not null, --bool
mentionable int not null, --bool
name text not null,
permissions_bits int not null,
position int not null
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS permission_overwrite (
rowid integer primary key autoincrement,
guild_channel_rowid int not null REFERENCES guild_channel(rowid),
allow_bits int not null,
deny_bits int not null,
permission_overwrite_type text not null,
permission_overwrite_id int not null
)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS message_archive_gets (
rowid integer primary key autoincrement,
channel_id text not null,
after_message_id text,
around_message_id text,
before_message_id text,
start_message_id text not null,
end_message_id text not null,
message_count_requested int not null,
message_count_received int not null,
received_live int not null --bool
)", NO_PARAMS).unwrap();

    conn.execute("CREATE INDEX IF NOT EXISTS mag_ ON message_archive_gets (substr('00000000000000000000'||start_message_id, -20, 20))", NO_PARAMS).unwrap();
    conn.execute("CREATE INDEX IF NOT EXISTS mag_end   ON message_archive_gets (substr('00000000000000000000'||  end_message_id, -20, 20))", NO_PARAMS).unwrap();
    //conn.execute("CREATE INDEX IF NOT EXISTS mag_start_plain ON message_archive_gets(start_message_id)", NO_PARAMS).unwrap();
    //conn.execute("CREATE INDEX IF NOT EXISTS mag_end_plain ON message_archive_gets(end_message_id)", NO_PARAMS).unwrap();

    conn.execute("CREATE TABLE IF NOT EXISTS migration_version (version int)", NO_PARAMS).unwrap();

    let perm_overwrite_exists:bool = conn.query_row("SELECT COUNT(*) FROM pragma_table_info('message') WHERE name='rowid'", NO_PARAMS, |r| r.get(0)).unwrap();

    let mut version:i64 = if perm_overwrite_exists { -1 } else { -2 };

    while version < 5 {
        version = conn.query_row(
            "SELECT version FROM migration_version",
            NO_PARAMS,
            |a| a.get(0)
        ).optional().unwrap().unwrap_or(version);
        println!("DEBUG: version is {}", version);
        let tx = conn.transaction().unwrap();
        match version {
            -2 => {
                tx.execute_batch("
--BEGIN TRANSACTION;

--ALTER TABLE message ADD COLUMN rowid integer primary key autoincrement;
CREATE TEMPORARY TABLE message_backup (
rowid integer primary key autoincrement,
discord_id text not null,
--attachments
author_id text not null,
author_avatar text,
author_is_bot int not null, --bool
author_discriminator int not null,
author_name text not null,
channel_id text not null,
content text not null,
edited_timestamp text, --datetime
--embeds
guild_id text,
kind text,
member_is_some int not null, --bool
member_deaf int, --bool
member_joined_at text, --datetime
member_mute int, --bool
member_roles int, --REFERENCES id_arr(row_id),
--mentions
mention_everyone int not null, --bool
mention_roles int not null, --REFERENCES id_arr(row_id),
nonce_debug text not null, --{:?}
pinned int not null, --bool
--reactions
timestamp text not null,
tts int not null, --bool
webhook_id text,
archive_recvd_at text not null --datetime
);
INSERT INTO message_backup SELECT rowid,* FROM message;
DROP TABLE message;
CREATE TABLE message (
rowid integer primary key autoincrement,
discord_id text not null,
--attachments
author_id text not null,
author_avatar text,
author_is_bot int not null, --bool
author_discriminator int not null,
author_name text not null,
channel_id text not null,
content text not null,
edited_timestamp text, --datetime
--embeds
guild_id text,
kind text,
member_is_some int not null, --bool
member_deaf int, --bool
member_joined_at text, --datetime
member_mute int, --bool
member_roles int REFERENCES id_arr(row_id),
--mentions
mention_everyone int not null, --bool
mention_roles int not null REFERENCES id_arr(row_id),
nonce_debug text not null, --{:?}
pinned int not null, --bool
--reactions
timestamp text not null,
tts int not null, --bool
webhook_id text,
archive_recvd_at text not null --datetime
);
INSERT INTO message SELECT * FROM message_backup;
DROP TABLE message_backup;


--------
--remove columns height_text, width_text, and size_text from attachment
CREATE TEMPORARY TABLE attachment_backup( 
rowid integer primary key autoincrement,
message_rowid int not null, --REFERENCES message(rowid),
discord_id text not null,
filename text not null,
height int,
width int,
proxy_url text not null,
size int not null,
url text not null
);
INSERT INTO attachment_backup SELECT rowid,message_rowid,discord_id,filename,height,width,proxy_url,size,url FROM attachment;
DROP TABLE attachment;
CREATE TABLE attachment( 
  rowid integer primary key autoincrement,
  message_rowid int not null REFERENCES message(rowid),
  discord_id text not null,
  filename text not null,
  height int,
  width int,
  proxy_url text not null,
  size int not null,
  url text not null
);
INSERT INTO attachment SELECT * FROM attachment_backup;
DROP TABLE attachment_backup;

-------
--ALTER TABLE embed_field ADD COLUMN rowid integer primary key autoincrement;

CREATE TABLE embed_field_backup (
rowid integer primary key autoincrement,
embed_rowid int not null REFERENCES embed(rowid),
inline int not null, --bool
name text not null,
value text not null
);
INSERT INTO embed_field_backup SELECT rowid,* FROM embed_field;
DROP TABLE embed_field;
CREATE TABLE embed_field (
rowid integer primary key autoincrement,
embed_rowid int not null REFERENCES embed(rowid),
inline int not null, --bool
name text not null,
value text not null
);
INSERT INTO embed_field SELECT * FROM embed_field_backup;
DROP TABLE embed_field_backup;

-------
--ALTER TABLE message_archive_gets ADD COLUMN rowid integer primary key autoincrement;

CREATE TABLE message_archive_gets_backup (
rowid integer primary key autoincrement,
channel_id text not null,
after_message_id text,
around_message_id text,
before_message_id text,
start_message_id text not null,
end_message_id text not null,
message_count_requested int not null,
message_count_received int not null,
received_live int not null --bool
);
INSERT INTO message_archive_gets_backup SELECT rowid,* FROM message_archive_gets;
DROP TABLE message_archive_gets;
CREATE TABLE message_archive_gets (
rowid integer primary key autoincrement,
channel_id text not null,
after_message_id text,
around_message_id text,
before_message_id text,
start_message_id text not null,
end_message_id text not null,
message_count_requested int not null,
message_count_received int not null,
received_live int not null --bool
);
INSERT INTO message_archive_gets SELECT * FROM message_archive_gets_backup;
DROP TABLE message_archive_gets_backup;

--COMMIT;
").unwrap();
                version = -1
            }
            -1 => {
                tx.execute_batch("INSERT INTO migration_version (version) VALUES (0)").unwrap()
            },
            0 => {
                tx.execute_batch(
                    "ALTER TABLE message ADD COLUMN content_binary data;
UPDATE migration_version SET version = 1;"
                ).expect("Could not migrate 0=>1")
            },
            1 => {
                tx.execute_batch("
CREATE TEMPORARY TABLE IF NOT EXISTS message_archive_gets_backup (
rowid integer primary key autoincrement,
channel_id text not null,
after_message_id text,
around_message_id text,
before_message_id text,
start_message_id text not null,
end_message_id text not null,
message_count_requested int not null,
message_count_received int not null,
received_live int not null --bool
);
INSERT INTO message_archive_gets_backup SELECT * FROM message_archive_gets;
DROP TABLE message_archive_gets;
CREATE TABLE message_archive_gets (
rowid integer primary key autoincrement,
channel_id int not null,
after_message_id int,
around_message_id int,
before_message_id int,
start_message_id int not null,
end_message_id int not null,
message_count_requested int not null,
message_count_received int not null
);
INSERT INTO message_archive_gets SELECT rowid,CAST(channel_id as int),CAST(after_message_id as int),CAST(around_message_id as int),CAST(before_message_id as int),CAST(start_message_id as int),CAST(end_message_id as int),message_count_requested,message_count_received FROM message_archive_gets_backup;
DROP TABLE message_archive_gets_backup;
CREATE INDEX mag_start  ON message_archive_gets( start_message_id);
CREATE INDEX mag_end    ON message_archive_gets(   end_message_id);
CREATE INDEX mag_after  ON message_archive_gets( after_message_id);
CREATE INDEX mag_before ON message_archive_gets(before_message_id);
UPDATE migration_version SET version = 2;
").expect("Could not execute migration 1=>2")
            },
            2 => {
                tx.execute_batch("
CREATE TABLE group_channel (
rowid integer primary key autoincrement,
discord_id int not null,
ready_rowid int REFERENCES ready(rowid),
icon text,
last_message_id int,
last_pin_timestamp text, --datetime
name text,
owner_id int not null
);
CREATE TABLE group_user (
rowid integer primary key autoincrement,
discord_id int not null,
group_channel_rowid int not null REFERENCES group_channel(rowid),
avatar text,
bot int not null, --bool
discriminator int not null,
name text not null
);
CREATE TABLE private_channel (
rowid integer primary key autoincrement,
discord_id int not null,
ready_rowid int REFERENCES ready(rowid),
last_message_id int,
last_pin_timestamp text, --datetime
kind text not null,
recipient_id int not null,
recipient_avatar text,
recipient_bot int not null, --bool
recipient_discriminator int not null,
recipient_name text not null
);

UPDATE migration_version SET version = 3;
").expect("Failed migration 2=>3")
            },
            3 => {
                tx.execute_batch("
CREATE INDEX mag_channel_start  ON message_archive_gets(channel_id, start_message_id);
CREATE INDEX mag_channel_end    ON message_archive_gets(channel_id,   end_message_id);
CREATE INDEX mag_channel_after  ON message_archive_gets(channel_id, after_message_id);
CREATE INDEX mag_channel_before ON message_archive_gets(channel_id,before_message_id);
UPDATE migration_version SET version = 4;
").expect("Failed migration 3=>4");
            },
            4 => {
                tx.execute_batch("
UPDATE message SET 
author_avatar = REPLACE(author_avatar,'$','$$'),
author_name = REPLACE(author_name,'$','$$');

UPDATE attachment SET
filename = REPLACE(filename,'$','$$'),
url = REPLACE(url,'$','$$');

UPDATE embed SET
author_icon_url = REPLACE(author_icon_url,'$','$$'),
author_name = REPLACE(author_name,'$','$$'),
author_proxy_icon_url = REPLACE(author_proxy_icon_url,'$','$$'),
author_url = REPLACE(author_url,'$','$$'),
description = REPLACE(description,'$','$$'),
footer_icon_url = REPLACE(footer_icon_url,'$','$$'),
footer_proxy_icon_url = REPLACE(footer_proxy_icon_url,'$','$$'),
footer_text = REPLACE(footer_text,'$','$$'),
image_proxy_url = REPLACE(image_proxy_url,'$','$$'),
image_url = REPLACE(image_url,'$','$$'),
provider_name = REPLACE(provider_name,'$','$$'),
provider_url = REPLACE(provider_url,'$','$$'),
thumbnail_url = REPLACE(thumbnail_url,'$','$$'),
thumbnail_proxy_url = REPLACE(thumbnail_proxy_url,'$','$$'),
title = REPLACE(title,'$','$$'),
url = REPLACE(url,'$','$$'),
video_url = REPLACE(video_url,'$','$$');

UPDATE embed_field SET
name = REPLACE(name,'$','$$'),
value = REPLACE(value,'$','$$');

UPDATE user_mention SET
avatar = REPLACE(avatar,'$','$$'),
name = REPLACE(name,'$','$$');

UPDATE reaction SET
reaction_name = REPLACE(reaction_name,'$','$$'),
reaction_string = REPLACE(reaction_string,'$','$$');

UPDATE ready SET
trace = REPLACE(trace,'$','$$'),
user_avatar = REPLACE(user_avatar,'$','$$'),
user_email = REPLACE(user_email,'$','$$'),
user_name = REPLACE(user_name,'$','$$');

UPDATE user_presence SET
game_name = REPLACE(game_name,'$','$$'),
game_url = REPLACE(game_url,'$','$$'),
nick = REPLACE(nick,'$','$$');

UPDATE group_channel SET
icon = REPLACE(icon,'$','$$'),
name = REPLACE(name,'$','$$');

UPDATE group_user SET
avatar = REPLACE(avatar,'$','$$'),
name = REPLACE(name,'$','$$');

UPDATE private_channel SET
recipient_avatar = REPLACE(recipient_avatar,'$','$$'),
recipient_name = REPLACE(recipient_name,'$','$$');

UPDATE guild SET
features = REPLACE(features,'$','$$'),
icon = REPLACE(icon,'$','$$'),
name = REPLACE(name,'$','$$'),
region = REPLACE(region,'$','$$'),
splash = REPLACE(splash,'$','$$');

UPDATE guild_channel SET
name = REPLACE(name,'$','$$'),
topic = REPLACE(topic,'$','$$');

--permission_overwrite doesnt need any

UPDATE emoji SET
name = REPLACE(name,'$','$$');

UPDATE member SET
nick = REPLACE(nick,'$','$$'),
user_avatar = REPLACE(user_avatar,'$','$$'),
user_name = REPLACE(user_name,'$','$$');

UPDATE user_presence SET
game_name = REPLACE(game_name,'$','$$'),
game_url = REPLACE(game_url,'$','$$'),
nick = REPLACE(nick,'$','$$');

UPDATE guild_role SET
name = REPLACE(name,'$','$$');

UPDATE voice_state SET
token = REPLACE(token,'$','$$');


UPDATE migration_version SET version = 5;
").expect("Failed migration 4=>5");
            },
            5 => {
                /*tx.execute_batch("
CREATE TABLE channel_create_event (
rowid integer primary key autoincrement,
recvd_at text not null --datetime
);

CREATE TABLE guild_channel_bak (
rowid integer primary key autoincrement,
discord_id int not null,
guild_rowid int REFERENCES guild(rowid), --possibly null
guild_id int not null,
bitrate int,
category_id int,
kind text not null,
last_message_id int,
last_pin_timestamp text, --datetime
name text not null,
--permission overwrites
position int not null, --can be negative!
topic text,
user_limit int,
nsfw int not null --bool
);
INSERT INTO guild_channel_bak SELECT * FROM guild_channel;
DROP TABLE guild_channel;
CREATE TABLE guild_channel (
rowid integer primary key autoincrement,
discord_id int not null,
guild_rowid int REFERENCES guild(rowid), --possibly null
channel_create_event_rowid int UNIQUE REFERENCES channel_create_event(rowid), --possibly null
guild_id int not null,
bitrate int,
category_id int,
kind text not null,
last_message_id int,
last_pin_timestamp text, --datetime
name text not null,
--permission overwrites
position int not null, --can be negative!
topic text,
user_limit int,
nsfw int not null --bool
);
INSERT INTO guild_channel SELECT rowid, discord_id, guild_rowid, NULL, guild_id, bitrate, category_id, kind, last_message_id, last_pin_timestamp, name, position, topic, user_limit, nsfw FROM guild_channel_bak;


UPDATE migration_version SET version = 6;
").unwrap();*/
            },
            //6 => {},
            _ => panic!("unrecognized migration version"),
        }
        tx.commit().unwrap();
    }
}
