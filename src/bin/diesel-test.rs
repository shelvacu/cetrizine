#![recursion_limit="1024"]
#![feature(trace_macros)]
#![allow(unused_imports,non_camel_case_types)]
#[macro_use]
extern crate diesel;
extern crate serenity;
extern crate chrono;

//trace_macros!(true);
mod db_types {
    include!{"../db_types.rs"}
}
trace_macros!(false);
mod schema {
    include!{"../schema.rs"}
}

#[derive(Queryable,Debug)]
pub struct Member {
    pub rowid:i64,
    pub guild_rowid:i64,
    pub guild_id:Snowflake,
    pub deaf:bool,
    pub joined_at:Option<chrono::DateTime<chrono::Utc>>,
    pub mute:bool,
    pub nick:Option<String>,
    pub roles:Vec<Snowflake>,
    pub user_info:db_types::DbDiscordUser,
}

use schema::member;
use db_types::Snowflake;

#[derive(Debug,Insertable)]
#[table_name="member"]
pub struct NewMember {
    pub guild_rowid:i64,
    pub guild_id:Snowflake,
    pub deaf:bool,
    pub joined_at:Option<chrono::DateTime<chrono::Utc>>,
    pub mute:bool,
    pub nick:Option<String>,
    pub roles:Vec<Snowflake>,
    pub user_info:db_types::DbDiscordUser,
}    

use diesel::pg::PgConnection;
use diesel::prelude::*;

fn main() {
    use schema::member::dsl::*;
    let conn = PgConnection::establish("postgres://shelvacu@%2Fvar%2Frun%2Fpostgresql:5434/diesel_test").unwrap();
    dbg!(member.limit(5).load::<Member>(&conn).unwrap());
    let new_member = NewMember{
        guild_rowid: 1,
        guild_id: Snowflake(537167820539166720),
        deaf: false,
        joined_at: None,
        mute: false,
        nick: None,
        roles: vec![Snowflake(5),Snowflake(6),Snowflake(7),],
        user_info: db_types::DbDiscordUser{
            discord_id: Snowflake(123),
            avatar: Some("abcxyz".to_string()),
            is_bot: true,
            discriminator: 1337,
            name: String::from("Bob bobby McBobberton"),
        }
    };

    /*let mem:Member = */diesel::insert_into(schema::member::table)
        .values(&new_member)
        .execute(&conn).unwrap();
    //dbg!(mem);
}
