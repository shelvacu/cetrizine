use crate::diesel::prelude::*;
use crate::diesel::sql_types::BigInt;

use std::convert::TryInto;

enum MigrationSpec {
    Invalid,
    Normal(&'static str),
}

use MigrationSpec::*;

const MIGRATIONS:&[MigrationSpec] = &[
    Invalid, //0
    Invalid, //1
    Invalid, //2
    Invalid, //3
    Invalid, //4
    Invalid, //5
    Invalid, //6
    Normal(include_str!("migrations/7to8.sql")), //7
    Normal(include_str!("migrations/8to9.sql")), //8
    Normal(include_str!("migrations/9to10.sql")), //9
    Normal(include_str!("migrations/10to11.sql")), //10
    Normal(include_str!("migrations/11to12-previous-message.sql")), //11
    Normal(include_str!("migrations/12to13-migration-version-pkey.sql")), //12
    Normal(include_str!("migrations/13to14-sqlite-migration-progress-pkey.sql")), //13
    Normal(include_str!("migrations/14to15-attachment-download.sql")), //14
    Normal(include_str!("migrations/15to16-download-headers.sql")), //15
    Normal(include_str!("migrations/16to17-raw-message-downloads.sql")), //16
    Normal(include_str!("migrations/17to18-fix-fuckup.sql")), //17
    Normal(include_str!("migrations/18to19-rock-paper-scissors.sql")), //18
    Normal(include_str!("migrations/19to20-rps-rematch.sql")), //19
    //SELECT MIN(a.rowid) as new_rowid, b.rowid as old_rowid, MAX(a.sha256sum_hex) FROM download_data as a, download_data as b WHERE a.sha256sum_hex = b.sha256sum_hex AND a.rowid < b.rowid GROUP BY b.rowid;
];

pub const CURRENT_MIGRATION_VERSION:usize = MIGRATIONS.len();

pub const DB_INIT_SQL:&str = include_str!("../postgres_init.sql");

pub fn get_migration_version(conn: &diesel::pg::PgConnection) -> i64 {
    use crate::schema::migration_version::dsl;

    dsl::migration_version.select(dsl::version).limit(1).get_result(conn).unwrap():i64
    //conn.query("SELECT version FROM migration_version LIMIT 1", &[]).unwrap().get(0).get(0)
}

pub fn migration_is_current(conn: &diesel::pg::PgConnection) -> bool {
    get_migration_version(conn) == (CURRENT_MIGRATION_VERSION as i64)
}
    

pub fn do_postgres_migrate(conn: &diesel::pg::PgConnection) {
    use crate::schema::migration_version::dsl::*;
    loop {
        let mv = get_migration_version(conn).try_into().unwrap():usize;
        if mv > CURRENT_MIGRATION_VERSION {
            panic!("Database is newer than {} binary! Max expected migration version: {} Found migration version: {}",env!("CARGO_PKG_NAME"), CURRENT_MIGRATION_VERSION, mv);
        }
        if mv == CURRENT_MIGRATION_VERSION { break; }
        match MIGRATIONS[mv] {
            Invalid => panic!(),
            Normal(sql_str) => {
                info!("Running migration v{} => v{}", mv, mv+1);
                conn.transaction(|| {
                    use crate::diesel::connection::SimpleConnection;
                    conn.batch_execute(sql_str).unwrap();
                    if mv == 15 {
                        #[derive(QueryableByName,Debug,Clone,Copy)]
                        struct NewOld {
                            #[sql_type = "BigInt"]
                            new_rowid: i64,
                            #[sql_type = "BigInt"]
                            old_rowid: i64,
                        }
                        let results:Vec<NewOld> = diesel::sql_query("SELECT MIN(a.rowid) as new_rowid, b.rowid as old_rowid FROM download_data as a, download_data as b WHERE a.sha256sum_hex = b.sha256sum_hex AND a.rowid < b.rowid GROUP BY b.rowid").load(conn).unwrap();
                        info!("Found {} download_data duplicates", results.len());
                        for res in results {
                            diesel::sql_query("UPDATE download SET download_data_rowid = $1 WHERE download_data_rowid = $2")
                                .bind::<BigInt,_>(res.new_rowid)
                                .bind::<BigInt,_>(res.old_rowid)
                                .execute(conn).unwrap();
                            diesel::sql_query("DELETE FROM download_data WHERE rowid = $1")
                                .bind::<BigInt,_>(res.old_rowid)
                                .execute(conn).unwrap();
                        }
                    }
                    let updated =
                        diesel::update(
                            migration_version.filter(version.eq(mv.try_into().unwrap():i64))
                        )
                        .set(version.eq((mv+1).try_into().unwrap():i64))
                        .execute(conn).unwrap();
                    if updated != 1 {
                        panic!("Failed to set migration_version");
                    }
                    Ok(()):Result<(),crate::diesel::result::Error>
                }).unwrap();
                info!("Finished migration v{} => v{}", mv, mv+1);
            }
        }
    }       
}
