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
];

pub const CURRENT_MIGRATION_VERSION:usize = MIGRATIONS.len();

pub const DB_INIT_SQL:&str = include_str!("../postgres_init.sql");

#[derive(Debug,Queryable)]
pub struct MigrationVersion {
    version: i64
}

pub fn get_migration_version(conn: &diesel::pg::PgConnection) -> i64 {
    use crate::schema::migration_version::dsl;
    dsl::migration_version.limit(1).load::<MigrationVersion>(conn).unwrap()[0].version
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
                    conn.batch_execute(sql_str).unwrap();
                    let updated =
                        diesel::update(
                            migration_version.filter(version.eq(mv.try_into().unwrap():i64))
                        )
                        .set(version.eq((mv+1).try_into().unwrap():i64))
                        .execute().unwrap();
                    if updated != 1 {
                        panic!("Failed to set migration_version");
                    }
                });
                info!("Finished migration v{} => v{}", mv, mv+1);
            }
        }
    }       
}
