use super::pg;

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
];

pub const CURRENT_MIGRATION_VERSION:usize = MIGRATIONS.len();

pub const DB_INIT_SQL:&str = include_str!("../postgres_init.sql");

pub fn get_migration_version<C: pg::GenericConnection>(conn: &C) -> i64 {
    conn.query("SELECT version FROM migration_version LIMIT 1", &[]).unwrap().get(0).get(0)
}

pub fn migration_is_current<C: pg::GenericConnection>(conn: &C) -> bool {
    get_migration_version(conn) == (CURRENT_MIGRATION_VERSION as i64)
}
    

pub fn do_postgres_migrate<C: pg::GenericConnection>(conn: &C) {
    loop {
        let mv = get_migration_version(conn) as usize;
        if mv > CURRENT_MIGRATION_VERSION {
            panic!("Database is newer than {} binary! Max expected migration version: {} Found migration version: {}",env!("CARGO_PKG_NAME"), CURRENT_MIGRATION_VERSION, mv);
        }
        if mv == CURRENT_MIGRATION_VERSION { break; }
        match MIGRATIONS[mv] {
            Invalid => panic!(),
            Normal(sql_str) => {
                info!("Running migration v{} => v{}", mv, mv+1);
                let tx = conn.transaction().unwrap();
                tx.batch_execute(sql_str).unwrap();
                let updated = tx.execute(
                    "UPDATE migration_version SET version = $1 WHERE version = $2",
                    &[
                        &((mv+1) as i64),
                        &(mv as i64)
                    ]
                ).unwrap();
                if updated != 1 {
                    panic!("Failed to set migration_version");
                }
                tx.commit().unwrap();
                info!("Finished migration v{} => v{}", mv, mv+1);
            }
        }
    }       
}
