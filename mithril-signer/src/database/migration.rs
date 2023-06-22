//! Migration module
//!
use mithril_common::database::SqlMigration;

/// Get all the migrations required by this version of the software.
/// There shall be one migration per database version. There could be several
/// statements per migration.
pub fn get_migrations() -> Vec<SqlMigration> {
    vec![
        // Migration 1
        // Alter `db_version` tables to remove `default current_timestamp` clause from its
        // `updated_at` field, and migrate old date data to rfc 3339.
        SqlMigration::new(
            1,
            r"
-- In some context, most likely tests, the db_version isn't created since the migrator isn't used
create table if not exists 'db_version' (application_type text not null primary key, version integer not null, updated_at text not null);

create table new_db_version (application_type text not null primary key, version integer not null, updated_at text not null);
insert into new_db_version select * from db_version order by rowid asc;

drop table db_version;
alter table new_db_version rename to db_version;
            ",
        ),
    ]
}
