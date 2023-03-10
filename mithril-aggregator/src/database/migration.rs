//! Migration module
//!
use mithril_common::database::SqlMigration;

/// Get all the migrations required by this version of the software.
/// There shall be one migration per database version. There could be several
/// statements per migration.
pub fn get_migrations() -> Vec<SqlMigration> {
    vec![SqlMigration::new(
        1,
        r#"
create table stake_pool (
    stake_pool_id text      not null,
    epoch         integer   not null,
    stake         integer    not null,
    created_at    text      not null default current_timestamp,
    primary key (epoch, stake_pool_id)
)"#,
    )]
}
