//! Migration module for cardano transactions store
//!
use mithril_common::database::SqlMigration;

/// Get all the migrations required by this version of the software.
/// There shall be one migration per database version. There could be several
/// statements per migration.
pub fn get_migrations() -> Vec<SqlMigration> {
    vec![
        // Migration 1
        // Add the `cardano_tx` table.
        SqlMigration::new(
            1,
            r#"
create table cardano_tx (
    transaction_hash        text      not null,
    block_number            integer   not null,
    immutable_file_number   integer   not null,
    primary key (transaction_hash)
);

create unique index cardano_transactions_unique_index on cardano_tx(immutable_file_number);
"#,
        ),
    ]
}
