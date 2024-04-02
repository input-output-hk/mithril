//! Migration module for cardano transactions store
//!
use mithril_persistence::database::SqlMigration;

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

create unique index cardano_tx_immutable_file_number_index on cardano_tx(immutable_file_number);
"#,
        ),
        // Migration 2
        // Fix the `cardano_tx` table index on immutable_file number, incorrectly marked as unique.
        SqlMigration::new(
            2,
            r#"
-- remove all data from the cardano tx table since a lot of transactions where missing for each
-- block and we rely on their insert order.
delete from cardano_tx;

drop index cardano_tx_immutable_file_number_index;
create index cardano_tx_immutable_file_number_index on cardano_tx(immutable_file_number);

vacuum;
"#,
        ),
        // Migration 3
        // Add `slot_number` and `block_hash` columns to `cardano_tx`.
        SqlMigration::new(
            3,
            r#"
-- remove all data from the cardano tx table since the new columns are mandatory
delete from cardano_tx;

alter table cardano_tx add column slot_number integer not null;
alter table cardano_tx add column block_hash text not null;

vacuum;
        "#,
        ),
    ]
}
