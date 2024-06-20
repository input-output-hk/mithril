//! Migration module for cardano transactions store
//!
use crate::database::SqlMigration;

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
        // Migration 4
        // Add index on `block_number` column of `cardano_tx` table
        SqlMigration::new(
            4,
            r#"
create index block_number_index on cardano_tx(block_number);
"#,
        ),
        // Migration 5
        // Add `block_range_root` table
        SqlMigration::new(
            5,
            r#"
create table block_range_root (
    start         integer   not null,
    end           integer   not null,
    merkle_root   text      not null,
    primary key (start, end)
);
"#,
        ),
        // Migration 6
        // Add composite index on `block_number/transaction_hash` column of `cardano_tx` table
        // Truncate `block_range_root` table after changing the order of retrieval of the transactions
        SqlMigration::new(
            6,
            r#"
create index block_number_transaction_hash_index on cardano_tx(block_number, transaction_hash);

-- remove all data from the block_range_root table since the order used to create them has changed
delete from block_range_root;

vacuum;
"#,
        ),
        // Migration 7
        // Enable full `auto_vacuum` on the database to prevent the database from growing
        // indefinitely since data is often deleted with chain rollbacks or, only on signers,
        // transactions pruning.
        SqlMigration::new(
            7,
            r#"
-- 'pragma auto_vacuum = full' can't be applied to an existing database, so we need to recreate
-- the database by using 'vacuum'.
pragma auto_vacuum = full;
vacuum;
"#,
        ),
    ]
}
