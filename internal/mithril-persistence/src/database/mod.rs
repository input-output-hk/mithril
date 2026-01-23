//! database module.
//! This module contains queries and entities shared between all application types.

pub mod cardano_transaction_migration;
mod db_version;
mod hydrator;
pub(crate) mod query;
pub mod record;
pub mod repository;
mod version_checker;

pub use db_version::*;
pub use hydrator::Hydrator;
pub use version_checker::{DatabaseVersionChecker, SqlMigration};

/// Database version.
pub type DbVersion = i64;

#[cfg(test)]
pub(crate) mod test_helper {
    use std::path::Path;

    use mithril_common::StdResult;
    use mithril_common::entities::CardanoBlockWithTransactions;

    use crate::database::query::{InsertCardanoBlockQuery, InsertCardanoTransactionQuery};
    use crate::database::record::IntoRecords;
    use crate::sqlite::{
        ConnectionBuilder, ConnectionExtensions, ConnectionOptions, SqliteConnection,
    };

    /// In-memory sqlite database without foreign key support with cardano db migrations applied
    pub fn cardano_tx_db_connection() -> StdResult<SqliteConnection> {
        let connection = ConnectionBuilder::open_memory()
            .with_options(&[ConnectionOptions::ForceDisableForeignKeys])
            .with_migrations(crate::database::cardano_transaction_migration::get_migrations())
            .build()?;
        Ok(connection)
    }

    /// In-memory sqlite database without foreign key support with cardano db migrations applied
    #[allow(unused)] // Useful for debugging
    pub fn cardano_tx_db_connection_file(dir_path: &Path) -> StdResult<SqliteConnection> {
        let connection = ConnectionBuilder::open_file(&dir_path.join("cardano_tx.db"))
            .with_options(&[ConnectionOptions::ForceDisableForeignKeys])
            .with_migrations(crate::database::cardano_transaction_migration::get_migrations())
            .build()?;
        Ok(connection)
    }

    #[cfg(test)]
    pub(crate) fn insert_cardano_blocks_and_transaction(
        connection: &SqliteConnection,
        blocks_with_txs: Vec<CardanoBlockWithTransactions>,
    ) {
        let (blocks_records, transactions_records) = blocks_with_txs.into_records();

        connection
            .apply(InsertCardanoBlockQuery::insert_many(blocks_records).unwrap())
            .unwrap();
        connection
            .apply(InsertCardanoTransactionQuery::insert_many(transactions_records).unwrap())
            .unwrap();
    }
}
