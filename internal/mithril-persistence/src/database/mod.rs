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
pub mod test_helper {
    use sqlite::ConnectionThreadSafe;

    use mithril_common::StdResult;

    use crate::sqlite::{ConnectionBuilder, ConnectionOptions};

    /// In-memory sqlite database without foreign key support with cardano db migrations applied
    pub fn cardano_tx_db_connection() -> StdResult<ConnectionThreadSafe> {
        let connection = ConnectionBuilder::open_memory()
            .with_options(&[ConnectionOptions::ForceDisableForeignKeys])
            .with_migrations(crate::database::cardano_transaction_migration::get_migrations())
            .build()?;
        Ok(connection)
    }
}
