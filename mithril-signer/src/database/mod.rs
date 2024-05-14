//! database module.
//! This module contains the entities definition tied with database
//! representation with their associated providers.
pub mod migration;
pub mod repository;

#[cfg(test)]
pub mod test_utils {
    use sqlite::ConnectionThreadSafe;

    use mithril_common::StdResult;
    use mithril_persistence::database::cardano_transaction_migration;
    use mithril_persistence::sqlite::{ConnectionBuilder, ConnectionOptions};

    pub fn cardano_tx_db_connection() -> StdResult<ConnectionThreadSafe> {
        let connection = ConnectionBuilder::open_memory()
            .with_options(&[ConnectionOptions::ForceDisableForeignKeys])
            .with_migrations(cardano_transaction_migration::get_migrations())
            .build()?;
        Ok(connection)
    }
}
