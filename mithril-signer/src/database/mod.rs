//! database module.
//! This module contains the entities definition tied with database
//! representation with their associated providers.
pub mod cardano_transaction_migration;
pub mod migration;
pub(crate) mod provider;
pub mod record;
pub mod repository;

#[cfg(test)]
pub mod test_utils {
    use sqlite::ConnectionThreadSafe;
    use std::sync::Arc;

    use mithril_common::StdResult;
    use mithril_persistence::sqlite::{ConnectionBuilder, ConnectionOptions};

    use super::*;

    pub fn cardano_tx_connection() -> StdResult<Arc<ConnectionThreadSafe>> {
        let connection = ConnectionBuilder::open_memory()
            .with_options(&[ConnectionOptions::ForceDisableForeignKeys])
            .with_migrations(cardano_transaction_migration::get_migrations())
            .build()?;
        Ok(Arc::new(connection))
    }
}
