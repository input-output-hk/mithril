//! database module.
//! This module contains the entities definition tied with database
//! representation with their associated providers.
pub mod cardano_transaction_migration;
pub mod migration;
pub mod provider;

#[cfg(test)]
pub mod test_utils {
    use mithril_common::StdResult;
    use mithril_persistence::sqlite::SqliteConnection;

    use super::*;

    pub fn apply_all_transactions_db_migrations(connection: &SqliteConnection) -> StdResult<()> {
        for migration in cardano_transaction_migration::get_migrations() {
            connection.execute(&migration.alterations)?;
        }

        Ok(())
    }
}
