use anyhow::Context;
use sqlite::Value;

use mithril_common::entities::BlockNumber;
use mithril_common::StdResult;

use crate::database::record::CardanoTransactionRecord;
use crate::sqlite::{
    EntityCursor, Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

/// Query to delete old [CardanoTransactionRecord] from the sqlite database
pub struct DeleteCardanoTransactionProvider<'conn> {
    connection: &'conn SqliteConnection,
}

impl<'conn> Provider<'conn> for DeleteCardanoTransactionProvider<'conn> {
    type Entity = CardanoTransactionRecord;

    fn get_connection(&'conn self) -> &'conn SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:cardano_tx:}", "cardano_tx")]));

        format!("delete from cardano_tx where {condition} returning {projection}")
    }
}

impl<'conn> DeleteCardanoTransactionProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn SqliteConnection) -> Self {
        Self { connection }
    }

    fn get_prune_condition(
        &self,
        block_number_threshold: BlockNumber,
    ) -> StdResult<WhereCondition> {
        let threshold = Value::Integer(block_number_threshold.try_into().with_context(|| {
            format!("Failed to convert threshold `{block_number_threshold}` to i64")
        })?);

        Ok(WhereCondition::new("block_number < ?*", vec![threshold]))
    }

    /// Prune the cardano transaction data below the given threshold.
    pub fn prune(
        &self,
        block_number_threshold: BlockNumber,
    ) -> StdResult<EntityCursor<CardanoTransactionRecord>> {
        let filters = self.get_prune_condition(block_number_threshold)?;

        self.find(filters)
    }
}

#[cfg(test)]
mod tests {
    use crate::database::provider::{
        GetCardanoTransactionProvider, InsertCardanoTransactionProvider,
    };
    use crate::database::test_helper::cardano_tx_db_connection;
    use crate::sqlite::GetAllProvider;

    use super::*;

    fn insert_transactions(connection: &SqliteConnection, records: Vec<CardanoTransactionRecord>) {
        let provider = InsertCardanoTransactionProvider::new(connection);
        let condition = provider.get_insert_many_condition(records).unwrap();
        let mut cursor = provider.find(condition).unwrap();
        cursor.next().unwrap();
    }

    fn test_transaction_set() -> Vec<CardanoTransactionRecord> {
        vec![
            CardanoTransactionRecord::new("tx-hash-0", 10, 50, "block-hash-10", 1),
            CardanoTransactionRecord::new("tx-hash-1", 10, 51, "block-hash-10", 1),
            CardanoTransactionRecord::new("tx-hash-2", 11, 52, "block-hash-11", 1),
            CardanoTransactionRecord::new("tx-hash-3", 11, 53, "block-hash-11", 1),
            CardanoTransactionRecord::new("tx-hash-4", 12, 54, "block-hash-12", 1),
            CardanoTransactionRecord::new("tx-hash-5", 12, 55, "block-hash-12", 1),
        ]
    }

    #[test]
    fn test_prune_work_even_without_transactions_in_db() {
        let connection = cardano_tx_db_connection().unwrap();

        let prune_provider = DeleteCardanoTransactionProvider::new(&connection);
        let cursor = prune_provider
            .prune(100)
            .expect("pruning shouldn't crash without transactions stored");
        assert_eq!(0, cursor.count());
    }

    #[test]
    fn test_prune_all_data_if_given_block_number_is_larger_than_stored_number_of_block() {
        let connection = cardano_tx_db_connection().unwrap();
        insert_transactions(&connection, test_transaction_set());

        let prune_provider = DeleteCardanoTransactionProvider::new(&connection);
        let cursor = prune_provider.prune(100_000).unwrap();
        assert_eq!(test_transaction_set().len(), cursor.count());

        let get_provider = GetCardanoTransactionProvider::new(&connection);
        let cursor = get_provider.get_all().unwrap();
        assert_eq!(0, cursor.count());
    }

    #[test]
    fn test_prune_keep_all_tx_of_last_block_if_given_number_of_block_is_zero() {
        let connection = cardano_tx_db_connection().unwrap();
        insert_transactions(&connection, test_transaction_set());

        let prune_provider = DeleteCardanoTransactionProvider::new(&connection);
        let cursor = prune_provider.prune(0).unwrap();
        assert_eq!(0, cursor.count());

        let get_provider = GetCardanoTransactionProvider::new(&connection);
        let cursor = get_provider.get_all().unwrap();
        assert_eq!(test_transaction_set().len(), cursor.count());
    }

    #[test]
    fn test_prune_data_of_below_given_blocks() {
        let connection = cardano_tx_db_connection().unwrap();
        insert_transactions(&connection, test_transaction_set());

        let prune_provider = DeleteCardanoTransactionProvider::new(&connection);
        let cursor = prune_provider.prune(12).unwrap();
        assert_eq!(4, cursor.count());

        let get_provider = GetCardanoTransactionProvider::new(&connection);
        let cursor = get_provider.get_all().unwrap();
        assert_eq!(2, cursor.count());
    }
}
