use std::ops::Range;

use sqlite::Value;

use mithril_common::entities::{BlockNumber, BlockRange, TransactionHash};

use crate::database::record::CardanoTransactionRecord;
use crate::sqlite::{
    GetAllCondition, Provider, ProviderV2, SourceAlias, SqLiteEntity, SqliteConnection,
    WhereCondition,
};

/// Simple queries to retrieve [CardanoTransaction] from the sqlite database.
pub struct GetCardanoTransactionProvider<'client> {
    connection: &'client SqliteConnection,
}

impl<'client> GetCardanoTransactionProvider<'client> {
    /// Create a new instance
    pub fn new(connection: &'client SqliteConnection) -> Self {
        Self { connection }
    }

    // Useful in test and probably in the future.
    pub fn get_transaction_hash_condition(
        &self,
        transaction_hash: &TransactionHash,
    ) -> WhereCondition {
        WhereCondition::new(
            "transaction_hash = ?*",
            vec![Value::String(transaction_hash.to_owned())],
        )
    }

    pub fn get_transaction_hashes_condition(
        &self,
        transactions_hashes: Vec<TransactionHash>,
    ) -> WhereCondition {
        let hashes_values = transactions_hashes.into_iter().map(Value::String).collect();

        WhereCondition::where_in("transaction_hash", hashes_values)
    }

    pub fn get_transaction_block_ranges_condition(
        &self,
        block_ranges: Vec<BlockRange>,
    ) -> WhereCondition {
        let mut where_condition = WhereCondition::default();
        for block_range in block_ranges {
            where_condition = where_condition.or_where(WhereCondition::new(
                "(block_number >= ?* and block_number < ?*)",
                vec![
                    Value::Integer(block_range.start as i64),
                    Value::Integer(block_range.end as i64),
                ],
            ))
        }

        where_condition
    }

    pub fn get_transaction_between_blocks_condition(
        &self,
        range: Range<BlockNumber>,
    ) -> WhereCondition {
        WhereCondition::new(
            "block_number >= ?*",
            vec![Value::Integer(range.start as i64)],
        )
        .and_where(WhereCondition::new(
            "block_number < ?*",
            vec![Value::Integer(range.end as i64)],
        ))
    }
}

impl<'client> Provider<'client> for GetCardanoTransactionProvider<'client> {
    type Entity = CardanoTransactionRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:cardano_tx:}", "cardano_tx")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("select {projection} from cardano_tx where {condition} order by block_number, transaction_hash")
    }
}

impl GetAllCondition for GetCardanoTransactionProvider<'_> {}

/// Simple queries to retrieve [CardanoTransaction] from the sqlite database.
pub struct GetCardanoTransactionProviderV2 {
    filters: WhereCondition,
}

impl GetCardanoTransactionProviderV2 {
    pub fn by_hash(transaction_hash: &TransactionHash) -> Self {
        Self {
            filters: WhereCondition::new(
                "transaction_hash = ?*",
                vec![Value::String(transaction_hash.to_owned())],
            ),
        }
    }
}

impl ProviderV2 for GetCardanoTransactionProviderV2 {
    type Entity = CardanoTransactionRecord;

    fn filters(&self) -> WhereCondition {
        self.filters.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:cardano_tx:}", "cardano_tx")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("select {projection} from cardano_tx where {condition} order by block_number, transaction_hash")
    }
}

#[cfg(test)]
mod tests {
    use crate::database::provider::InsertCardanoTransactionProvider;
    use crate::database::test_helper::cardano_tx_db_connection;
    use crate::sqlite::ConnectionExtensions;

    use super::*;

    fn insert_transactions(connection: &SqliteConnection, records: Vec<CardanoTransactionRecord>) {
        let provider = InsertCardanoTransactionProvider::new(connection);
        let condition = provider.get_insert_many_condition(records).unwrap();
        let mut cursor = provider.find(condition).unwrap();
        cursor.next().unwrap();
    }

    #[test]
    fn test_get_v2() {
        let connection = cardano_tx_db_connection().unwrap();
        let in_db = vec![
            CardanoTransactionRecord::new("tx-hash-0", 10, 50, "block-hash-10", 1),
            CardanoTransactionRecord::new("tx-hash-1", 10, 51, "block-hash-10", 1),
        ];

        insert_transactions(&connection, in_db.clone());

        let cursor = connection
            .fetch(GetCardanoTransactionProviderV2::by_hash(
                &"tx-hash-0".to_string(),
            ))
            .unwrap();
        let transactions: Vec<CardanoTransactionRecord> = cursor.collect();

        assert_eq!(in_db[0..1].to_vec(), transactions);
    }
}
