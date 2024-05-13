use std::ops::Range;

use sqlite::Value;

use mithril_common::entities::{BlockNumber, BlockRange, TransactionHash};
#[cfg(test)]
use mithril_persistence::sqlite::GetAllCondition;
use mithril_persistence::sqlite::{
    Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::CardanoTransactionRecord;

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

#[cfg(test)]
impl GetAllCondition for GetCardanoTransactionProvider<'_> {}
