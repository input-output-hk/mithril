use sqlite::Value;
use std::ops::Range;

use mithril_common::entities::{BlockNumber, ImmutableFileNumber, TransactionHash};
use mithril_persistence::sqlite::{
    Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::CardanoTransactionRecord;

#[cfg(test)]
use mithril_persistence::sqlite::GetAllCondition;

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

    pub fn get_transaction_up_to_beacon_condition(
        &self,
        beacon: ImmutableFileNumber,
    ) -> WhereCondition {
        WhereCondition::new(
            "immutable_file_number <= ?*",
            vec![Value::Integer(beacon as i64)],
        )
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
