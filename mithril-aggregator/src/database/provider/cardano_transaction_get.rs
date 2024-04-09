use sqlite::Value;

use mithril_common::entities::{ImmutableFileNumber, TransactionHash};
use mithril_persistence::sqlite::{
    Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::CardanoTransactionRecord;

/// Simple queries to retrieve [CardanoTransaction] from the sqlite database.
pub(crate) struct GetCardanoTransactionProvider<'client> {
    connection: &'client SqliteConnection,
}

impl<'client> GetCardanoTransactionProvider<'client> {
    /// Create a new instance
    pub fn new(connection: &'client SqliteConnection) -> Self {
        Self { connection }
    }

    // Useful in test and probably in the future.
    pub(crate) fn get_transaction_hash_condition(
        &self,
        transaction_hash: &TransactionHash,
    ) -> WhereCondition {
        WhereCondition::new(
            "transaction_hash = ?*",
            vec![Value::String(transaction_hash.to_owned())],
        )
    }

    pub(crate) fn get_transaction_up_to_beacon_condition(
        &self,
        beacon: ImmutableFileNumber,
    ) -> WhereCondition {
        WhereCondition::new(
            "immutable_file_number <= ?*",
            vec![Value::Integer(beacon as i64)],
        )
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

        format!("select {projection} from cardano_tx where {condition} order by rowid")
    }
}
