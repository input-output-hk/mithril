use anyhow::Context;
use sqlite::Value;

use mithril_common::entities::BlockNumber;
use mithril_common::StdResult;

use crate::database::record::CardanoTransactionRecord;
use crate::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

/// Query to delete old [CardanoTransactionRecord] from the sqlite database
pub struct DeleteCardanoTransactionQuery {
    condition: WhereCondition,
}

impl Query for DeleteCardanoTransactionQuery {
    type Entity = CardanoTransactionRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:cardano_tx:}", "cardano_tx")]));

        format!("delete from cardano_tx where {condition} returning {projection}")
    }
}

impl DeleteCardanoTransactionQuery {
    pub fn below_block_number_threshold(block_number_threshold: BlockNumber) -> StdResult<Self> {
        let threshold = Value::Integer(block_number_threshold.try_into().with_context(|| {
            format!("Failed to convert threshold `{block_number_threshold}` to i64")
        })?);

        Ok(Self {
            condition: WhereCondition::new("block_number < ?*", vec![threshold]),
        })
    }

    pub fn above_block_number_threshold(block_number_threshold: BlockNumber) -> StdResult<Self> {
        let threshold = Value::Integer(block_number_threshold.try_into().with_context(|| {
            format!("Failed to convert threshold `{block_number_threshold}` to i64")
        })?);

        Ok(Self {
            condition: WhereCondition::new("block_number > ?*", vec![threshold]),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::database::query::{GetCardanoTransactionQuery, InsertCardanoTransactionQuery};
    use crate::database::test_helper::cardano_tx_db_connection;
    use crate::sqlite::{ConnectionExtensions, SqliteConnection};

    use super::*;

    fn insert_transactions(connection: &SqliteConnection, records: Vec<CardanoTransactionRecord>) {
        connection
            .fetch_first(InsertCardanoTransactionQuery::insert_many(records).unwrap())
            .unwrap();
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

    mod prune_below_threshold_tests {
        use super::*;

        #[test]
        fn test_prune_work_even_without_transactions_in_db() {
            let connection = cardano_tx_db_connection().unwrap();

            let cursor = connection
                .fetch(DeleteCardanoTransactionQuery::below_block_number_threshold(100).unwrap())
                .expect("pruning shouldn't crash without transactions stored");
            assert_eq!(0, cursor.count());
        }

        #[test]
        fn test_prune_all_data_if_given_block_number_is_larger_than_stored_number_of_block() {
            let connection = cardano_tx_db_connection().unwrap();
            insert_transactions(&connection, test_transaction_set());

            let query =
                DeleteCardanoTransactionQuery::below_block_number_threshold(100_000).unwrap();
            let cursor = connection.fetch(query).unwrap();
            assert_eq!(test_transaction_set().len(), cursor.count());

            let cursor = connection.fetch(GetCardanoTransactionQuery::all()).unwrap();
            assert_eq!(0, cursor.count());
        }

        #[test]
        fn test_prune_keep_all_tx_of_last_block_if_given_number_of_block_is_zero() {
            let connection = cardano_tx_db_connection().unwrap();
            insert_transactions(&connection, test_transaction_set());

            let query = DeleteCardanoTransactionQuery::below_block_number_threshold(0).unwrap();
            let cursor = connection.fetch(query).unwrap();
            assert_eq!(0, cursor.count());

            let cursor = connection.fetch(GetCardanoTransactionQuery::all()).unwrap();
            assert_eq!(test_transaction_set().len(), cursor.count());
        }

        #[test]
        fn test_prune_data_of_below_given_blocks() {
            let connection = cardano_tx_db_connection().unwrap();
            insert_transactions(&connection, test_transaction_set());

            let query = DeleteCardanoTransactionQuery::below_block_number_threshold(12).unwrap();
            let cursor = connection.fetch(query).unwrap();
            assert_eq!(4, cursor.count());

            let cursor = connection.fetch(GetCardanoTransactionQuery::all()).unwrap();
            assert_eq!(2, cursor.count());
        }
    }

    mod prune_above_threshold_tests {
        use super::*;

        #[test]
        fn test_prune_work_even_without_transactions_in_db() {
            let connection = cardano_tx_db_connection().unwrap();

            let cursor = connection
                .fetch(DeleteCardanoTransactionQuery::above_block_number_threshold(100).unwrap())
                .expect("pruning shouldn't crash without transactions stored");
            assert_eq!(0, cursor.count());
        }

        #[test]
        fn test_prune_all_data_if_given_block_number_is_lower_than_stored_number_of_block() {
            let connection = cardano_tx_db_connection().unwrap();
            insert_transactions(&connection, test_transaction_set());

            let query = DeleteCardanoTransactionQuery::above_block_number_threshold(0).unwrap();
            let cursor = connection.fetch(query).unwrap();
            assert_eq!(test_transaction_set().len(), cursor.count());

            let cursor = connection.fetch(GetCardanoTransactionQuery::all()).unwrap();
            assert_eq!(0, cursor.count());
        }

        #[test]
        fn test_prune_keep_all_tx_of_last_block_if_given_number_of_block_is_greater_than_the_highest_one(
        ) {
            let connection = cardano_tx_db_connection().unwrap();
            insert_transactions(&connection, test_transaction_set());

            let query =
                DeleteCardanoTransactionQuery::above_block_number_threshold(100_000).unwrap();
            let cursor = connection.fetch(query).unwrap();
            assert_eq!(0, cursor.count());

            let cursor = connection.fetch(GetCardanoTransactionQuery::all()).unwrap();
            assert_eq!(test_transaction_set().len(), cursor.count());
        }

        #[test]
        fn test_prune_data_of_above_given_blocks() {
            let connection = cardano_tx_db_connection().unwrap();
            insert_transactions(&connection, test_transaction_set());

            let query = DeleteCardanoTransactionQuery::above_block_number_threshold(10).unwrap();
            let cursor = connection.fetch(query).unwrap();
            assert_eq!(4, cursor.count());

            let cursor = connection.fetch(GetCardanoTransactionQuery::all()).unwrap();
            assert_eq!(2, cursor.count());
        }
    }
}
