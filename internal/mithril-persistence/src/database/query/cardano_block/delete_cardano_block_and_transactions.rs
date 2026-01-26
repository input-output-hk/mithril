use anyhow::Context;
use sqlite::Value;

use mithril_common::StdResult;
use mithril_common::entities::BlockNumber;

use crate::database::record::CardanoBlockRecord;
use crate::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

/// Query to delete old [CardanoBlockRecord] from the sqlite database (with cascade delete of associated transactions records)
pub struct DeleteCardanoBlockAndTransactionQuery {
    condition: WhereCondition,
}

impl Query for DeleteCardanoBlockAndTransactionQuery {
    type Entity = CardanoBlockRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:cardano_block:}", "cardano_block")]));

        format!("delete from cardano_block where {condition} returning {projection}")
    }
}

impl DeleteCardanoBlockAndTransactionQuery {
    #[cfg(test)]
    pub fn all() -> Self {
        Self {
            condition: WhereCondition::default(),
        }
    }

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
    use mithril_common::entities::{CardanoBlockWithTransactions, SlotNumber};

    use crate::database::query::{GetCardanoBlockQuery, GetCardanoTransactionQuery};
    use crate::database::test_helper::{
        cardano_tx_db_connection, insert_cardano_blocks_and_transaction,
    };
    use crate::sqlite::ConnectionExtensions;

    use super::*;

    fn test_blocks_transactions_set() -> Vec<CardanoBlockWithTransactions> {
        vec![
            CardanoBlockWithTransactions::new(
                "block-hash-10",
                BlockNumber(10),
                SlotNumber(50),
                vec!["tx-hash-0", "tx-hash-1"],
            ),
            CardanoBlockWithTransactions::new(
                "block-hash-11",
                BlockNumber(11),
                SlotNumber(51),
                vec!["tx-hash-2", "tx-hash-3"],
            ),
            CardanoBlockWithTransactions::new(
                "block-hash-12",
                BlockNumber(12),
                SlotNumber(52),
                vec!["tx-hash-4", "tx-hash-5"],
            ),
        ]
    }

    #[test]
    fn test_delete_all_data() {
        let connection = cardano_tx_db_connection().unwrap();
        insert_cardano_blocks_and_transaction(&connection, test_blocks_transactions_set());

        let query = DeleteCardanoBlockAndTransactionQuery::all();
        let cursor = connection.fetch(query).unwrap();
        assert_eq!(test_blocks_transactions_set().len(), cursor.count());

        let cursor = connection.fetch(GetCardanoBlockQuery::all()).unwrap();
        assert_eq!(0, cursor.count());

        let cursor = connection.fetch(GetCardanoTransactionQuery::all()).unwrap();
        assert_eq!(0, cursor.count());
    }

    #[tokio::test]
    async fn ensure_transactions_are_deleted_after_block_deletion() {
        let connection = cardano_tx_db_connection().unwrap();
        insert_cardano_blocks_and_transaction(
            &connection,
            vec![CardanoBlockWithTransactions::new(
                "block-hash-10",
                BlockNumber(10),
                SlotNumber(50),
                vec!["tx-hash-0"],
            )],
        );

        connection
            .fetch_first(DeleteCardanoBlockAndTransactionQuery::all())
            .unwrap();

        let number_of_remaining_txs: i64 = connection
            .query_single_cell("select count(*) from cardano_tx", &[])
            .unwrap();
        assert_eq!(0, number_of_remaining_txs);
    }

    mod prune_below_threshold_tests {
        use super::*;

        #[test]
        fn test_prune_work_even_without_transactions_in_db() {
            let connection = cardano_tx_db_connection().unwrap();

            let cursor = connection
                .fetch(
                    DeleteCardanoBlockAndTransactionQuery::below_block_number_threshold(
                        BlockNumber(100),
                    )
                    .unwrap(),
                )
                .expect("pruning shouldn't crash without transactions stored");
            assert_eq!(0, cursor.count());
        }

        #[test]
        fn test_prune_all_data_if_given_block_number_is_larger_than_stored_number_of_block() {
            let connection = cardano_tx_db_connection().unwrap();
            insert_cardano_blocks_and_transaction(&connection, test_blocks_transactions_set());

            let query = DeleteCardanoBlockAndTransactionQuery::below_block_number_threshold(
                BlockNumber(100_000),
            )
            .unwrap();
            let cursor = connection.fetch(query).unwrap();
            assert_eq!(test_blocks_transactions_set().len(), cursor.count());

            let cursor = connection.fetch(GetCardanoBlockQuery::all()).unwrap();
            assert_eq!(0, cursor.count());

            let cursor = connection.fetch(GetCardanoTransactionQuery::all()).unwrap();
            assert_eq!(0, cursor.count());
        }

        #[test]
        fn test_prune_keep_all_tx_of_last_block_if_given_number_of_block_is_zero() {
            let connection = cardano_tx_db_connection().unwrap();
            insert_cardano_blocks_and_transaction(&connection, test_blocks_transactions_set());

            let query =
                DeleteCardanoBlockAndTransactionQuery::below_block_number_threshold(BlockNumber(0))
                    .unwrap();
            let cursor = connection.fetch(query).unwrap();
            assert_eq!(0, cursor.count());

            let cursor = connection.fetch(GetCardanoBlockQuery::all()).unwrap();
            assert_eq!(test_blocks_transactions_set().len(), cursor.count());

            let cursor = connection.fetch(GetCardanoTransactionQuery::all()).unwrap();
            assert_eq!(
                test_blocks_transactions_set()
                    .iter()
                    .map(|x| x.transactions_count())
                    .sum::<usize>(),
                cursor.count()
            );
        }

        #[test]
        fn test_prune_data_of_below_given_blocks() {
            let connection = cardano_tx_db_connection().unwrap();
            insert_cardano_blocks_and_transaction(&connection, test_blocks_transactions_set());

            let query = DeleteCardanoBlockAndTransactionQuery::below_block_number_threshold(
                BlockNumber(12),
            )
            .unwrap();
            let cursor = connection.fetch(query).unwrap();
            assert_eq!(2, cursor.count());

            let cursor = connection.fetch(GetCardanoBlockQuery::all()).unwrap();
            assert_eq!(1, cursor.count());

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
                .fetch(
                    DeleteCardanoBlockAndTransactionQuery::above_block_number_threshold(
                        BlockNumber(100),
                    )
                    .unwrap(),
                )
                .expect("pruning shouldn't crash without transactions stored");
            assert_eq!(0, cursor.count());
        }

        #[test]
        fn test_prune_all_data_if_given_block_number_is_lower_than_stored_number_of_block() {
            let connection = cardano_tx_db_connection().unwrap();
            insert_cardano_blocks_and_transaction(&connection, test_blocks_transactions_set());

            let query =
                DeleteCardanoBlockAndTransactionQuery::above_block_number_threshold(BlockNumber(0))
                    .unwrap();
            let cursor = connection.fetch(query).unwrap();
            assert_eq!(test_blocks_transactions_set().len(), cursor.count());

            let cursor = connection.fetch(GetCardanoBlockQuery::all()).unwrap();
            assert_eq!(0, cursor.count());

            let cursor = connection.fetch(GetCardanoTransactionQuery::all()).unwrap();
            assert_eq!(0, cursor.count());
        }

        #[test]
        fn test_prune_keep_all_tx_of_last_block_if_given_number_of_block_is_greater_than_the_highest_one()
         {
            let connection = cardano_tx_db_connection().unwrap();
            insert_cardano_blocks_and_transaction(&connection, test_blocks_transactions_set());

            let query = DeleteCardanoBlockAndTransactionQuery::above_block_number_threshold(
                BlockNumber(100_000),
            )
            .unwrap();
            let cursor = connection.fetch(query).unwrap();
            assert_eq!(0, cursor.count());

            let cursor = connection.fetch(GetCardanoBlockQuery::all()).unwrap();
            assert_eq!(test_blocks_transactions_set().len(), cursor.count());

            let cursor = connection.fetch(GetCardanoTransactionQuery::all()).unwrap();
            assert_eq!(
                test_blocks_transactions_set()
                    .iter()
                    .map(|x| x.transactions_count())
                    .sum::<usize>(),
                cursor.count()
            );
        }

        #[test]
        fn test_prune_data_of_above_given_blocks() {
            let connection = cardano_tx_db_connection().unwrap();
            insert_cardano_blocks_and_transaction(&connection, test_blocks_transactions_set());

            let query = DeleteCardanoBlockAndTransactionQuery::above_block_number_threshold(
                BlockNumber(10),
            )
            .unwrap();
            let cursor = connection.fetch(query).unwrap();
            assert_eq!(2, cursor.count());

            let cursor = connection.fetch(GetCardanoBlockQuery::all()).unwrap();
            assert_eq!(1, cursor.count());

            let cursor = connection.fetch(GetCardanoTransactionQuery::all()).unwrap();
            assert_eq!(2, cursor.count());
        }
    }
}
