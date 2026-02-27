use std::ops::Range;

use sqlite::Value;

use mithril_common::entities::{BlockNumber, TransactionHash};

use crate::database::record::CardanoTransactionRecord;
use crate::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

/// Simple queries to retrieve [CardanoTransaction] from the sqlite database.
pub struct GetCardanoTransactionQuery {
    condition: WhereCondition,
}

impl GetCardanoTransactionQuery {
    pub fn all() -> Self {
        Self {
            condition: WhereCondition::default(),
        }
    }

    // Useful in test and probably in the future.
    pub fn by_transaction_hash<T: Into<TransactionHash>>(transaction_hash: T) -> Self {
        Self {
            condition: WhereCondition::new(
                "transaction_hash = ?*",
                vec![Value::String(transaction_hash.into())],
            ),
        }
    }

    pub fn by_transaction_hashes(
        transactions_hashes: Vec<TransactionHash>,
        up_to_or_equal: BlockNumber,
    ) -> Self {
        let hashes_values = transactions_hashes.into_iter().map(Value::String).collect();
        let condition = WhereCondition::where_in("transaction_hash", hashes_values).and_where(
            WhereCondition::new(
                "block_number <= ?*",
                vec![Value::Integer(*up_to_or_equal as i64)],
            ),
        );

        Self { condition }
    }

    pub fn between_blocks<T: Into<Range<BlockNumber>>>(range: T) -> Self {
        let block_range = range.into();
        let condition = WhereCondition::new(
            "(block_number >= ?* and block_number < ?*)",
            vec![
                Value::Integer(*block_range.start as i64),
                Value::Integer(*block_range.end as i64),
            ],
        );

        Self { condition }
    }
}

impl Query for GetCardanoTransactionQuery {
    type Entity = CardanoTransactionRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[
            ("{:cardano_tx:}", "cardano_tx"),
            ("{:cardano_block:}", "cardano_block"),
        ]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!(
            r#"
select {projection}
from cardano_block
    inner join cardano_tx on cardano_block.block_hash = cardano_tx.block_hash
where {condition}
order by cardano_block.block_number, cardano_tx.transaction_hash
"#
        )
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{CardanoBlockWithTransactions, SlotNumber};

    use crate::database::test_helper::{
        cardano_tx_db_connection, insert_cardano_blocks_and_transaction,
    };
    use crate::sqlite::{ConnectionExtensions, SqliteConnection};

    use super::*;

    fn transaction_record(
        tx_hash: &str,
        block_number: BlockNumber,
        slot_number: SlotNumber,
    ) -> CardanoTransactionRecord {
        CardanoTransactionRecord::new(
            tx_hash.to_owned(),
            block_number,
            slot_number,
            format!("block_hash-{block_number}"),
        )
    }

    fn test_blocks_transactions_set() -> Vec<CardanoBlockWithTransactions> {
        vec![
            CardanoBlockWithTransactions::new(
                "block_hash-10",
                BlockNumber(10),
                SlotNumber(50),
                vec!["tx-1", "tx-2"],
            ),
            CardanoBlockWithTransactions::new(
                "block_hash-14",
                BlockNumber(14),
                SlotNumber(54),
                vec!["tx-3"],
            ),
            CardanoBlockWithTransactions::new(
                "block_hash-15",
                BlockNumber(15),
                SlotNumber(55),
                vec!["tx-4"],
            ),
            CardanoBlockWithTransactions::new(
                "block_hash-20",
                BlockNumber(20),
                SlotNumber(60),
                vec!["tx-5"],
            ),
            CardanoBlockWithTransactions::new(
                "block_hash-24",
                BlockNumber(24),
                SlotNumber(64),
                vec!["tx-6"],
            ),
            CardanoBlockWithTransactions::new(
                "block_hash-30",
                BlockNumber(30),
                SlotNumber(70),
                vec!["tx-7"],
            ),
        ]
    }

    fn connection_with_test_data_set() -> SqliteConnection {
        let connection = cardano_tx_db_connection().unwrap();
        insert_cardano_blocks_and_transaction(&connection, test_blocks_transactions_set());
        connection
    }

    mod get_by_transaction_hash {
        use super::*;

        #[tokio::test]
        async fn get_existing_hash() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoTransactionRecord> = connection
                .fetch_collect(GetCardanoTransactionQuery::by_transaction_hash("tx-1"))
                .unwrap();

            assert_eq!(
                vec![transaction_record("tx-1", BlockNumber(10), SlotNumber(50))],
                result
            );
        }

        #[tokio::test]
        async fn get_non_existing_transaction() {
            let connection = connection_with_test_data_set();
            let result = connection
                .fetch_first(GetCardanoTransactionQuery::by_transaction_hash(
                    "tx-not-exist",
                ))
                .unwrap();

            assert_eq!(None, result);
        }
    }

    mod get_by_transaction_hashes {
        use super::*;

        #[tokio::test]
        async fn empty_hashes_list() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoTransactionRecord> = connection
                .fetch_collect(GetCardanoTransactionQuery::by_transaction_hashes(
                    vec![],
                    BlockNumber(30),
                ))
                .unwrap();

            assert_eq!(Vec::<CardanoTransactionRecord>::new(), result);
        }

        #[tokio::test]
        async fn one_hash_exactly_at_boundary_block_number() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoTransactionRecord> = connection
                .fetch_collect(GetCardanoTransactionQuery::by_transaction_hashes(
                    vec!["tx-7".to_string()],
                    BlockNumber(30),
                ))
                .unwrap();

            assert_eq!(
                vec![transaction_record("tx-7", BlockNumber(30), SlotNumber(70))],
                result
            );
        }

        #[tokio::test]
        async fn multiple_hashes_with_one_above_block_number_boundary() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoTransactionRecord> = connection
                .fetch_collect(GetCardanoTransactionQuery::by_transaction_hashes(
                    vec!["tx-1".to_string(), "tx-4".to_string(), "tx-6".to_string()],
                    BlockNumber(20),
                ))
                .unwrap();

            assert_eq!(
                vec![
                    transaction_record("tx-1", BlockNumber(10), SlotNumber(50)),
                    transaction_record("tx-4", BlockNumber(15), SlotNumber(55)),
                ],
                result
            );
        }

        #[tokio::test]
        async fn all_hashes_higher_than_block_number_boundary() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoTransactionRecord> = connection
                .fetch_collect(GetCardanoTransactionQuery::by_transaction_hashes(
                    vec!["tx-1".to_string(), "tx-4".to_string()],
                    BlockNumber(5),
                ))
                .unwrap();

            assert_eq!(Vec::<CardanoTransactionRecord>::new(), result);
        }
    }

    mod get_between_blocks {
        use super::*;

        #[tokio::test]
        async fn range_that_contains_no_block() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoTransactionRecord> = connection
                .fetch_collect(GetCardanoTransactionQuery::between_blocks(
                    BlockNumber(100)..BlockNumber(300),
                ))
                .unwrap();

            assert_eq!(Vec::<CardanoTransactionRecord>::new(), result);
        }

        #[tokio::test]
        async fn get_with_empty_range() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoTransactionRecord> = connection
                .fetch_collect(GetCardanoTransactionQuery::between_blocks(
                    BlockNumber(30)..BlockNumber(30),
                ))
                .unwrap();

            assert_eq!(Vec::<CardanoTransactionRecord>::new(), result);
        }

        #[tokio::test]
        async fn range_that_contains_a_block() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoTransactionRecord> = connection
                .fetch_collect(GetCardanoTransactionQuery::between_blocks(
                    BlockNumber(25)..BlockNumber(35),
                ))
                .unwrap();

            assert_eq!(
                vec![transaction_record("tx-7", BlockNumber(30), SlotNumber(70))],
                result
            );
        }

        #[tokio::test]
        async fn start_boundary_is_inclusive() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoTransactionRecord> = connection
                .fetch_collect(GetCardanoTransactionQuery::between_blocks(
                    BlockNumber(30)..BlockNumber(36),
                ))
                .unwrap();

            assert_eq!(
                vec![transaction_record("tx-7", BlockNumber(30), SlotNumber(70))],
                result
            );
        }

        #[tokio::test]
        async fn end_boundary_is_exclusive() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoTransactionRecord> = connection
                .fetch_collect(GetCardanoTransactionQuery::between_blocks(
                    BlockNumber(28)..BlockNumber(30),
                ))
                .unwrap();

            assert_eq!(Vec::<CardanoTransactionRecord>::new(), result);
        }

        #[tokio::test]
        async fn get_block_right_after_start_boundary() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoTransactionRecord> = connection
                .fetch_collect(GetCardanoTransactionQuery::between_blocks(
                    BlockNumber(29)..BlockNumber(34),
                ))
                .unwrap();

            assert_eq!(
                vec![transaction_record("tx-7", BlockNumber(30), SlotNumber(70))],
                result
            );
        }

        #[tokio::test]
        async fn get_block_right_below_end_boundary() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoTransactionRecord> = connection
                .fetch_collect(GetCardanoTransactionQuery::between_blocks(
                    BlockNumber(28)..BlockNumber(31),
                ))
                .unwrap();

            assert_eq!(
                vec![transaction_record("tx-7", BlockNumber(30), SlotNumber(70))],
                result
            );
        }
    }
}
