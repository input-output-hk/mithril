use sqlite::Value;
use std::ops::Range;

use mithril_common::entities::{BlockNumber, BlockRange};

use crate::database::record::CardanoBlockTransactionsRecord;
use crate::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

/// Simple queries to retrieve [CardanoBlockTransactionsRecord] from the sqlite database.
///
/// IMPORTANT: records returned by this query are unordered as the order is done rust side after
/// conversion to `CardanoBlockTransactionMkTreeNode`.
pub struct GetCardanoBlockTransactionsQuery {
    condition: WhereCondition,
}

impl GetCardanoBlockTransactionsQuery {
    // Useful in test and probably in the future.
    #[cfg(test)]
    pub fn by_block_hash<T: Into<mithril_common::entities::BlockHash>>(block_hash: T) -> Self {
        Self {
            condition: WhereCondition::new(
                "cardano_block.block_hash = ?*",
                vec![Value::String(block_hash.into())],
            ),
        }
    }

    pub fn by_block_range(block_range: BlockRange) -> Self {
        let condition = WhereCondition::new(
            "(block_number >= ?* and block_number < ?*)",
            vec![
                Value::Integer(*block_range.start as i64),
                Value::Integer(*block_range.end as i64),
            ],
        );

        Self { condition }
    }

    pub fn between_blocks(range: Range<BlockNumber>) -> Self {
        let condition = WhereCondition::new(
            "block_number >= ?*",
            vec![Value::Integer(*range.start as i64)],
        )
        .and_where(WhereCondition::new(
            "block_number < ?*",
            vec![Value::Integer(*range.end as i64)],
        ));

        Self { condition }
    }
}

impl Query for GetCardanoBlockTransactionsQuery {
    type Entity = CardanoBlockTransactionsRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[
            ("{:cardano_block:}", "cardano_block"),
            ("{:cardano_tx:}", "cardano_tx"),
        ]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!(
            r#"
select {projection}
from cardano_block
    left join cardano_tx on cardano_block.block_hash = cardano_tx.block_hash
where {condition}
group by cardano_block.block_hash
"#
        )
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{BlockNumber, CardanoBlockWithTransactions, SlotNumber};
    use mithril_common::test::entities_extensions::BlockRangeTestExtension;

    use crate::database::record::CardanoBlockTransactionsRecord;
    use crate::database::test_helper::{
        cardano_tx_db_connection, insert_cardano_blocks_and_transaction,
    };
    use crate::sqlite::{ConnectionExtensions, SqliteConnection};

    use super::*;

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
            CardanoBlockWithTransactions::new(
                "block_hash-40",
                BlockNumber(40),
                SlotNumber(80),
                Vec::<String>::new(),
            ),
        ]
    }

    fn connection_with_test_data_set() -> SqliteConnection {
        let connection = cardano_tx_db_connection().unwrap();
        insert_cardano_blocks_and_transaction(&connection, test_blocks_transactions_set());
        connection
    }

    fn blocks_with_txs_record(
        block_number: BlockNumber,
        slot_number: SlotNumber,
        tx_hashes: &[&str],
    ) -> CardanoBlockTransactionsRecord {
        CardanoBlockTransactionsRecord::new(
            format!("block_hash-{block_number}"),
            block_number,
            slot_number,
            tx_hashes.to_vec(),
        )
    }

    mod get_by_block_hash {
        use super::*;

        #[tokio::test]
        async fn get_non_existing_transaction() {
            let connection = connection_with_test_data_set();
            let result = connection
                .fetch_first(GetCardanoBlockTransactionsQuery::by_block_hash(
                    "block-not-exist",
                ))
                .unwrap();

            assert_eq!(None, result);
        }

        #[tokio::test]
        async fn get_existing_hash_with_transactions() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoBlockTransactionsRecord> = connection
                .fetch_collect(GetCardanoBlockTransactionsQuery::by_block_hash(
                    "block_hash-10",
                ))
                .unwrap();

            assert_eq!(
                vec![blocks_with_txs_record(
                    BlockNumber(10),
                    SlotNumber(50),
                    &["tx-1", "tx-2"]
                )],
                result
            );
        }

        #[tokio::test]
        async fn get_existing_hash_without_transaction() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoBlockTransactionsRecord> = connection
                .fetch_collect(GetCardanoBlockTransactionsQuery::by_block_hash(
                    "block_hash-40",
                ))
                .unwrap();

            assert_eq!(
                vec![blocks_with_txs_record(BlockNumber(40), SlotNumber(80), &[])],
                result
            );
        }
    }

    mod get_by_block_range {
        use super::*;

        #[tokio::test]
        async fn get_a_range_that_does_not_intersect_any_block() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoBlockTransactionsRecord> = connection
                .fetch_collect(GetCardanoBlockTransactionsQuery::by_block_range(
                    BlockRange::new(100, 200),
                ))
                .unwrap();

            assert_eq!(Vec::<CardanoBlockTransactionsRecord>::new(), result);
        }

        #[tokio::test]
        async fn get_a_range_with_a_block_contained_in_the_range() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoBlockTransactionsRecord> = connection
                .fetch_collect(GetCardanoBlockTransactionsQuery::by_block_range(
                    BlockRange::new(9, 12),
                ))
                .unwrap();

            assert_eq!(
                vec![blocks_with_txs_record(
                    BlockNumber(10),
                    SlotNumber(50),
                    &["tx-1", "tx-2"]
                ),],
                result
            );
        }

        #[tokio::test]
        async fn get_a_range_with_a_block_at_start_boundary() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoBlockTransactionsRecord> = connection
                .fetch_collect(GetCardanoBlockTransactionsQuery::by_block_range(
                    BlockRange::new(10, 12),
                ))
                .unwrap();

            assert_eq!(
                vec![blocks_with_txs_record(
                    BlockNumber(10),
                    SlotNumber(50),
                    &["tx-1", "tx-2"]
                ),],
                result
            );
        }

        #[tokio::test]
        async fn get_a_range_with_a_block_at_end_boundary() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoBlockTransactionsRecord> = connection
                .fetch_collect(GetCardanoBlockTransactionsQuery::by_block_range(
                    BlockRange::new(14, 15),
                ))
                .unwrap();

            assert_eq!(
                vec![blocks_with_txs_record(BlockNumber(14), SlotNumber(54), &["tx-3"])],
                result
            );
        }
    }

    mod between_blocks {
        use super::*;

        #[tokio::test]
        async fn range_that_contains_no_block() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoBlockTransactionsRecord> = connection
                .fetch_collect(GetCardanoBlockTransactionsQuery::between_blocks(
                    BlockNumber(100)..BlockNumber(300),
                ))
                .unwrap();

            assert_eq!(Vec::<CardanoBlockTransactionsRecord>::new(), result);
        }

        #[tokio::test]
        async fn get_with_empty_range() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoBlockTransactionsRecord> = connection
                .fetch_collect(GetCardanoBlockTransactionsQuery::between_blocks(
                    BlockNumber(30)..BlockNumber(30),
                ))
                .unwrap();

            assert_eq!(Vec::<CardanoBlockTransactionsRecord>::new(), result);
        }

        #[tokio::test]
        async fn range_that_contains_a_block() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoBlockTransactionsRecord> = connection
                .fetch_collect(GetCardanoBlockTransactionsQuery::between_blocks(
                    BlockNumber(25)..BlockNumber(35),
                ))
                .unwrap();

            assert_eq!(
                vec![blocks_with_txs_record(BlockNumber(30), SlotNumber(70), &["tx-7"])],
                result
            );
        }

        #[tokio::test]
        async fn start_boundary_is_inclusive() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoBlockTransactionsRecord> = connection
                .fetch_collect(GetCardanoBlockTransactionsQuery::between_blocks(
                    BlockNumber(30)..BlockNumber(36),
                ))
                .unwrap();

            assert_eq!(
                vec![blocks_with_txs_record(BlockNumber(30), SlotNumber(70), &["tx-7"])],
                result
            );
        }

        #[tokio::test]
        async fn end_boundary_is_exclusive() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoBlockTransactionsRecord> = connection
                .fetch_collect(GetCardanoBlockTransactionsQuery::between_blocks(
                    BlockNumber(28)..BlockNumber(30),
                ))
                .unwrap();

            assert_eq!(Vec::<CardanoBlockTransactionsRecord>::new(), result);
        }

        #[tokio::test]
        async fn get_block_right_after_start_boundary() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoBlockTransactionsRecord> = connection
                .fetch_collect(GetCardanoBlockTransactionsQuery::between_blocks(
                    BlockNumber(29)..BlockNumber(34),
                ))
                .unwrap();

            assert_eq!(
                vec![blocks_with_txs_record(BlockNumber(30), SlotNumber(70), &["tx-7"])],
                result
            );
        }

        #[tokio::test]
        async fn get_block_right_below_end_boundary() {
            let connection = connection_with_test_data_set();
            let result: Vec<CardanoBlockTransactionsRecord> = connection
                .fetch_collect(GetCardanoBlockTransactionsQuery::between_blocks(
                    BlockNumber(28)..BlockNumber(31),
                ))
                .unwrap();

            assert_eq!(
                vec![blocks_with_txs_record(BlockNumber(30), SlotNumber(70), &["tx-7"])],
                result
            );
        }
    }
}
