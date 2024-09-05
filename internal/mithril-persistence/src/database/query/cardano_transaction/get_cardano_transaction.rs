use std::ops::Range;

use sqlite::Value;

use mithril_common::entities::{BlockNumber, BlockRange, SlotNumber, TransactionHash};

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
    pub fn by_transaction_hash(transaction_hash: &TransactionHash) -> Self {
        Self {
            condition: WhereCondition::new(
                "transaction_hash = ?*",
                vec![Value::String(transaction_hash.to_owned())],
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

    pub fn by_block_ranges(block_ranges: Vec<BlockRange>) -> Self {
        let mut condition = WhereCondition::default();
        for block_range in block_ranges {
            condition = condition.or_where(WhereCondition::new(
                "(block_number >= ?* and block_number < ?*)",
                vec![
                    Value::Integer(*block_range.start as i64),
                    Value::Integer(*block_range.end as i64),
                ],
            ))
        }

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

    pub fn with_highest_block_number_below_slot_number(slot_number: SlotNumber) -> Self {
        Self {
            condition: WhereCondition::new(
                "block_number = (select max(block_number) from cardano_tx where slot_number <= ?*)",
                vec![Value::Integer(*slot_number as i64)],
            ),
        }
    }

    pub fn with_highest_block_number() -> Self {
        Self {
            condition: WhereCondition::new(
                "block_number = (select max(block_number) from cardano_tx)",
                vec![],
            ),
        }
    }
}

impl Query for GetCardanoTransactionQuery {
    type Entity = CardanoTransactionRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:cardano_tx:}", "cardano_tx")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("select {projection} from cardano_tx where {condition} order by block_number, transaction_hash")
    }
}

#[cfg(test)]
mod tests {
    use crate::database::query::InsertCardanoTransactionQuery;
    use crate::database::test_helper::cardano_tx_db_connection;
    use crate::sqlite::{ConnectionExtensions, SqliteConnection};

    use super::*;

    fn insert_transactions(connection: &SqliteConnection, records: Vec<CardanoTransactionRecord>) {
        connection
            .fetch_first(InsertCardanoTransactionQuery::insert_many(records).unwrap())
            .unwrap();
    }

    fn transaction_record(
        block_number: BlockNumber,
        slot_number: SlotNumber,
    ) -> CardanoTransactionRecord {
        CardanoTransactionRecord::new(
            format!("tx-hash-{}", slot_number),
            block_number,
            slot_number,
            format!("block-hash-{}", block_number),
        )
    }

    #[test]
    fn with_highest_block_number() {
        let connection = cardano_tx_db_connection().unwrap();

        let cursor = connection
            .fetch(GetCardanoTransactionQuery::with_highest_block_number())
            .unwrap();
        assert_eq!(0, cursor.count());

        insert_transactions(
            &connection,
            vec![
                transaction_record(BlockNumber(10), SlotNumber(50)),
                transaction_record(BlockNumber(10), SlotNumber(51)),
                transaction_record(BlockNumber(11), SlotNumber(54)),
                transaction_record(BlockNumber(11), SlotNumber(55)),
            ],
        );

        let records: Vec<CardanoTransactionRecord> = connection
            .fetch_collect(GetCardanoTransactionQuery::with_highest_block_number())
            .unwrap();
        assert_eq!(
            vec![
                transaction_record(BlockNumber(11), SlotNumber(54)),
                transaction_record(BlockNumber(11), SlotNumber(55)),
            ],
            records
        );
    }

    #[test]
    fn with_highest_block_number_below_slot_number() {
        let connection = cardano_tx_db_connection().unwrap();

        let cursor = connection
            .fetch(
                GetCardanoTransactionQuery::with_highest_block_number_below_slot_number(
                    SlotNumber(51),
                ),
            )
            .unwrap();
        assert_eq!(0, cursor.count());

        insert_transactions(
            &connection,
            vec![transaction_record(BlockNumber(2), SlotNumber(5))],
        );

        let records: Vec<CardanoTransactionRecord> = connection
            .fetch_collect(
                GetCardanoTransactionQuery::with_highest_block_number_below_slot_number(
                    SlotNumber(5),
                ),
            )
            .unwrap();
        assert_eq!(
            vec![transaction_record(BlockNumber(2), SlotNumber(5)),],
            records
        );

        insert_transactions(
            &connection,
            vec![
                transaction_record(BlockNumber(10), SlotNumber(50)),
                transaction_record(BlockNumber(11), SlotNumber(51)),
                transaction_record(BlockNumber(14), SlotNumber(54)),
                transaction_record(BlockNumber(15), SlotNumber(55)),
            ],
        );

        let records: Vec<CardanoTransactionRecord> = connection
            .fetch_collect(
                GetCardanoTransactionQuery::with_highest_block_number_below_slot_number(
                    SlotNumber(53),
                ),
            )
            .unwrap();
        assert_eq!(
            vec![transaction_record(BlockNumber(11), SlotNumber(51)),],
            records
        );

        let records: Vec<CardanoTransactionRecord> = connection
            .fetch_collect(
                GetCardanoTransactionQuery::with_highest_block_number_below_slot_number(
                    SlotNumber(54),
                ),
            )
            .unwrap();
        assert_eq!(
            vec![transaction_record(BlockNumber(14), SlotNumber(54)),],
            records
        );
    }
}
