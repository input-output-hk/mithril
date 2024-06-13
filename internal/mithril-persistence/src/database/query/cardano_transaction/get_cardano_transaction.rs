use std::ops::Range;

use sqlite::Value;

use mithril_common::entities::{BlockNumber, BlockRange, TransactionHash};

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
                vec![Value::Integer(up_to_or_equal as i64)],
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
                    Value::Integer(block_range.start as i64),
                    Value::Integer(block_range.end as i64),
                ],
            ))
        }

        Self { condition }
    }

    pub fn between_blocks(range: Range<BlockNumber>) -> Self {
        let condition = WhereCondition::new(
            "block_number >= ?*",
            vec![Value::Integer(range.start as i64)],
        )
        .and_where(WhereCondition::new(
            "block_number < ?*",
            vec![Value::Integer(range.end as i64)],
        ));

        Self { condition }
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
                CardanoTransactionRecord::new("tx-hash-0", 10, 50, "block-hash-10", 1),
                CardanoTransactionRecord::new("tx-hash-1", 10, 51, "block-hash-10", 1),
                CardanoTransactionRecord::new("tx-hash-2", 11, 54, "block-hash-11", 1),
                CardanoTransactionRecord::new("tx-hash-3", 11, 55, "block-hash-11", 1),
            ],
        );

        let records: Vec<CardanoTransactionRecord> = connection
            .fetch_collect(GetCardanoTransactionQuery::with_highest_block_number())
            .unwrap();
        assert_eq!(
            vec![
                CardanoTransactionRecord::new("tx-hash-2", 11, 54, "block-hash-11", 1),
                CardanoTransactionRecord::new("tx-hash-3", 11, 55, "block-hash-11", 1),
            ],
            records
        );
    }
}
