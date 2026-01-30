use sqlite::Value;

use mithril_common::entities::{BlockHash, SlotNumber};

use crate::database::record::CardanoBlockRecord;
use crate::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

/// Simple queries to retrieve [CardanoBlockRecord] from the sqlite database.
pub struct GetCardanoBlockQuery {
    condition: WhereCondition,
}

impl GetCardanoBlockQuery {
    pub fn all() -> Self {
        Self {
            condition: WhereCondition::default(),
        }
    }

    // Useful in test and probably in the future.
    pub fn by_block_hash(block_hash: &BlockHash) -> Self {
        Self {
            condition: WhereCondition::new(
                "block_hash = ?*",
                vec![Value::String(block_hash.to_owned())],
            ),
        }
    }

    pub fn with_highest_block_number_below_slot_number(slot_number: SlotNumber) -> Self {
        Self {
            condition: WhereCondition::new(
                "block_number = (select max(block_number) from cardano_block where slot_number <= ?*)",
                vec![Value::Integer(*slot_number as i64)],
            ),
        }
    }

    pub fn with_highest_block_number() -> Self {
        Self {
            condition: WhereCondition::new(
                "block_number = (select max(block_number) from cardano_block)",
                vec![],
            ),
        }
    }
}

impl Query for GetCardanoBlockQuery {
    type Entity = CardanoBlockRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:cardano_block:}", "cardano_block")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!(
            "select {projection} from cardano_block where {condition} order by block_number, block_hash"
        )
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::BlockNumber;

    use crate::database::query::InsertCardanoBlockQuery;
    use crate::database::test_helper::cardano_tx_db_connection;
    use crate::sqlite::{ConnectionExtensions, SqliteConnection};

    use super::*;

    fn insert_blocks(connection: &SqliteConnection, records: Vec<CardanoBlockRecord>) {
        connection
            .fetch_first(InsertCardanoBlockQuery::insert_many(records).unwrap())
            .unwrap();
    }

    fn block_record(block_number: BlockNumber, slot_number: SlotNumber) -> CardanoBlockRecord {
        CardanoBlockRecord::new(
            format!("block-hash-{block_number}"),
            block_number,
            slot_number,
        )
    }

    #[test]
    fn with_highest_block_number() {
        let connection = cardano_tx_db_connection().unwrap();

        let cursor = connection
            .fetch(GetCardanoBlockQuery::with_highest_block_number())
            .unwrap();
        assert_eq!(0, cursor.count());

        insert_blocks(
            &connection,
            vec![
                block_record(BlockNumber(10), SlotNumber(50)),
                block_record(BlockNumber(11), SlotNumber(51)),
                block_record(BlockNumber(12), SlotNumber(54)),
                block_record(BlockNumber(13), SlotNumber(55)),
            ],
        );

        let records: Vec<CardanoBlockRecord> = connection
            .fetch_collect(GetCardanoBlockQuery::with_highest_block_number())
            .unwrap();
        assert_eq!(vec![block_record(BlockNumber(13), SlotNumber(55))], records);
    }

    #[test]
    fn with_highest_block_number_below_slot_number() {
        let connection = cardano_tx_db_connection().unwrap();

        let cursor = connection
            .fetch(
                GetCardanoBlockQuery::with_highest_block_number_below_slot_number(SlotNumber(51)),
            )
            .unwrap();
        assert_eq!(0, cursor.count());

        insert_blocks(
            &connection,
            vec![block_record(BlockNumber(2), SlotNumber(5))],
        );

        let records: Vec<CardanoBlockRecord> = connection
            .fetch_collect(
                GetCardanoBlockQuery::with_highest_block_number_below_slot_number(SlotNumber(5)),
            )
            .unwrap();
        assert_eq!(vec![block_record(BlockNumber(2), SlotNumber(5)),], records);

        insert_blocks(
            &connection,
            vec![
                block_record(BlockNumber(10), SlotNumber(50)),
                block_record(BlockNumber(11), SlotNumber(51)),
                block_record(BlockNumber(14), SlotNumber(54)),
                block_record(BlockNumber(15), SlotNumber(55)),
            ],
        );

        let records: Vec<CardanoBlockRecord> = connection
            .fetch_collect(
                GetCardanoBlockQuery::with_highest_block_number_below_slot_number(SlotNumber(53)),
            )
            .unwrap();
        assert_eq!(
            vec![block_record(BlockNumber(11), SlotNumber(51)),],
            records
        );

        let records: Vec<CardanoBlockRecord> = connection
            .fetch_collect(
                GetCardanoBlockQuery::with_highest_block_number_below_slot_number(SlotNumber(54)),
            )
            .unwrap();
        assert_eq!(
            vec![block_record(BlockNumber(14), SlotNumber(54)),],
            records
        );
    }
}
