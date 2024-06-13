use anyhow::Context;
use sqlite::Value;

use mithril_common::entities::{BlockNumber, BlockRange};
use mithril_common::StdResult;

use crate::database::record::BlockRangeRootRecord;
use crate::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

/// Query to delete old [BlockRangeRootRecord] from the sqlite database
pub struct DeleteBlockRangeRootQuery {
    condition: WhereCondition,
}

impl Query for DeleteBlockRangeRootQuery {
    type Entity = BlockRangeRootRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let aliases = SourceAlias::new(&[("{:block_range_root:}", "block_range_root")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("delete from block_range_root where {condition} returning {projection}")
    }
}

impl DeleteBlockRangeRootQuery {
    pub fn contains_or_above_block_number_threshold(
        block_number_threshold: BlockNumber,
    ) -> StdResult<Self> {
        let block_range = BlockRange::from_block_number(block_number_threshold);
        let threshold = Value::Integer(block_range.start.try_into().with_context(|| {
            format!("Failed to convert threshold `{block_number_threshold}` to i64")
        })?);

        Ok(Self {
            condition: WhereCondition::new("start >= ?*", vec![threshold]),
        })
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::crypto_helper::MKTreeNode;
    use mithril_common::entities::BlockRange;

    use crate::database::query::{GetBlockRangeRootQuery, InsertBlockRangeRootQuery};
    use crate::database::test_helper::cardano_tx_db_connection;
    use crate::sqlite::{ConnectionExtensions, SqliteConnection};

    use super::*;

    fn insert_block_range_roots(connection: &SqliteConnection, records: Vec<BlockRangeRootRecord>) {
        connection
            .fetch_first(InsertBlockRangeRootQuery::insert_many(records).unwrap())
            .unwrap();
    }

    fn block_range_root_dataset() -> Vec<BlockRangeRootRecord> {
        [
            (
                BlockRange::from_block_number(BlockRange::LENGTH),
                MKTreeNode::from_hex("AAAA").unwrap(),
            ),
            (
                BlockRange::from_block_number(BlockRange::LENGTH * 2),
                MKTreeNode::from_hex("BBBB").unwrap(),
            ),
            (
                BlockRange::from_block_number(BlockRange::LENGTH * 3),
                MKTreeNode::from_hex("CCCC").unwrap(),
            ),
        ]
        .into_iter()
        .map(BlockRangeRootRecord::from)
        .collect()
    }

    #[test]
    fn test_prune_work_even_without_block_range_root_in_db() {
        let connection = cardano_tx_db_connection().unwrap();

        let cursor = connection
            .fetch(
                DeleteBlockRangeRootQuery::contains_or_above_block_number_threshold(100).unwrap(),
            )
            .expect("pruning shouldn't crash without block range root stored");
        assert_eq!(0, cursor.count());
    }

    #[test]
    fn test_prune_all_data_if_given_block_number_is_lower_than_stored_number_of_block() {
        parameterized_test_prune_block_range(0, block_range_root_dataset().len());
    }

    #[test]
    fn test_prune_keep_all_block_range_root_if_given_number_of_block_is_greater_than_the_highest_one(
    ) {
        parameterized_test_prune_block_range(100_000, 0);
    }

    #[test]
    fn test_prune_block_range_when_block_number_is_block_range_start() {
        parameterized_test_prune_block_range(BlockRange::LENGTH * 2, 2);
    }

    #[test]
    fn test_prune_block_range_when_block_number_is_in_block_range() {
        parameterized_test_prune_block_range(BlockRange::LENGTH * 2 + 1, 2);
    }

    #[test]
    fn test_keep_block_range_when_block_number_is_just_before_range_start() {
        parameterized_test_prune_block_range(BlockRange::LENGTH * 2 - 1, 3);
    }

    fn parameterized_test_prune_block_range(
        block_threshold: BlockNumber,
        delete_record_number: usize,
    ) {
        let connection = cardano_tx_db_connection().unwrap();
        let dataset = block_range_root_dataset();
        insert_block_range_roots(&connection, dataset.clone());

        let query =
            DeleteBlockRangeRootQuery::contains_or_above_block_number_threshold(block_threshold)
                .unwrap();
        let cursor = connection.fetch(query).unwrap();
        assert_eq!(delete_record_number, cursor.count());

        let cursor = connection.fetch(GetBlockRangeRootQuery::all()).unwrap();
        assert_eq!(dataset.len() - delete_record_number, cursor.count());
    }
}
