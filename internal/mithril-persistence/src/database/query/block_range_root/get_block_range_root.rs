use sqlite::Value;

use mithril_common::entities::BlockNumber;

use crate::database::record::BlockRangeRootRecord;
use crate::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

/// Simple queries to retrieve [BlockRangeRootRecord] from the sqlite database.
pub struct GetBlockRangeRootQuery {
    condition: WhereCondition,
}

impl GetBlockRangeRootQuery {
    pub fn all() -> Self {
        Self {
            condition: WhereCondition::default(),
        }
    }

    pub fn contains_or_below_block_number(block_number: BlockNumber) -> Self {
        Self {
            condition: WhereCondition::new("start < ?*", vec![Value::Integer(block_number as i64)]),
        }
    }

    pub fn highest() -> Self {
        Self {
            condition: WhereCondition::new("end = (select max(end) from block_range_root)", vec![]),
        }
    }
}

impl Query for GetBlockRangeRootQuery {
    type Entity = BlockRangeRootRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:block_range_root:}", "block_range_root")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("select {projection} from block_range_root where {condition} order by start, end")
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::crypto_helper::MKTreeNode;
    use mithril_common::entities::BlockRange;

    use crate::database::query::block_range_root::test_helper::insert_block_range_roots;
    use crate::database::query::GetBlockRangeRootQuery;
    use crate::database::test_helper::cardano_tx_db_connection;
    use crate::sqlite::ConnectionExtensions;

    use super::*;

    fn block_range_root_dataset() -> Vec<BlockRangeRootRecord> {
        [
            (
                BlockRange::from_block_number(15),
                MKTreeNode::from_hex("AAAA").unwrap(),
            ),
            (
                BlockRange::from_block_number(30),
                MKTreeNode::from_hex("BBBB").unwrap(),
            ),
            (
                BlockRange::from_block_number(45),
                MKTreeNode::from_hex("CCCC").unwrap(),
            ),
        ]
        .into_iter()
        .map(BlockRangeRootRecord::from)
        .collect()
    }

    #[test]
    fn test_get_contains_or_below_block_number_with_empty_db() {
        let connection = cardano_tx_db_connection().unwrap();

        let cursor: Vec<BlockRangeRootRecord> = connection
            .fetch_collect(GetBlockRangeRootQuery::contains_or_below_block_number(100))
            .unwrap();
        assert_eq!(Vec::<BlockRangeRootRecord>::new(), cursor);
    }

    #[test]
    fn test_get_contains_or_below_block_number_higher_than_the_highest_stored_block_range() {
        let connection = cardano_tx_db_connection().unwrap();
        let dataset = block_range_root_dataset();
        insert_block_range_roots(&connection, dataset.clone());

        let cursor: Vec<BlockRangeRootRecord> = connection
            .fetch_collect(GetBlockRangeRootQuery::contains_or_below_block_number(
                10_000,
            ))
            .unwrap();

        assert_eq!(dataset, cursor);
    }

    #[test]
    fn test_get_contains_or_below_block_number_below_end_of_the_third_block_range() {
        let connection = cardano_tx_db_connection().unwrap();
        let dataset = block_range_root_dataset();
        insert_block_range_roots(&connection, dataset.clone());

        let cursor: Vec<BlockRangeRootRecord> = connection
            .fetch_collect(GetBlockRangeRootQuery::contains_or_below_block_number(44))
            .unwrap();

        assert_eq!(&dataset[0..2], &cursor);
    }

    #[test]
    fn test_get_contains_or_below_block_number_equal_to_end_of_the_third_block_range() {
        let connection = cardano_tx_db_connection().unwrap();
        let dataset = block_range_root_dataset();
        insert_block_range_roots(&connection, dataset.clone());

        let cursor: Vec<BlockRangeRootRecord> = connection
            .fetch_collect(GetBlockRangeRootQuery::contains_or_below_block_number(45))
            .unwrap();

        assert_eq!(&dataset[0..2], &cursor);
    }

    #[test]
    fn test_get_contains_or_below_block_number_after_end_of_the_third_block_range() {
        let connection = cardano_tx_db_connection().unwrap();
        let dataset = block_range_root_dataset();
        insert_block_range_roots(&connection, dataset.clone());

        let cursor: Vec<BlockRangeRootRecord> = connection
            .fetch_collect(GetBlockRangeRootQuery::contains_or_below_block_number(46))
            .unwrap();

        assert_eq!(dataset, cursor);
    }

    #[test]
    fn test_get_highest_with_empty_db() {
        let connection = cardano_tx_db_connection().unwrap();

        let cursor: Option<BlockRangeRootRecord> = connection
            .fetch_first(GetBlockRangeRootQuery::highest())
            .unwrap();
        assert_eq!(None, cursor);
    }

    #[test]
    fn test_get_highest() {
        let connection = cardano_tx_db_connection().unwrap();
        let dataset = block_range_root_dataset();
        insert_block_range_roots(&connection, dataset.clone());

        let cursor: Option<BlockRangeRootRecord> = connection
            .fetch_first(GetBlockRangeRootQuery::highest())
            .unwrap();
        assert_eq!(dataset.last().cloned(), cursor);
    }
}
