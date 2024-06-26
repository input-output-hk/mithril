use mithril_common::entities::BlockNumber;
use sqlite::Value;

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

    pub fn up_to_block_number(up_to_or_equal_end_block_number: BlockNumber) -> Self {
        Self {
            condition: WhereCondition::new(
                "end <= ?*",
                vec![Value::Integer(up_to_or_equal_end_block_number as i64)],
            ),
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
