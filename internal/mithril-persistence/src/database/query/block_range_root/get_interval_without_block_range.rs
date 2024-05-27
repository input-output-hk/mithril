use crate::database::record::IntervalWithoutBlockRangeRootRecord;
use crate::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

/// Query that return the interval of block numbers that does not have a block range root.
pub struct GetIntervalWithoutBlockRangeRootQuery {
    condition: WhereCondition,
}

impl GetIntervalWithoutBlockRangeRootQuery {
    pub fn new() -> Self {
        Self {
            condition: Default::default(),
        }
    }
}

impl Query for GetIntervalWithoutBlockRangeRootQuery {
    type Entity = IntervalWithoutBlockRangeRootRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[
            ("{:interval_start:}", "interval_start"),
            ("{:interval_end:}", "interval_end"),
        ]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!(
            "select {projection} from \
        (select max(end) as start from block_range_root where {condition}) as interval_start, \
        (select max(block_number) as end from cardano_tx where {condition}) as interval_end"
        )
    }
}
