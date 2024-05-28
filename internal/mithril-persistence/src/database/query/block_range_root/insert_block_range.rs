use std::iter::repeat;

use sqlite::Value;

use mithril_common::StdResult;

use crate::database::record::BlockRangeRootRecord;
use crate::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

/// Query to insert [BlockRangeRootRecord] in the sqlite database
pub struct InsertBlockRangeRootQuery {
    condition: WhereCondition,
}

impl InsertBlockRangeRootQuery {
    /// Query that insert multiples records.
    pub fn insert_many(block_range_records: Vec<BlockRangeRootRecord>) -> StdResult<Self> {
        let columns = "(start, end, merkle_root)";
        let values_columns: Vec<&str> = repeat("(?*, ?*, ?*)")
            .take(block_range_records.len())
            .collect();

        let values: StdResult<Vec<Value>> =
            block_range_records
                .into_iter()
                .try_fold(vec![], |mut vec, record| {
                    vec.append(&mut vec![
                        Value::Integer(record.range.start.try_into()?),
                        Value::Integer(record.range.end.try_into()?),
                        Value::String(record.merkle_root.to_hex()),
                    ]);
                    Ok(vec)
                });
        let condition = WhereCondition::new(
            format!("{columns} values {}", values_columns.join(", ")).as_str(),
            values?,
        );

        Ok(Self { condition })
    }
}

impl Query for InsertBlockRangeRootQuery {
    type Entity = BlockRangeRootRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:block_range_root:}", "block_range_root")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("insert or ignore into block_range_root {condition} returning {projection}")
    }
}
