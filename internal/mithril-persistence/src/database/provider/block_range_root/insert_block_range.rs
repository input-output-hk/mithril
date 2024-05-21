use std::iter::repeat;

use sqlite::Value;

use mithril_common::StdResult;

use crate::database::record::BlockRangeRootRecord;
use crate::sqlite::{Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition};

/// Query to insert [BlockRangeRootRecord] in the sqlite database
pub struct InsertBlockRangeRootProvider<'client> {
    connection: &'client SqliteConnection,
}

impl<'client> InsertBlockRangeRootProvider<'client> {
    /// Create a new instance
    pub fn new(connection: &'client SqliteConnection) -> Self {
        Self { connection }
    }

    /// Condition to insert multiples records.
    pub fn get_insert_many_condition(
        &self,
        block_range_records: Vec<BlockRangeRootRecord>,
    ) -> StdResult<WhereCondition> {
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

        Ok(WhereCondition::new(
            format!("{columns} values {}", values_columns.join(", ")).as_str(),
            values?,
        ))
    }
}

impl<'client> Provider<'client> for InsertBlockRangeRootProvider<'client> {
    type Entity = BlockRangeRootRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:block_range_root:}", "block_range_root")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("insert or ignore into block_range_root {condition} returning {projection}")
    }
}
