use std::iter::repeat_n;

use sqlite::Value;

use mithril_common::StdResult;

use crate::database::record::CardanoBlockRecord;
use crate::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

/// Query to insert [CardanoBlockRecord] in the sqlite database
pub struct InsertCardanoBlockQuery {
    condition: WhereCondition,
}

impl InsertCardanoBlockQuery {
    /// Query that insert multiples records.
    pub fn insert_many(transactions_records: Vec<CardanoBlockRecord>) -> StdResult<Self> {
        let columns = "(block_number, slot_number, block_hash)";
        let values_columns: Vec<&str> =
            repeat_n("(?*, ?*, ?*)", transactions_records.len()).collect();

        let values: StdResult<Vec<Value>> =
            transactions_records.into_iter().try_fold(vec![], |mut vec, record| {
                vec.append(&mut vec![
                    Value::Integer(record.block_number.try_into()?),
                    Value::Integer(record.slot_number.try_into()?),
                    Value::String(record.block_hash.clone()),
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

impl Query for InsertCardanoBlockQuery {
    type Entity = CardanoBlockRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:cardano_block:}", "cardano_block")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("insert or ignore into cardano_block {condition} returning {projection}")
    }
}
