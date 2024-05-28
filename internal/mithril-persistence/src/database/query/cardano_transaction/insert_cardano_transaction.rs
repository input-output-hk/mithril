use std::iter::repeat;

use sqlite::Value;

use mithril_common::StdResult;

use crate::database::record::CardanoTransactionRecord;
use crate::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

/// Query to insert [CardanoTransactionRecord] in the sqlite database
pub struct InsertCardanoTransactionQuery {
    condition: WhereCondition,
}

impl InsertCardanoTransactionQuery {
    /// Query that insert one record.
    pub fn insert_one(record: &CardanoTransactionRecord) -> StdResult<Self> {
        Self::insert_many(vec![record.clone()])
    }

    /// Query that insert multiples records.
    pub fn insert_many(transactions_records: Vec<CardanoTransactionRecord>) -> StdResult<Self> {
        let columns =
            "(transaction_hash, block_number, slot_number, block_hash, immutable_file_number)";
        let values_columns: Vec<&str> = repeat("(?*, ?*, ?*, ?*, ?*)")
            .take(transactions_records.len())
            .collect();

        let values: StdResult<Vec<Value>> =
            transactions_records
                .into_iter()
                .try_fold(vec![], |mut vec, record| {
                    vec.append(&mut vec![
                        Value::String(record.transaction_hash),
                        Value::Integer(record.block_number.try_into()?),
                        Value::Integer(record.slot_number.try_into()?),
                        Value::String(record.block_hash.clone()),
                        Value::Integer(record.immutable_file_number.try_into()?),
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

impl Query for InsertCardanoTransactionQuery {
    type Entity = CardanoTransactionRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:cardano_tx:}", "cardano_tx")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("insert or ignore into cardano_tx {condition} returning {projection}")
    }
}
