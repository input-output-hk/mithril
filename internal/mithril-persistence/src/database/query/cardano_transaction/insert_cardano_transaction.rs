use std::iter::repeat_n;

use sqlite::Value;

use mithril_common::StdResult;

use crate::database::record::StorableCardanoTransactionRecord;
use crate::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

/// Query to insert [CardanoTransactionRecord] in the sqlite database
pub struct InsertCardanoTransactionQuery {
    condition: WhereCondition,
}

impl InsertCardanoTransactionQuery {
    /// Query that insert multiples records.
    pub fn insert_many(
        transactions_records: Vec<StorableCardanoTransactionRecord>,
    ) -> StdResult<Self> {
        let columns = "(transaction_hash,  block_hash)";
        let values_columns: Vec<&str> = repeat_n("(?*, ?*)", transactions_records.len()).collect();

        let values: StdResult<Vec<Value>> =
            transactions_records.into_iter().try_fold(vec![], |mut vec, record| {
                vec.append(&mut vec![
                    Value::String(record.transaction_hash),
                    Value::String(record.block_hash),
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
    type Entity = StorableCardanoTransactionRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:cardano_tx:}", "cardano_tx")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("insert or ignore into cardano_tx {condition} returning {projection}")
    }
}
