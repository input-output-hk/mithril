use std::iter::repeat;

use chrono::Utc;
use sqlite::Value;

use mithril_common::entities::{Epoch, PartyId, Stake};
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::StakePool;

/// Query to insert or replace [StakePool] in the sqlite database
pub struct InsertOrReplaceStakePoolQuery {
    condition: WhereCondition,
}

impl InsertOrReplaceStakePoolQuery {
    pub fn many(records: Vec<(PartyId, Epoch, Stake)>) -> Self {
        let columns = "(stake_pool_id, epoch, stake, created_at)";
        let values_columns: Vec<&str> = repeat("(?*, ?*, ?*, ?*)").take(records.len()).collect();
        let values = records
            .into_iter()
            .flat_map(|(stake_pool_id, epoch, stake)| {
                vec![
                    Value::String(stake_pool_id),
                    Value::Integer(epoch.try_into().unwrap()),
                    Value::Integer(i64::try_from(stake).unwrap()),
                    Value::String(Utc::now().to_rfc3339()),
                ]
            })
            .collect();
        let condition = WhereCondition::new(
            format!("{columns} values {}", values_columns.join(", ")).as_str(),
            values,
        );

        Self { condition }
    }
}

impl Query for InsertOrReplaceStakePoolQuery {
    type Entity = StakePool;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:stake_pool:}", "stake_pool")]));

        format!("insert or replace into stake_pool {condition} returning {projection}")
    }
}

#[cfg(test)]
mod tests {
    use crate::database::query::GetStakePoolQuery;
    use crate::database::test_helper::{insert_stake_pool, main_db_connection};
    use mithril_persistence::sqlite::ConnectionExtensions;

    use super::*;

    #[test]
    fn test_update_stakes() {
        let connection = main_db_connection().unwrap();
        insert_stake_pool(&connection, &[3]).unwrap();

        let pools: Vec<StakePool> = connection
            .fetch_collect(InsertOrReplaceStakePoolQuery::many(vec![(
                "pool4".to_string(),
                Epoch(3),
                9999,
            )]))
            .unwrap();
        let stake_pool = pools.first().unwrap();

        assert_eq!("pool4".to_string(), stake_pool.stake_pool_id);
        assert_eq!(Epoch(3), stake_pool.epoch);
        assert_eq!(9999, stake_pool.stake);

        let mut cursor = connection
            .fetch(GetStakePoolQuery::by_epoch(Epoch(3)).unwrap())
            .unwrap();
        let stake_pool = cursor.next().expect("Should have a stake pool 'pool4'.");

        assert_eq!("pool4".to_string(), stake_pool.stake_pool_id);
        assert_eq!(Epoch(3), stake_pool.epoch);
        assert_eq!(9999, stake_pool.stake);
        assert_eq!(3, cursor.count());
    }
}
