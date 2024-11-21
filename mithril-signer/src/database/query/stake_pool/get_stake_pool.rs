use sqlite::Value;

use mithril_common::entities::Epoch;
use mithril_common::StdResult;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::StakePool;

/// Simple queries to retrieve [StakePool] from the sqlite database.
pub struct GetStakePoolQuery {
    condition: WhereCondition,
}

impl GetStakePoolQuery {
    /// Get StakePools for a given Epoch for given pool_ids.
    pub fn by_epoch(epoch: Epoch) -> StdResult<Self> {
        let condition = WhereCondition::new("epoch = ?*", vec![Value::Integer(epoch.try_into()?)]);

        Ok(Self { condition })
    }

    #[cfg(test)]
    pub(crate) fn all() -> Self {
        Self {
            condition: WhereCondition::default(),
        }
    }
}

impl Query for GetStakePoolQuery {
    type Entity = StakePool;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:stake_pool:}", "sp")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("select {projection} from stake_pool as sp where {condition} order by epoch asc, stake desc, stake_pool_id asc")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::database::{query::InsertOrReplaceStakePoolQuery, test_helper::main_db_connection};
    use mithril_persistence::sqlite::ConnectionExtensions;

    #[test]
    fn test_query_sorts_the_return_stake_pool_by_epoch_stack_and_stake_pool_id() {
        let connection = main_db_connection().unwrap();
        connection
            .apply(InsertOrReplaceStakePoolQuery::many(vec![
                ("pool-A".to_string(), Epoch(1), 1500),
                ("pool-D".to_string(), Epoch(2), 1250),
                ("pool-B".to_string(), Epoch(1), 1000),
                ("pool-E".to_string(), Epoch(1), 1600),
                ("pool-C".to_string(), Epoch(1), 1600),
            ]))
            .unwrap();

        let stake_pool_in_database: Vec<StakePool> =
            connection.fetch_collect(GetStakePoolQuery::all()).unwrap();

        assert_eq!(
            vec![
                ("pool-C".to_string(), Epoch(1), 1600),
                ("pool-E".to_string(), Epoch(1), 1600),
                ("pool-A".to_string(), Epoch(1), 1500),
                ("pool-B".to_string(), Epoch(1), 1000),
                ("pool-D".to_string(), Epoch(2), 1250),
            ],
            stake_pool_in_database
                .into_iter()
                .map(|s| (s.stake_pool_id, s.epoch, s.stake))
                .collect::<Vec<_>>()
        );
    }
}
