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
    use crate::database::test_helper::{insert_stake_pool, main_db_connection};
    use mithril_persistence::sqlite::ConnectionExtensions;

    use super::*;

    #[test]
    fn test_get_stake_pools() {
        let connection = main_db_connection().unwrap();
        insert_stake_pool(&connection, &[1, 2, 3]).unwrap();

        let mut cursor = connection
            .fetch(GetStakePoolQuery::by_epoch(Epoch(1)).unwrap())
            .unwrap();

        let stake_pool = cursor.next().expect("Should have a stake pool 'pool3'.");
        assert_eq!(
            ("pool3".to_string(), Epoch(1), 1200),
            (stake_pool.stake_pool_id, stake_pool.epoch, stake_pool.stake)
        );
        assert_eq!(2, cursor.count());

        let mut cursor = connection
            .fetch(GetStakePoolQuery::by_epoch(Epoch(3)).unwrap())
            .unwrap();

        let stake_pool = cursor.next().expect("Should have a stake pool 'pool2'.");
        assert_eq!(
            ("pool2".to_string(), Epoch(3), 1190),
            (stake_pool.stake_pool_id, stake_pool.epoch, stake_pool.stake)
        );
        assert_eq!(2, cursor.count());

        let cursor = connection
            .fetch(GetStakePoolQuery::by_epoch(Epoch(5)).unwrap())
            .unwrap();
        assert_eq!(0, cursor.count());
    }
}
