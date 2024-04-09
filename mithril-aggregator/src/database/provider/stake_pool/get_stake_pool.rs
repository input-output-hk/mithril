use sqlite::Value;

use mithril_common::entities::Epoch;
use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    EntityCursor, Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::StakePool;

/// Simple queries to retrieve [StakePool] from the sqlite database.
pub struct GetStakePoolProvider<'client> {
    client: &'client SqliteConnection,
}

impl<'client> GetStakePoolProvider<'client> {
    /// Create a new provider
    pub fn new(client: &'client SqliteConnection) -> Self {
        Self { client }
    }

    fn condition_by_epoch(&self, epoch: &Epoch) -> StdResult<WhereCondition> {
        Ok(WhereCondition::new(
            "epoch = ?*",
            vec![Value::Integer(epoch.try_into()?)],
        ))
    }

    /// Get StakePools for a given Epoch for given pool_ids.
    pub fn get_by_epoch(&self, epoch: &Epoch) -> StdResult<EntityCursor<StakePool>> {
        let filters = self.condition_by_epoch(epoch)?;
        let stake_pool = self.find(filters)?;

        Ok(stake_pool)
    }
}

impl<'client> Provider<'client> for GetStakePoolProvider<'client> {
    type Entity = StakePool;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.client
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

    use super::*;

    #[test]
    fn test_get_stake_pools() {
        let connection = main_db_connection().unwrap();
        insert_stake_pool(&connection, &[1, 2, 3]).unwrap();

        let provider = GetStakePoolProvider::new(&connection);
        let mut cursor = provider.get_by_epoch(&Epoch(1)).unwrap();

        let stake_pool = cursor.next().expect("Should have a stake pool 'pool3'.");
        assert_eq!(
            ("pool3".to_string(), Epoch(1), 1200),
            (stake_pool.stake_pool_id, stake_pool.epoch, stake_pool.stake)
        );
        assert_eq!(2, cursor.count());

        let mut cursor = provider.get_by_epoch(&Epoch(3)).unwrap();

        let stake_pool = cursor.next().expect("Should have a stake pool 'pool2'.");
        assert_eq!(
            ("pool2".to_string(), Epoch(3), 1190),
            (stake_pool.stake_pool_id, stake_pool.epoch, stake_pool.stake)
        );
        assert_eq!(2, cursor.count());

        let cursor = provider.get_by_epoch(&Epoch(5)).unwrap();
        assert_eq!(0, cursor.count());
    }
}
