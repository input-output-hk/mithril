use std::iter::repeat;

use chrono::Utc;
use sqlite::Value;

use mithril_common::entities::{Epoch, PartyId, Stake};
use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    EntityCursor, Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::StakePool;

/// Simple queries to retrieve [StakePool] from the sqlite database.
pub(crate) struct GetStakePoolProvider<'client> {
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

/// Query to insert or replace [StakePool] in the sqlite database
pub(crate) struct InsertOrReplaceStakePoolProvider<'conn> {
    connection: &'conn SqliteConnection,
}

impl<'conn> InsertOrReplaceStakePoolProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn SqliteConnection) -> Self {
        Self { connection }
    }

    pub(crate) fn get_insert_or_replace_condition(
        &self,
        records: Vec<(PartyId, Epoch, Stake)>,
    ) -> WhereCondition {
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

        WhereCondition::new(
            format!("{columns} values {}", values_columns.join(", ")).as_str(),
            values,
        )
    }

    pub(crate) fn persist_many(
        &self,
        records: Vec<(PartyId, Epoch, Stake)>,
    ) -> StdResult<Vec<StakePool>> {
        let filters = self.get_insert_or_replace_condition(records);

        Ok(self.find(filters)?.collect())
    }
}

impl<'conn> Provider<'conn> for InsertOrReplaceStakePoolProvider<'conn> {
    type Entity = StakePool;

    fn get_connection(&'conn self) -> &'conn SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:stake_pool:}", "stake_pool")]));

        format!("insert or replace into stake_pool {condition} returning {projection}")
    }
}

/// Query to delete old [StakePool] from the sqlite database
pub(crate) struct DeleteStakePoolProvider<'conn> {
    connection: &'conn SqliteConnection,
}

impl<'conn> Provider<'conn> for DeleteStakePoolProvider<'conn> {
    type Entity = StakePool;

    fn get_connection(&'conn self) -> &'conn SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:stake_pool:}", "stake_pool")]));

        format!("delete from stake_pool where {condition} returning {projection}")
    }
}

impl<'conn> DeleteStakePoolProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn SqliteConnection) -> Self {
        Self { connection }
    }

    /// Create the SQL condition to prune data older than the given Epoch.
    fn get_prune_condition(&self, epoch_threshold: Epoch) -> WhereCondition {
        WhereCondition::new(
            "epoch < ?*",
            vec![Value::Integer(epoch_threshold.try_into().unwrap())],
        )
    }

    /// Prune the stake pools data older than the given epoch.
    pub fn prune(&self, epoch_threshold: Epoch) -> StdResult<EntityCursor<StakePool>> {
        let filters = self.get_prune_condition(epoch_threshold);

        self.find(filters)
    }
}

#[cfg(test)]
mod tests {
    use sqlite::Connection;

    use crate::database::test_helper::{apply_all_migrations_to_db, insert_stake_pool};

    use super::*;

    pub fn setup_stake_db(
        connection: &SqliteConnection,
        epoch_to_insert_stake_pools: &[i64],
    ) -> StdResult<()> {
        apply_all_migrations_to_db(connection)?;
        insert_stake_pool(connection, epoch_to_insert_stake_pools)
    }

    #[test]
    fn test_get_stake_pools() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_stake_db(&connection, &[1, 2, 3]).unwrap();

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

    #[test]
    fn test_update_stakes() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_stake_db(&connection, &[3]).unwrap();

        let provider = InsertOrReplaceStakePoolProvider::new(&connection);
        let pools = provider
            .persist_many(vec![("pool4".to_string(), Epoch(3), 9999)])
            .unwrap();
        let stake_pool = pools.first().unwrap();

        assert_eq!("pool4".to_string(), stake_pool.stake_pool_id);
        assert_eq!(Epoch(3), stake_pool.epoch);
        assert_eq!(9999, stake_pool.stake);

        let provider = GetStakePoolProvider::new(&connection);
        let mut cursor = provider.get_by_epoch(&Epoch(3)).unwrap();
        let stake_pool = cursor.next().expect("Should have a stake pool 'pool4'.");

        assert_eq!("pool4".to_string(), stake_pool.stake_pool_id);
        assert_eq!(Epoch(3), stake_pool.epoch);
        assert_eq!(9999, stake_pool.stake);
        assert_eq!(3, cursor.count());
    }

    #[test]
    fn test_prune() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_stake_db(&connection, &[1, 2]).unwrap();

        let provider = DeleteStakePoolProvider::new(&connection);
        let cursor = provider.prune(Epoch(2)).unwrap();

        assert_eq!(3, cursor.count());

        let provider = GetStakePoolProvider::new(&connection);
        let cursor = provider.get_by_epoch(&Epoch(1)).unwrap();

        assert_eq!(0, cursor.count());

        let cursor = provider.get_by_epoch(&Epoch(2)).unwrap();

        assert_eq!(3, cursor.count());
    }
}
