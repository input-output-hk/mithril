use std::ops::Not;
use std::sync::Arc;

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use sqlite::{Connection, Value};

use mithril_common::{
    entities::{Epoch, PartyId, Stake, StakeDistribution},
    sqlite::{
        EntityCursor, HydrationError, Projection, Provider, SourceAlias, SqLiteEntity,
        WhereCondition,
    },
    store::{adapter::AdapterError, StakeStorer, StoreError},
};

use mithril_common::StdError;
use tokio::sync::Mutex;

/// Stake pool as read from Chain.
#[derive(Debug, PartialEq)]
pub struct StakePool {
    /// Pool Id
    stake_pool_id: PartyId,

    /// Total stake of this pool.
    stake: u64,

    /// Epoch at which this pool is valid.
    epoch: Epoch,

    /// DateTime of the record creation.
    created_at: DateTime<Utc>,
}

impl SqLiteEntity for StakePool {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let epoch_int = row.read::<i64, _>(2);
        let datetime = &row.read::<&str, _>(3);
        let stake = row.read::<i64, _>(1);

        let stake_pool = Self {
            stake_pool_id: row.read::<&str, _>(0).to_string(),
            stake: u64::try_from(stake).map_err(|e| {
                HydrationError::InconsistentType(format!(
                    "Could not cast the stake from internal db I64 → U64. Error: '{e}'."
                ))
            })?,
            epoch: Epoch(epoch_int.try_into().map_err(|e| {
                HydrationError::InvalidData(format!(
                    "Could not cast i64 ({epoch_int}) to u64. Error: '{e}'"
                ))
            })?),
            created_at: DateTime::parse_from_rfc3339(datetime)
                .map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{datetime}' to rfc3339 Datetime. Error: {e}"
                    ))
                })?
                .with_timezone(&Utc),
        };

        Ok(stake_pool)
    }

    fn get_projection() -> Projection {
        let mut projection = Projection::default();
        projection.add_field("stake_pool_id", "{:stake_pool:}.stake_pool_id", "text");
        projection.add_field("stake", "{:stake_pool:}.stake", "integer");
        projection.add_field("epoch", "{:stake_pool:}.epoch", "integer");
        projection.add_field("created_at", "{:stake_pool:}.created_at", "text");

        projection
    }
}

/// Simple [StakePool] provider.
pub struct StakePoolProvider<'client> {
    client: &'client Connection,
}

impl<'client> StakePoolProvider<'client> {
    /// Create a new provider
    pub fn new(client: &'client Connection) -> Self {
        Self { client }
    }

    fn condition_by_epoch(&self, epoch: &Epoch) -> Result<WhereCondition, StdError> {
        let epoch: i64 = i64::try_from(epoch.0)?;

        Ok(WhereCondition::new(
            "epoch = ?*",
            vec![Value::Integer(epoch)],
        ))
    }

    /// Get StakePools for a given Epoch for given pool_ids.
    pub fn get_by_epoch(&self, epoch: &Epoch) -> Result<EntityCursor<StakePool>, StdError> {
        let filters = self.condition_by_epoch(epoch)?;
        let stake_pool = self.find(filters)?;

        Ok(stake_pool)
    }
}

impl<'client> Provider<'client> for StakePoolProvider<'client> {
    type Entity = StakePool;

    fn get_connection(&'client self) -> &'client Connection {
        self.client
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:stake_pool:}", "sp")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("select {projection} from stake_pool as sp where {condition} order by epoch asc, stake desc, stake_pool_id asc")
    }
}

/// Query to update the stake distribution
pub struct InsertOrReplaceStakePoolProvider<'conn> {
    connection: &'conn Connection,
}

impl<'conn> InsertOrReplaceStakePoolProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn Connection) -> Self {
        Self { connection }
    }

    fn get_insert_or_replace_condition(
        &self,
        stake_pool_id: &str,
        epoch: Epoch,
        stake: Stake,
    ) -> WhereCondition {
        let epoch = i64::try_from(epoch.0).unwrap();
        let stake = i64::try_from(stake).unwrap();

        WhereCondition::new(
            "(stake_pool_id, epoch, stake, created_at) values (?1, ?2, ?3, ?4)",
            vec![
                Value::String(stake_pool_id.to_owned()),
                Value::Integer(epoch),
                Value::Integer(stake),
                Value::String(Utc::now().to_rfc3339()),
            ],
        )
    }

    fn persist(
        &self,
        stake_pool_id: &str,
        epoch: Epoch,
        stake: Stake,
    ) -> Result<StakePool, StdError> {
        let filters = self.get_insert_or_replace_condition(stake_pool_id, epoch, stake);

        let entity = self.find(filters)?
            .next()
            .unwrap_or_else(|| panic!("No entity returned by the persister, stake_pool_id = {stake_pool_id} for epoch {epoch:?}"));

        Ok(entity)
    }
}

impl<'conn> Provider<'conn> for InsertOrReplaceStakePoolProvider<'conn> {
    type Entity = StakePool;

    fn get_connection(&'conn self) -> &'conn Connection {
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

/// Provider to remove old data from the stake_pool table
pub struct DeleteStakePoolProvider<'conn> {
    connection: &'conn Connection,
}

impl<'conn> Provider<'conn> for DeleteStakePoolProvider<'conn> {
    type Entity = StakePool;

    fn get_connection(&'conn self) -> &'conn Connection {
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
    pub fn new(connection: &'conn Connection) -> Self {
        Self { connection }
    }

    /// Create the SQL condition to prune data older than the given Epoch.
    fn get_prune_condition(&self, epoch_threshold: Epoch) -> WhereCondition {
        let epoch_value = Value::Integer(i64::try_from(epoch_threshold.0).unwrap());

        WhereCondition::new("epoch < ?*", vec![epoch_value])
    }

    /// Prune the stake pools data older than the given epoch.
    pub fn prune(&self, epoch_threshold: Epoch) -> Result<EntityCursor<StakePool>, StdError> {
        let filters = self.get_prune_condition(epoch_threshold);

        self.find(filters)
    }
}

/// Service to deal with stake pools (read & write).
pub struct StakePoolStore {
    connection: Arc<Mutex<Connection>>,

    /// Number of epochs before previous records will be pruned at the next call to
    /// [save_protocol_parameters][StakePoolStore::save_stakes].
    retention_limit: Option<u64>,
}

impl StakePoolStore {
    /// Create a new StakePool service
    pub fn new(connection: Arc<Mutex<Connection>>, retention_limit: Option<u64>) -> Self {
        Self {
            connection,
            retention_limit,
        }
    }
}

#[async_trait]
impl StakeStorer for StakePoolStore {
    async fn save_stakes(
        &self,
        epoch: Epoch,
        stakes: StakeDistribution,
    ) -> Result<Option<StakeDistribution>, StoreError> {
        let connection = &*self.connection.lock().await;
        let provider = InsertOrReplaceStakePoolProvider::new(connection);
        let mut new_stakes = StakeDistribution::new();
        connection
            .execute("begin transaction")
            .map_err(|e| AdapterError::QueryError(e.into()))?;

        for (pool_id, stake) in stakes {
            let stake_pool = provider
                .persist(&pool_id, epoch, stake)
                .map_err(|e| AdapterError::GeneralError(format!("{e}")))?;
            new_stakes.insert(pool_id.to_string(), stake_pool.stake);
        }

        // Prune useless old stake distributions.
        if let Some(threshold) = self.retention_limit {
            let _ = DeleteStakePoolProvider::new(connection)
                .prune(epoch - threshold)
                .map_err(AdapterError::QueryError)?
                .count();
        }

        connection
            .execute("commit transaction")
            .map_err(|e| AdapterError::QueryError(e.into()))?;

        Ok(Some(new_stakes))
    }

    async fn get_stakes(&self, epoch: Epoch) -> Result<Option<StakeDistribution>, StoreError> {
        let connection = &*self.connection.lock().await;
        let provider = StakePoolProvider::new(connection);
        let cursor = provider
            .get_by_epoch(&epoch)
            .map_err(|e| AdapterError::GeneralError(format!("Could not get stakes: {e}")))?;
        let mut stake_distribution = StakeDistribution::new();

        for stake_pool in cursor {
            stake_distribution.insert(stake_pool.stake_pool_id, stake_pool.stake);
        }

        Ok(stake_distribution
            .is_empty()
            .not()
            .then_some(stake_distribution))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::database::provider::apply_all_migrations_to_db;

    pub fn setup_stake_db(
        connection: &Connection,
        epoch_to_insert_settings: &[i64],
    ) -> Result<(), StdError> {
        apply_all_migrations_to_db(connection)?;

        let query = {
            // leverage the expanded parameter from this provider which is unit
            // tested on its own above.
            let update_provider = InsertOrReplaceStakePoolProvider::new(connection);
            let (sql_values, _) = update_provider
                .get_insert_or_replace_condition("pool_id", Epoch(1), 1000)
                .expand();

            format!("insert into stake_pool {sql_values}")
        };

        // Note: decreasing stakes for pool3 so we can test that the order has changed
        for (pool_id, epoch, stake) in epoch_to_insert_settings.iter().flat_map(|epoch| {
            [
                ("pool1", *epoch, 1000 + (epoch - 1) * 40),
                ("pool2", *epoch, 1100 + (epoch - 1) * 45),
                ("pool3", *epoch, 1200 - (epoch - 1) * 50),
            ]
        }) {
            let mut statement = connection.prepare(&query)?;
            statement
                .bind::<&[(_, Value)]>(&[
                    (1, pool_id.to_string().into()),
                    (2, Value::Integer(epoch)),
                    (3, Value::Integer(stake)),
                    (4, Utc::now().to_rfc3339().into()),
                ])
                .unwrap();
            statement.next().unwrap();
        }

        Ok(())
    }

    #[test]
    fn projection() {
        let projection = StakePool::get_projection();
        let aliases = SourceAlias::new(&[("{:stake_pool:}", "sp")]);

        assert_eq!(
            "sp.stake_pool_id as stake_pool_id, sp.stake as stake, sp.epoch as epoch, sp.created_at as created_at".to_string(),
            projection.expand(aliases)
        );
    }

    #[test]
    fn get_pool_by_epoch() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = StakePoolProvider::new(&connection);
        let condition = provider.condition_by_epoch(&Epoch(17)).unwrap();
        let (filter, values) = condition.expand();

        assert_eq!("epoch = ?1".to_string(), filter);
        assert_eq!(vec![Value::Integer(17)], values);
    }

    #[test]
    fn insert_or_replace_stake_pool() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = InsertOrReplaceStakePoolProvider::new(&connection);
        let condition = provider.get_insert_or_replace_condition("pool_id", Epoch(1), 1000);
        let (values, params) = condition.expand();

        assert_eq!(
            "(stake_pool_id, epoch, stake, created_at) values (?1, ?2, ?3, ?4)".to_string(),
            values
        );
        assert_eq!(
            vec![
                Value::String("pool_id".to_string()),
                Value::Integer(1),
                Value::Integer(1000),
                // Last params is the created_at date, since it's created by the condition itself
                // (using Utc::now()) we don't need to test it
                params.last().unwrap().clone()
            ],
            params
        );
    }

    #[test]
    fn prune() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = DeleteStakePoolProvider::new(&connection);
        let condition = provider.get_prune_condition(Epoch(5));
        let (condition, params) = condition.expand();

        assert_eq!("epoch < ?1".to_string(), condition);
        assert_eq!(vec![Value::Integer(5)], params);
    }

    #[test]
    fn test_get_stake_pools() {
        let connection = Connection::open(":memory:").unwrap();
        setup_stake_db(&connection, &[1, 2, 3]).unwrap();

        let provider = StakePoolProvider::new(&connection);
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
        let connection = Connection::open(":memory:").unwrap();
        setup_stake_db(&connection, &[3]).unwrap();

        let provider = InsertOrReplaceStakePoolProvider::new(&connection);
        let stake_pool = provider.persist("pool4", Epoch(3), 9999).unwrap();

        assert_eq!("pool4".to_string(), stake_pool.stake_pool_id);
        assert_eq!(Epoch(3), stake_pool.epoch);
        assert_eq!(9999, stake_pool.stake);

        let provider = StakePoolProvider::new(&connection);
        let mut cursor = provider.get_by_epoch(&Epoch(3)).unwrap();
        let stake_pool = cursor.next().expect("Should have a stake pool 'pool4'.");

        assert_eq!("pool4".to_string(), stake_pool.stake_pool_id);
        assert_eq!(Epoch(3), stake_pool.epoch);
        assert_eq!(9999, stake_pool.stake);
        assert_eq!(3, cursor.count());
    }

    #[test]
    fn test_prune() {
        let connection = Connection::open(":memory:").unwrap();
        setup_stake_db(&connection, &[1, 2]).unwrap();

        let provider = DeleteStakePoolProvider::new(&connection);
        let cursor = provider.prune(Epoch(2)).unwrap();

        assert_eq!(3, cursor.count());

        let provider = StakePoolProvider::new(&connection);
        let cursor = provider.get_by_epoch(&Epoch(1)).unwrap();

        assert_eq!(0, cursor.count());

        let cursor = provider.get_by_epoch(&Epoch(2)).unwrap();

        assert_eq!(3, cursor.count());
    }

    #[tokio::test]
    async fn save_protocol_parameters_prune_older_epoch_settings() {
        let connection = Connection::open(":memory:").unwrap();
        const STAKE_POOL_PRUNE_EPOCH_THRESHOLD: u64 = 10;
        setup_stake_db(&connection, &[1, 2]).unwrap();
        let store = StakePoolStore::new(
            Arc::new(Mutex::new(connection)),
            Some(STAKE_POOL_PRUNE_EPOCH_THRESHOLD),
        );

        store
            .save_stakes(
                Epoch(2) + STAKE_POOL_PRUNE_EPOCH_THRESHOLD,
                StakeDistribution::from_iter([("pool1".to_string(), 100)]),
            )
            .await
            .expect("saving stakes should not fails");
        let epoch1_stakes = store.get_stakes(Epoch(1)).await.unwrap();
        let epoch2_stakes = store.get_stakes(Epoch(2)).await.unwrap();

        assert_eq!(
            None, epoch1_stakes,
            "Stakes at epoch 1 should have been pruned",
        );
        assert!(
            epoch2_stakes.is_some(),
            "Stakes at epoch 2 should still exist",
        );
    }
}
