use std::error::Error;

use chrono::NaiveDateTime;
use sqlite::{Connection, Value};

use mithril_common::{
    entities::{Epoch, PartyId},
    sqlite::{
        EntityCursor, HydrationError, Projection, Provider, SourceAlias, SqLiteEntity,
        WhereCondition,
    },
};

/// Stake pool as read from Chain.
pub struct StakePool {
    /// Pool Id
    stake_pool_id: PartyId,

    /// Total stake of this pool.
    stake: i64,

    /// Epoch at which this pool is valid.
    epoch: Epoch,

    /// DateTime of the record creation.
    created_at: NaiveDateTime,
}

impl SqLiteEntity for StakePool {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let epoch_int = row.get::<i64, _>(3);
        let datetime = &row.get::<String, _>(4);
        let stake_pool = Self {
            stake_pool_id: row.get::<String, _>(1),
            stake: row.get::<i64, _>(2),
            epoch: Epoch(epoch_int.try_into().map_err(|e| {
                HydrationError::InvalidData(format!("Could not cast i64 {} to u64.", epoch_int))
            })?),
            created_at: NaiveDateTime::parse_from_str(datetime, "%Y-%m-%d %H:%M:%S")
                .map_err(|e| HydrationError::InvalidData(format!("{e}")))?,
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

    fn condition_by_pool_id(&self, pool_ids: &[i64], epoch: &Epoch) -> WhereCondition {
        let pool_ids: Vec<Value> = pool_ids.iter().map(|&v| Value::Integer(v)).collect();
        let epoch: i64 = i64::try_from(epoch.0).unwrap();

        WhereCondition::where_in("stake_pool_id", pool_ids).and_where(WhereCondition::new(
            "epoch = ?*",
            vec![Value::Integer(epoch)],
        ))
    }

    /// Get StakePools for a given Epoch for given pool_ids.
    pub fn get_by_pool_ids(
        &self,
        pool_ids: &[i64],
        epoch: &Epoch,
    ) -> Result<EntityCursor<StakePool>, Box<dyn Error>> {
        let filters = self.condition_by_pool_id(pool_ids, epoch);
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

        format!("select {projection} from stake_distribution as sp where {condition}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn get_pool_by_ids() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = StakePoolProvider::new(&connection);
        let condition = provider.condition_by_pool_id(&[2, 3, 5], &Epoch(17));

        assert_eq!(
            "stake_pool_id in (?1, ?2, ?3) and epoch = ?4".to_string(),
            condition.expand().0
        );
    }
}
