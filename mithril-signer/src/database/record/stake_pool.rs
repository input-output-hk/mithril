use chrono::{DateTime, Utc};

use mithril_common::entities::{Epoch, PartyId};
use mithril_persistence::sqlite::{HydrationError, Projection, SqLiteEntity};

/// Stake pool as read from Chain.
#[derive(Debug, PartialEq)]
pub struct StakePool {
    /// Pool Id
    pub stake_pool_id: PartyId,

    /// Total stake of this pool.
    pub stake: u64,

    /// Epoch at which this pool is valid.
    pub epoch: Epoch,

    /// DateTime of the record creation.
    pub created_at: DateTime<Utc>,
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
                HydrationError::InvalidData(format!(
                    "Could not cast the StakePool.stake from internal db I64 → U64. Error: '{e}'.",
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
