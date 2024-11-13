use async_trait::async_trait;
use mithril_common::{
    entities::{Epoch, StakeDistribution},
    StdResult,
};

/// Represent a way to store the stake of mithril party members.
#[async_trait]
pub trait StakeStorer: Sync + Send {
    /// Save the stakes in the store for a given `epoch`.
    async fn save_stakes(
        &self,
        epoch: Epoch,
        stakes: StakeDistribution,
    ) -> StdResult<Option<StakeDistribution>>;

    /// Get the stakes of all party at a given `epoch`.
    async fn get_stakes(&self, epoch: Epoch) -> StdResult<Option<StakeDistribution>>;
}
