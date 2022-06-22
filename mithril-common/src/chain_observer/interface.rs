use crate::entities::*;
use async_trait::async_trait;
use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum ChainObserverError {}

#[async_trait]
pub trait ChainObserver: Sync + Send {
    /// Retrieve the current epoch of the Cardano network
    async fn get_current_epoch(&self) -> Result<Option<Epoch>, ChainObserverError>;

    /// Retrieve the current stake distribution of the Cardano network
    async fn get_current_stake_distribution(
        &self,
    ) -> Result<Option<StakeDistribution>, ChainObserverError>;
}
