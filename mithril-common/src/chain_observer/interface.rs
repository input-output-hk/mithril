use crate::entities::*;
use async_trait::async_trait;
use mockall::automock;
use std::error::Error as StdError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ChainObserverError {
    #[error("general error {0}")]
    General(Box<dyn StdError + Sync + Send>),
    #[error("could not parse content: {0}")]
    InvalidContent(Box<dyn StdError + Sync + Send>),
}

#[automock]
#[async_trait]
pub trait ChainObserver: Sync + Send {
    /// Retrieve the current epoch of the Cardano network
    async fn get_current_epoch(&self) -> Result<Option<Epoch>, ChainObserverError>;

    /// Retrieve the current stake distribution of the Cardano network
    async fn get_current_stake_distribution(
        &self,
    ) -> Result<Option<StakeDistribution>, ChainObserverError>;
}
