use crate::{
    crypto_helper::{KESPeriod, OpCert},
    entities::*,
};
use async_trait::async_trait;
use mockall::automock;
use std::error::Error as StdError;
use thiserror::Error;

/// [ChainObserver] related errors.
#[derive(Debug, Error)]
pub enum ChainObserverError {
    /// Generic [ChainObserver] error.
    #[error("general error {0}")]
    General(Box<dyn StdError + Sync + Send>),

    /// Error raised when the content could not be parsed.
    #[error("could not parse content: {0}")]
    InvalidContent(Box<dyn StdError + Sync + Send>),
}

/// Retrieve data from the cardano network
#[automock]
#[async_trait]
pub trait ChainObserver: Sync + Send + 'static {
    /// Retrieve the current epoch of the Cardano network
    async fn get_current_epoch(&self) -> Result<Option<Epoch>, ChainObserverError>;

    /// Retrieve the current stake distribution of the Cardano network
    async fn get_current_stake_distribution(
        &self,
    ) -> Result<Option<StakeDistribution>, ChainObserverError>;

    /// Retrieve the KES period of an operational certificate
    async fn get_current_kes_period(
        &self,
        _opcert: &OpCert,
    ) -> Result<Option<KESPeriod>, ChainObserverError> {
        Ok(None)
    }
}
