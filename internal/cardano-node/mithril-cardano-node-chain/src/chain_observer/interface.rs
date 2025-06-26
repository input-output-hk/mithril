use async_trait::async_trait;
use thiserror::Error;

use mithril_common::crypto_helper::{KesPeriod, OpCert};
use mithril_common::entities::{ChainPoint, Epoch, StakeDistribution};
use mithril_common::StdError;

use crate::entities::{ChainAddress, TxDatum};

/// [ChainObserver] related errors.
#[derive(Debug, Error)]
pub enum ChainObserverError {
    /// Generic [ChainObserver] error.
    #[error("general error")]
    General(#[source] StdError),

    /// Error raised when the content could not be parsed.
    #[error("could not parse content")]
    InvalidContent(#[source] StdError),
}

/// Retrieve data from the cardano network
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait ChainObserver: Sync + Send {
    /// Retrieve the datums associated to an address
    async fn get_current_datums(
        &self,
        address: &ChainAddress,
    ) -> Result<Vec<TxDatum>, ChainObserverError>;

    /// Retrieve the current era of the Cardano network
    async fn get_current_era(&self) -> Result<Option<String>, ChainObserverError>;

    /// Retrieve the current epoch of the Cardano network
    async fn get_current_epoch(&self) -> Result<Option<Epoch>, ChainObserverError>;

    /// Retrieve the current chain point of the Cardano network
    async fn get_current_chain_point(&self) -> Result<Option<ChainPoint>, ChainObserverError>;

    /// Retrieve the current stake distribution of the Cardano network
    async fn get_current_stake_distribution(
        &self,
    ) -> Result<Option<StakeDistribution>, ChainObserverError>;

    /// Retrieve the KES period of an operational certificate
    async fn get_current_kes_period(
        &self,
        _opcert: &OpCert,
    ) -> Result<Option<KesPeriod>, ChainObserverError> {
        Ok(None)
    }
}
