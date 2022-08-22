use async_trait::async_trait;
use std::sync::Arc;
use thiserror::Error;

use crate::chain_observer::ChainObserverError;
use crate::{
    chain_observer::ChainObserver, digesters::ImmutableFileObserver,
    digesters::ImmutableFileObserverError, entities::Beacon, CardanoNetwork,
};

/// Provide the current [Beacon] of a cardano node.
#[async_trait]
pub trait BeaconProvider
where
    Self: Sync + Send,
{
    /// Get the current [Beacon] of the cardano node.
    async fn get_current_beacon(&self) -> Result<Beacon, BeaconProviderError>;
}

/// [BeaconProvider] related errors.
#[derive(Error, Debug)]
pub enum BeaconProviderError {
    /// Raised when reading the current epoch with a [ChainObserver] fails.
    #[error("Could not get Epoch: {0}")]
    ChainObserver(#[from] ChainObserverError),

    /// Raised reading the current epoch succeeded but yield no result.
    #[error("No epoch yield by the chain observer, is your cardano node ready ?")]
    NoEpoch(),

    /// Raised when [getting the last immutable file number][ImmutableFileObserver::get_last_immutable_number]
    /// fails.
    #[error("Could not get last immutable file number: {0}")]
    ImmutableFileObserver(#[from] ImmutableFileObserverError),
}

/// A [BeaconProvider] using a [ChainObserver] and a [ImmutableFileObserver].
pub struct BeaconProviderImpl {
    chain_observer: Arc<dyn ChainObserver>,
    immutable_observer: Arc<dyn ImmutableFileObserver>,
    network: CardanoNetwork,
}

impl BeaconProviderImpl {
    /// [BeaconProviderImpl] factory.
    pub fn new(
        chain_observer: Arc<dyn ChainObserver>,
        immutable_observer: Arc<dyn ImmutableFileObserver>,
        network: CardanoNetwork,
    ) -> Self {
        Self {
            chain_observer,
            immutable_observer,
            network,
        }
    }
}

#[async_trait]
impl BeaconProvider for BeaconProviderImpl {
    async fn get_current_beacon(&self) -> Result<Beacon, BeaconProviderError> {
        let epoch = self
            .chain_observer
            .get_current_epoch()
            .await?
            .ok_or(BeaconProviderError::NoEpoch())?;
        let immutable_file_number = self.immutable_observer.get_last_immutable_number().await?;

        let beacon = Beacon {
            network: self.network.to_string(),
            epoch,
            immutable_file_number,
        };

        Ok(beacon)
    }
}

#[cfg(test)]
mod tests {
    use crate::chain_observer::{ChainObserver, ChainObserverError};
    use crate::digesters::DumbImmutableFileObserver;
    use crate::entities::{Epoch, StakeDistribution};

    use super::*;

    struct DumbChainObserver {}

    #[async_trait]
    impl ChainObserver for DumbChainObserver {
        async fn get_current_epoch(&self) -> Result<Option<Epoch>, ChainObserverError> {
            Ok(Some(Epoch(42)))
        }

        async fn get_current_stake_distribution(
            &self,
        ) -> Result<Option<StakeDistribution>, ChainObserverError> {
            Err(ChainObserverError::General(
                "this should not be called in the BeaconProvider"
                    .to_string()
                    .into(),
            ))
        }
    }

    #[tokio::test]
    async fn test_beacon_ok() {
        let beacon_provider = BeaconProviderImpl::new(
            Arc::new(DumbChainObserver {}),
            Arc::new(DumbImmutableFileObserver::default()),
            CardanoNetwork::TestNet(42),
        );
        let beacon = beacon_provider.get_current_beacon().await.unwrap();

        assert_eq!(42, beacon.epoch);
        assert_eq!(500, beacon.immutable_file_number);
    }

    #[tokio::test]
    async fn test_beacon_error() {
        let immutable_observer = DumbImmutableFileObserver::default();
        immutable_observer.shall_return(None).await;
        let beacon_provider = BeaconProviderImpl::new(
            Arc::new(DumbChainObserver {}),
            Arc::new(immutable_observer),
            CardanoNetwork::TestNet(42),
        );

        let result = beacon_provider.get_current_beacon().await;
        assert!(result.is_err());
    }
}
