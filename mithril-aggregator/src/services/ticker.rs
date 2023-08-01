//! ## Ticker Service
//!
//! This service read time information from the chain and helps create beacons
//! for every message types.

use std::sync::Arc;

use async_trait::async_trait;
use mithril_common::{
    chain_observer::ChainObserver,
    digesters::ImmutableFileObserver,
    entities::{Beacon, Epoch},
    CardanoNetwork, StdError,
};
use thiserror::Error;

type StdResult<T> = Result<T, StdError>;

#[derive(Debug, Error)]
enum MithrilTickerError {
    #[error("No Epoch information was returned by the ChainObserver.")]
    NoEpoch,
}

/// Service trait with consistent business oriented API.
#[async_trait]
pub trait TickerService: Send + Sync {
    /// Return the current Epoch as read from the chain.
    async fn get_current_epoch(&self) -> StdResult<Epoch>;

    /// Return the current Beacon used for CardanoImmutableFileDigest message type.
    async fn get_current_immutable_beacon(&self) -> StdResult<Beacon>;
}

/// ## MithrilTickerService
///
/// This service is responsible of giving the right time information to other
/// services. It reads data either from the Chain or the filesystem to create
/// beacons for each message type.
pub struct MithrilTickerService {
    chain_observer: Arc<dyn ChainObserver>,
    immutable_observer: Arc<dyn ImmutableFileObserver>,
    network: CardanoNetwork,
}

impl MithrilTickerService {
    /// Instantiate a new service
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
impl TickerService for MithrilTickerService {
    async fn get_current_epoch(&self) -> StdResult<Epoch> {
        let epoch = self
            .chain_observer
            .get_current_epoch()
            .await?
            .ok_or(MithrilTickerError::NoEpoch)?;

        Ok(epoch)
    }

    async fn get_current_immutable_beacon(&self) -> StdResult<Beacon> {
        let epoch = self.get_current_epoch().await?;
        let immutable_file_number = self.immutable_observer.get_last_immutable_number().await?;

        Ok(Beacon::new(
            self.network.to_string(),
            *epoch,
            immutable_file_number,
        ))
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{chain_observer::MockChainObserver, digesters::DumbImmutableFileObserver};

    use super::*;

    async fn get_ticker() -> MithrilTickerService {
        let mut chain_observer = MockChainObserver::new();
        chain_observer
            .expect_get_current_epoch()
            .returning(|| Ok(Some(Epoch(10))))
            .times(1);
        let immutable_observer = DumbImmutableFileObserver::new();
        immutable_observer.shall_return(Some(99)).await;
        let network = CardanoNetwork::DevNet(42);

        MithrilTickerService::new(
            Arc::new(chain_observer),
            Arc::new(immutable_observer),
            network,
        )
    }

    #[tokio::test]
    async fn get_epoch() {
        let ticker_service = get_ticker().await;
        let epoch = ticker_service.get_current_epoch().await.unwrap();

        assert_eq!(Epoch(10), epoch);
    }

    #[tokio::test]
    async fn get_immutable_beacon() {
        let ticker_service = get_ticker().await;
        let beacon = ticker_service.get_current_immutable_beacon().await.unwrap();

        assert_eq!(
            Beacon {
                epoch: Epoch(10),
                immutable_file_number: 99,
                network: "devnet".to_string()
            },
            beacon
        )
    }

    #[tokio::test]
    async fn no_beacon_error() {
        let mut chain_observer = MockChainObserver::new();
        chain_observer
            .expect_get_current_epoch()
            .returning(|| Ok(None))
            .times(1);
        let immutable_observer = DumbImmutableFileObserver::new();
        immutable_observer.shall_return(Some(99)).await;
        let network = CardanoNetwork::DevNet(42);

        let ticker_service = MithrilTickerService::new(
            Arc::new(chain_observer),
            Arc::new(immutable_observer),
            network,
        );
        let error = ticker_service.get_current_epoch().await.unwrap_err();
        let error = error
            .downcast_ref::<MithrilTickerError>()
            .expect("Expected error was a `MithrilTickerError`.");

        assert!(matches!(error, MithrilTickerError::NoEpoch));
    }
}
