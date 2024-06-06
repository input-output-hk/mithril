//! ## Ticker Service
//!
//! This service read time information from the chain and helps create beacons
//! for every message types.

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use std::sync::Arc;
use thiserror::Error;

use crate::chain_observer::ChainObserver;
use crate::digesters::ImmutableFileObserver;
use crate::entities::{Epoch, TimePoint};
use crate::StdResult;

/// ## TickerService
///
/// This service is responsible for giving the right time information to other
/// services. It reads data either from the Chain or the filesystem to create
/// beacons for each message type.
#[async_trait]
pub trait TickerService
where
    Self: Sync + Send,
{
    /// Get the current [Epoch] of the cardano node.
    async fn get_current_epoch(&self) -> StdResult<Epoch> {
        self.get_current_time_point()
            .await
            .map(|time_point| time_point.epoch)
    }

    /// Get the current [TimePoint] of the cardano node.
    async fn get_current_time_point(&self) -> StdResult<TimePoint>;
}

/// [TickerService] related errors.
#[derive(Error, Debug)]
pub enum TickerServiceError {
    /// Raised when reading the current epoch succeeded but yielded no result.
    #[error("No epoch yielded by the chain observer, is your cardano node ready?")]
    NoEpoch,

    /// Raised when reading the current chain point succeeded but yielded no result.
    #[error("No chain point yielded by the chain observer, is your cardano node ready?")]
    NoChainPoint,
}

/// A [TickerService] using a [ChainObserver] and a [ImmutableFileObserver].
pub struct MithrilTickerService {
    chain_observer: Arc<dyn ChainObserver>,
    immutable_observer: Arc<dyn ImmutableFileObserver>,
}

impl MithrilTickerService {
    /// [MithrilTickerService] factory.
    pub fn new(
        chain_observer: Arc<dyn ChainObserver>,
        immutable_observer: Arc<dyn ImmutableFileObserver>,
    ) -> Self {
        Self {
            chain_observer,
            immutable_observer,
        }
    }
}

#[async_trait]
impl TickerService for MithrilTickerService {
    async fn get_current_time_point(&self) -> StdResult<TimePoint> {
        let epoch = self
            .chain_observer
            .get_current_epoch()
            .await
            .map_err(|e| anyhow!(e))
            .with_context(|| "TimePoint Provider can not get current epoch")?
            .ok_or(TickerServiceError::NoEpoch)?;

        let immutable_file_number = self
            .immutable_observer
            .get_last_immutable_number()
            .await
            .with_context(|| {
                format!(
                    "TimePoint Provider can not get last immutable file number for epoch: '{epoch}'"
                )
            })?;

        let chain_point = self
            .chain_observer
            .get_current_chain_point()
            .await
            .map_err(|e| anyhow!(e))
            .with_context(|| "TimePoint Provider can not get current chain point")?
            .ok_or(TickerServiceError::NoChainPoint)?;

        Ok(TimePoint {
            epoch,
            immutable_file_number,
            chain_point,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::chain_observer::{ChainAddress, ChainObserver, ChainObserverError, TxDatum};
    use crate::digesters::DumbImmutableFileObserver;
    use crate::entities::{ChainPoint, Epoch, StakeDistribution};
    use anyhow::anyhow;

    use super::*;

    struct DumbChainObserver {}

    #[async_trait]
    impl ChainObserver for DumbChainObserver {
        async fn get_current_datums(
            &self,
            _address: &ChainAddress,
        ) -> Result<Vec<TxDatum>, ChainObserverError> {
            Ok(Vec::new())
        }

        async fn get_current_epoch(&self) -> Result<Option<Epoch>, ChainObserverError> {
            Ok(Some(Epoch(42)))
        }

        async fn get_current_chain_point(&self) -> Result<Option<ChainPoint>, ChainObserverError> {
            Ok(Some(ChainPoint {
                slot_number: 800,
                block_number: 51,
                block_hash: "1b69b3202fbe500".to_string(),
            }))
        }

        async fn get_current_stake_distribution(
            &self,
        ) -> Result<Option<StakeDistribution>, ChainObserverError> {
            Err(ChainObserverError::General(anyhow!(
                "this should not be called in the TimePointProvider"
            )))
        }
    }

    #[tokio::test]
    async fn test_get_current_epoch() {
        let ticker_service = MithrilTickerService::new(
            Arc::new(DumbChainObserver {}),
            Arc::new(DumbImmutableFileObserver::default()),
        );
        let epoch = ticker_service.get_current_epoch().await.unwrap();

        assert_eq!(Epoch(42), epoch);
    }

    #[tokio::test]
    async fn test_happy_path() {
        let ticker_service = MithrilTickerService::new(
            Arc::new(DumbChainObserver {}),
            Arc::new(DumbImmutableFileObserver::default()),
        );
        let time_point = ticker_service.get_current_time_point().await.unwrap();

        assert_eq!(
            TimePoint::new(
                42,
                500,
                ChainPoint {
                    slot_number: 800,
                    block_number: 51,
                    block_hash: "1b69b3202fbe500".to_string(),
                },
            ),
            time_point
        );
    }

    #[tokio::test]
    async fn test_error_from_dependency() {
        let immutable_observer = DumbImmutableFileObserver::default();
        immutable_observer.shall_return(None).await;
        let ticker_service =
            MithrilTickerService::new(Arc::new(DumbChainObserver {}), Arc::new(immutable_observer));

        let result = ticker_service.get_current_time_point().await;
        assert!(result.is_err());
    }
}
