use anyhow::anyhow;
use anyhow::Context;
use async_trait::async_trait;
use std::sync::Arc;
use thiserror::Error;

use crate::StdResult;
use crate::{chain_observer::ChainObserver, digesters::ImmutableFileObserver, entities::TimePoint};

/// Provide the current [TimePoint] of a cardano node.
#[async_trait]
pub trait TimePointProvider
where
    Self: Sync + Send,
{
    /// Get the current [TimePoint] of the cardano node.
    async fn get_current_time_point(&self) -> StdResult<TimePoint>;
}

/// [TimePointProvider] related errors.
#[derive(Error, Debug)]
pub enum TimePointProviderError {
    /// Raised reading the current epoch succeeded but yield no result.
    #[error("No epoch yield by the chain observer, is your cardano node ready ?")]
    NoEpoch,

    /// Raised reading the current chain point succeeded but yield no result.
    #[error("No chain point yield by the chain observer, is your cardano node ready ?")]
    NoChainPoint,
}

/// A [TimePointProvider] using a [ChainObserver] and a [ImmutableFileObserver].
pub struct TimePointProviderImpl {
    chain_observer: Arc<dyn ChainObserver>,
    immutable_observer: Arc<dyn ImmutableFileObserver>,
}

impl TimePointProviderImpl {
    /// [TimePointProviderImpl] factory.
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
impl TimePointProvider for TimePointProviderImpl {
    async fn get_current_time_point(&self) -> StdResult<TimePoint> {
        let epoch = self
            .chain_observer
            .get_current_epoch()
            .await
            .map_err(|e| anyhow!(e))
            .with_context(|| "TimePoint Provider can not get current epoch")?
            .ok_or(TimePointProviderError::NoEpoch)?;

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
            .ok_or(TimePointProviderError::NoChainPoint)?;

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
    async fn test_happy_path() {
        let time_point_provider = TimePointProviderImpl::new(
            Arc::new(DumbChainObserver {}),
            Arc::new(DumbImmutableFileObserver::default()),
        );
        let time_point = time_point_provider.get_current_time_point().await.unwrap();

        assert_eq!(
            TimePoint::new(
                42,
                500,
                ChainPoint {
                    slot_number: 800,
                    block_number: 51,
                    block_hash: "1b69b3202fbe500".to_string(),
                }
            ),
            time_point
        );
    }

    #[tokio::test]
    async fn test_error_from_dependency() {
        let immutable_observer = DumbImmutableFileObserver::default();
        immutable_observer.shall_return(None).await;
        let time_point_provider = TimePointProviderImpl::new(
            Arc::new(DumbChainObserver {}),
            Arc::new(immutable_observer),
        );

        let result = time_point_provider.get_current_time_point().await;
        assert!(result.is_err());
    }
}
