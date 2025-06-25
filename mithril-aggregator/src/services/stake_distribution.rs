//! Stake Pool manager for the Runners
//!

use async_trait::async_trait;
use std::{
    fmt::Display,
    sync::{Arc, RwLock},
};
use tokio::sync::{Mutex, MutexGuard};

use mithril_cardano_node_chain::chain_observer::ChainObserver;
use mithril_common::{
    StdError, StdResult,
    entities::{Epoch, StakeDistribution},
};
use mithril_persistence::store::StakeStorer;

use crate::database::repository::StakePoolStore;

/// Errors related to the [StakeDistributionService].
#[derive(Debug)]
pub enum StakePoolDistributionServiceError {
    /// Critical errors cannot be recovered.
    Technical {
        /// Error message
        message: String,
        /// Eventual nested error
        error: Option<StdError>,
    },
    /// The stake distribution for the given Epoch is not available.
    Unavailable(Epoch),
    /// The stake distribution compute is in progress for this Epoch.
    Busy(Epoch),
}

impl StakePoolDistributionServiceError {
    /// Simple way to nest technical errors
    pub fn technical_subsystem(error: StdError) -> Box<Self> {
        Box::new(Self::Technical {
            message: "Stake pool service subsystem error occurred.".to_string(),
            error: Some(error),
        })
    }
}

impl TryFrom<StdError> for StakePoolDistributionServiceError {
    type Error = Box<Self>;

    fn try_from(value: StdError) -> Result<Self, Self::Error> {
        Err(Box::new(Self::Technical {
            message: "subsystem error".to_string(),
            error: Some(value),
        }))
    }
}

impl Display for StakePoolDistributionServiceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Technical { message, error } => {
                if let Some(nested_error) = error {
                    write!(
                        f,
                        "Critical error: {message} (nested error: '{nested_error}')"
                    )
                } else {
                    write!(f, "Critical error: {message}")
                }
            }
            Self::Unavailable(epoch) => {
                write!(
                    f,
                    "The stake distribution for epoch {epoch:?} is not available."
                )
            }
            Self::Busy(epoch) => {
                write!(
                    f,
                    "The stake distribution for epoch {epoch:?} is actually processed."
                )
            }
        }
    }
}

impl std::error::Error for StakePoolDistributionServiceError {}

/// Responsible of synchronizing with Cardano stake distribution.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait StakeDistributionService: Sync + Send {
    /// Return the stake distribution fot the given epoch.
    async fn get_stake_distribution(
        &self,
        epoch: Epoch,
    ) -> Result<StakeDistribution, Box<StakePoolDistributionServiceError>>;

    /// This launches the stake distribution computation if not already started.
    async fn update_stake_distribution(&self)
    -> Result<(), Box<StakePoolDistributionServiceError>>;
}

/// Token to manage stake distribution update
struct UpdateToken {
    /// Stake distribution update semaphore
    is_busy: Mutex<()>,
    /// Last computed stake distribution
    busy_on_epoch: RwLock<Epoch>,
}

impl Default for UpdateToken {
    fn default() -> Self {
        Self {
            is_busy: Mutex::new(()),
            busy_on_epoch: RwLock::new(Epoch(0)),
        }
    }
}

impl UpdateToken {
    pub fn update(&self, epoch: Epoch) -> StdResult<MutexGuard<()>> {
        let update_semaphore = self.is_busy.try_lock().map_err(|_| {
            let last_updated_epoch = self.busy_on_epoch.read().unwrap();

            StakePoolDistributionServiceError::Busy(*last_updated_epoch)
        })?;
        let mut last_updated_epoch = self.busy_on_epoch.write().unwrap();
        *last_updated_epoch = epoch;

        Ok(update_semaphore)
    }

    pub fn is_busy(&self) -> Option<Epoch> {
        if self.is_busy.try_lock().is_err() {
            Some(*self.busy_on_epoch.read().unwrap())
        } else {
            None
        }
    }
}
/// Implementation of the stake distribution service.
pub struct MithrilStakeDistributionService {
    /// internal stake persistent layer
    stake_store: Arc<StakePoolStore>,
    /// Chain interaction subsystem
    chain_observer: Arc<dyn ChainObserver>,
    /// Lock management for updates
    update_token: UpdateToken,
}

impl MithrilStakeDistributionService {
    /// Create a new service instance
    pub fn new(stake_store: Arc<StakePoolStore>, chain_observer: Arc<dyn ChainObserver>) -> Self {
        Self {
            stake_store,
            chain_observer,
            update_token: UpdateToken::default(),
        }
    }
}

#[async_trait]
impl StakeDistributionService for MithrilStakeDistributionService {
    async fn get_stake_distribution(
        &self,
        epoch: Epoch,
    ) -> Result<StakeDistribution, Box<StakePoolDistributionServiceError>> {
        let stake_distribution = self
            .stake_store
            .get_stakes(epoch)
            .await
            .map_err(StakePoolDistributionServiceError::technical_subsystem)?
            .unwrap_or_default();

        if !stake_distribution.is_empty() {
            Ok(stake_distribution)
        } else if let Some(last_epoch) = self.update_token.is_busy() {
            if last_epoch == epoch {
                Err(StakePoolDistributionServiceError::Busy(epoch).into())
            } else {
                Err(StakePoolDistributionServiceError::Unavailable(epoch).into())
            }
        } else {
            Err(StakePoolDistributionServiceError::Unavailable(epoch).into())
        }
    }

    async fn update_stake_distribution(
        &self,
    ) -> Result<(), Box<StakePoolDistributionServiceError>> {
        let current_epoch = self
            .chain_observer
            .get_current_epoch()
            .await
            .map_err(|e| StakePoolDistributionServiceError::technical_subsystem(e.into()))?
            .expect("Chain observer get_current_epoch should never return None.")
            .offset_to_recording_epoch();

        match self.get_stake_distribution(current_epoch).await {
            Ok(_) => return Ok(()),
            Err(e) if matches!(*e, StakePoolDistributionServiceError::Unavailable(_)) => (),
            Err(e) => return Err(e),
        };
        let _mutex = self
            .update_token
            .update(current_epoch)
            .map_err(StakePoolDistributionServiceError::technical_subsystem)?;
        let stake_distribution = self
            .chain_observer
            .get_current_stake_distribution()
            .await
            .map_err(|e| StakePoolDistributionServiceError::technical_subsystem(e.into()))?
            .expect("ChainObserver get_current_stake_distribution should never return None.");

        let _ = self
            .stake_store
            .save_stakes(current_epoch, stake_distribution)
            .await
            .map_err(StakePoolDistributionServiceError::technical_subsystem)?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::temp_dir;

    use crate::dependency_injection::DependenciesBuilder;
    use crate::tools::mocks::MockChainObserver;

    use super::*;

    async fn get_service(chain_observer: MockChainObserver) -> MithrilStakeDistributionService {
        let mut builder = DependenciesBuilder::new_with_stdout_logger(Arc::new(
            crate::ServeCommandConfiguration::new_sample(temp_dir!()),
        ));
        let stake_service = MithrilStakeDistributionService::new(
            builder.get_stake_store().await.unwrap(),
            Arc::new(chain_observer),
        );

        let store = builder.get_stake_store().await.unwrap();
        for (epoch, stake_distribution) in [
            (
                Epoch(1),
                [
                    ("pool1".to_string(), 1000),
                    ("pool2".to_string(), 1100),
                    ("pool3".to_string(), 1300),
                ],
            ),
            (
                Epoch(2),
                [
                    ("pool1".to_string(), 1230),
                    ("pool2".to_string(), 1090),
                    ("pool3".to_string(), 1300),
                ],
            ),
            (
                Epoch(3),
                [
                    ("pool1".to_string(), 1250),
                    ("pool2".to_string(), 1370),
                    ("pool3".to_string(), 1300),
                ],
            ),
        ] {
            store
                .save_stakes(epoch, StakeDistribution::from(stake_distribution))
                .await
                .unwrap();
        }

        stake_service
    }

    #[tokio::test]
    async fn get_current_stake_distribution() {
        let chain_observer = MockChainObserver::new();
        let service = get_service(chain_observer).await;
        let expected_stake_distribution: StakeDistribution =
            [("pool2", 1370), ("pool3", 1300), ("pool1", 1250)]
                .into_iter()
                .map(|(pool_id, stake)| (pool_id.to_string(), stake as u64))
                .collect();

        assert_eq!(
            expected_stake_distribution,
            service.get_stake_distribution(Epoch(3)).await.unwrap()
        );
    }

    #[tokio::test]
    async fn get_unavailable_stake_distribution() {
        let chain_observer = MockChainObserver::new();
        let service = get_service(chain_observer).await;
        let result = service.get_stake_distribution(Epoch(5)).await.unwrap_err();

        assert!(matches!(
            *result,
            StakePoolDistributionServiceError::Unavailable(Epoch(x)) if x == 5
        ));
    }

    #[tokio::test]
    async fn update_stake_distribution_ok() {
        let expected_stake_distribution = StakeDistribution::from_iter(
            [("pool1", 2000), ("pool2", 2000), ("pool3", 2000)]
                .into_iter()
                .map(|(p, s)| (p.to_string(), s as u64)),
        );
        let returned_stake_distribution = expected_stake_distribution.clone();
        let mut chain_observer = MockChainObserver::new();
        chain_observer
            .expect_get_current_epoch()
            .returning(|| Ok(Some(Epoch(3))));
        chain_observer
            .expect_get_current_stake_distribution()
            .return_once(|| Ok(Some(returned_stake_distribution)));
        let service = get_service(chain_observer).await;
        service.update_stake_distribution().await.unwrap();
        let sd = service.get_stake_distribution(Epoch(4)).await.unwrap();

        assert_eq!(expected_stake_distribution, sd);
    }

    #[tokio::test]
    async fn update_stake_distribution_already() {
        let mut chain_observer = MockChainObserver::new();
        chain_observer
            .expect_get_current_epoch()
            .returning(|| Ok(Some(Epoch(2))))
            .times(1);
        let service = get_service(chain_observer).await;
        service.update_stake_distribution().await.unwrap();
    }

    #[tokio::test]
    async fn get_not_ready_yet() {
        let mut chain_observer = MockChainObserver::new();
        chain_observer
            .expect_get_current_epoch()
            .returning(|| Ok(Some(Epoch(3))));
        let service = get_service(chain_observer).await;
        let _mutex = service.update_token.update(Epoch(4)).unwrap();
        let result = service.get_stake_distribution(Epoch(4)).await.unwrap_err();

        assert!(matches!(
            *result,
            StakePoolDistributionServiceError::Busy(Epoch(x)) if x == 4
        ));
    }

    #[tokio::test]
    async fn get_not_ready_but_unavailable() {
        let mut chain_observer = MockChainObserver::new();
        chain_observer
            .expect_get_current_epoch()
            .returning(|| Ok(Some(Epoch(3))));
        let service = get_service(chain_observer).await;
        let _mutex = service.update_token.update(Epoch(4)).unwrap();
        let result = service.get_stake_distribution(Epoch(0)).await.unwrap_err();

        assert!(matches!(
            *result,
            StakePoolDistributionServiceError::Unavailable(Epoch(x)) if x == 0
        ));
    }

    #[tokio::test]
    async fn update_but_busy() {
        let mut chain_observer = MockChainObserver::new();
        chain_observer
            .expect_get_current_epoch()
            .returning(|| Ok(Some(Epoch(3))));
        let service = get_service(chain_observer).await;
        let _mutex = service.update_token.update(Epoch(4)).unwrap();
        let result = service.update_stake_distribution().await.unwrap_err();

        assert!(matches!(
            *result,
            StakePoolDistributionServiceError::Busy(Epoch(x)) if x == 4
        ));
    }
}
