use std::sync::Arc;

use async_trait::async_trait;
use mithril_common::entities::Epoch;
use mithril_common::entities::Signer;
use mithril_persistence::store::StakeStorer;
use slog_scope::{debug, trace};
use thiserror::Error;

use mithril_common::entities::EpochSettings;
use mithril_common::entities::SignerWithStake;
use mithril_common::StdResult;

use crate::RunnerError;

/// Errors dedicated to the CertifierService.
#[derive(Debug, Error)]
pub enum EpochServiceError {
    // /// One of the data that is held for an epoch duration by the service was not available.
    // #[error("Epoch service could not obtain {1} for epoch {0}")]
    // UnavailableData(Epoch, String),
    /// Raised when service has not collected data at least once.
    #[error("Epoch service was not initialized, the function `inform_epoch` must be called first")]
    NotYetInitialized,
    // /// Raised when service has not computed data for its current epoch.
    // #[error(
    //     "No data computed for epoch {0}, the function `precompute_epoch_data` must be called first"
    // )]
    // NotYetComputed(Epoch),
}

/// Service that aggregates all data that don't change in a given epoch.
#[async_trait]
pub trait EpochService: Sync + Send {
    // TODO should we pass ean EpochSettings or the individual fields ?
    async fn inform_epoch_settings(&mut self, epoch_settings: &EpochSettings) -> StdResult<()>;

    /// Get signers with stake for the current epoch
    fn current_signers_with_stake(&self) -> StdResult<&Vec<SignerWithStake>>;

    /// Get signers with stake for the next epoch
    fn next_signers_with_stake(&self) -> StdResult<&Vec<SignerWithStake>>;
}

struct EpochData {
    // epoch: Epoch,
    current_signers_with_stake: Vec<SignerWithStake>,
    next_signers_with_stake: Vec<SignerWithStake>,
}

/// Implementation of the [epoch service][EpochService].
pub struct MithrilEpochService {
    stake_storer: Arc<dyn StakeStorer>,

    epoch_data: Option<EpochData>,
}

impl MithrilEpochService {
    // Create a new service instance
    pub fn new(stake_storer: Arc<dyn StakeStorer>) -> Self {
        Self {
            stake_storer,
            // TODO init EpochData with empty None
            epoch_data: None,
        }
    }

    // TODO do it in EpochService and store SignerWithStack
    // Inject stake_store in EpochService (epoch + current protocol parameters)
    async fn associate_signers_with_stake(
        &self,
        epoch: Epoch,
        signers: &[Signer],
    ) -> StdResult<Vec<SignerWithStake>> {
        debug!("EpochService: associate_signers_with_stake");

        let stakes = self
            .stake_storer
            .get_stakes(epoch)
            .await?
            .ok_or_else(|| RunnerError::NoValueError(format!("stakes at epoch {epoch}")))?;

        let mut signers_with_stake = vec![];

        for signer in signers {
            let stake = stakes
                .get(&*signer.party_id)
                .ok_or_else(|| RunnerError::NoStakeForSigner(signer.party_id.to_string()))?;

            signers_with_stake.push(SignerWithStake::new(
                signer.party_id.to_owned(),
                signer.verification_key.to_owned(),
                signer.verification_key_signature.to_owned(),
                signer.operational_certificate.to_owned(),
                signer.kes_period.to_owned(),
                *stake,
            ));
            trace!(
                " > associating signer_id {} with stake {}",
                signer.party_id,
                *stake
            );
        }

        Ok(signers_with_stake)
    }
    fn unwrap_data(&self) -> Result<&EpochData, EpochServiceError> {
        self.epoch_data
            .as_ref()
            .ok_or(EpochServiceError::NotYetInitialized)
    }
}

#[async_trait]
impl EpochService for MithrilEpochService {
    async fn inform_epoch_settings(&mut self, epoch_settings: &EpochSettings) -> StdResult<()> {
        debug!(
            "EpochService: register_epoch_settings: {:?}",
            epoch_settings
        );

        let current_signers_with_stake = self
            .associate_signers_with_stake(epoch_settings.epoch, &epoch_settings.current_signers)
            .await?;
        let next_signers_with_stake = self
            .associate_signers_with_stake(epoch_settings.epoch, &epoch_settings.next_signers)
            .await?;

        self.epoch_data = Some(EpochData {
            current_signers_with_stake,
            next_signers_with_stake,
        });

        Ok(())
    }

    fn current_signers_with_stake(&self) -> StdResult<&Vec<SignerWithStake>> {
        Ok(&self.unwrap_data()?.current_signers_with_stake)
    }

    fn next_signers_with_stake(&self) -> StdResult<&Vec<SignerWithStake>> {
        Ok(&self.unwrap_data()?.next_signers_with_stake)
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::BTreeMap, sync::Arc};

    use super::*;

    use mithril_common::{
        entities::{Epoch, EpochSettings, StakeDistribution},
        test_utils::fake_data,
    };
    use mithril_persistence::store::{adapter::DumbStoreAdapter, StakeStore, StakeStorer};

    #[tokio::test]
    async fn test_signers_with_stake_are_not_available_before_register_epoch_settings_was_call() {
        let epoch = Epoch(12);
        // Signers and stake distribution
        let signers = fake_data::signers(10);
        let stake_distribution: StakeDistribution = signers
            .iter()
            .enumerate()
            .map(|(i, signer)| (signer.party_id.clone(), (i + 1) as u64 * 100))
            .collect();

        // Init stake_store
        let stake_store = Arc::new(StakeStore::new(Box::new(DumbStoreAdapter::new()), None));
        stake_store
            .save_stakes(epoch, stake_distribution.clone())
            .await
            .expect("save_stakes should not fail");

        // Build service and register epoch settings
        let mut service = MithrilEpochService::new(stake_store);
        assert!(service.current_signers_with_stake().is_err());
        assert!(service.next_signers_with_stake().is_err());
    }

    #[tokio::test]
    async fn test_signers_with_stake_can_be_empty_in_epoch_settings() {
        let epoch = Epoch(12);

        let stake_distribution: StakeDistribution = BTreeMap::new();

        // Init stake_store
        let stake_store = Arc::new(StakeStore::new(Box::new(DumbStoreAdapter::new()), None));
        stake_store
            .save_stakes(epoch, stake_distribution)
            .await
            .expect("save_stakes should not fail");

        // Epoch settings
        let epoch_settings = fake_data::epoch_settings();
        let epoch_settings = EpochSettings {
            epoch,
            current_signers: vec![],
            next_signers: vec![],
            ..epoch_settings.clone()
        };

        // Build service and register epoch settings
        let mut service = MithrilEpochService::new(stake_store);

        service
            .inform_epoch_settings(&epoch_settings.clone())
            .await
            .unwrap();

        // Check current_signers
        let current_signers = service.current_signers_with_stake().unwrap();
        assert_eq!(0, current_signers.len());

        let next_signers = service.next_signers_with_stake().unwrap();
        assert_eq!(0, next_signers.len());
    }

    #[tokio::test]
    async fn test_signers_with_stake_are_available_after_register_epoch_settings_call() {
        let epoch = Epoch(12);
        // Signers and stake distribution
        let signers = fake_data::signers(10);
        let stake_distribution: StakeDistribution = signers
            .iter()
            .enumerate()
            .map(|(i, signer)| (signer.party_id.clone(), (i + 1) as u64 * 100))
            .collect();

        // Init stake_store
        let stake_store = Arc::new(StakeStore::new(Box::new(DumbStoreAdapter::new()), None));
        stake_store
            .save_stakes(epoch, stake_distribution.clone())
            .await
            .expect("save_stakes should not fail");

        // Epoch settings
        let epoch_settings = fake_data::epoch_settings();
        let epoch_settings = EpochSettings {
            epoch,
            current_signers: signers[2..5].to_vec(),
            next_signers: signers[3..7].to_vec(),
            ..epoch_settings.clone()
        };

        // Build service and register epoch settings
        let mut service = MithrilEpochService::new(stake_store);

        service
            .inform_epoch_settings(&epoch_settings.clone())
            .await
            .unwrap();

        // Check current_signers
        let current_signers = service.current_signers_with_stake().unwrap();
        let expected_current_signers = epoch_settings.current_signers.clone();
        assert_eq!(expected_current_signers.len(), current_signers.len());
        for signer in current_signers {
            assert_eq!(
                stake_distribution.get(&signer.party_id).unwrap(),
                &signer.stake
            );
        }

        // Check next_signers
        let next_signers = service.next_signers_with_stake().unwrap();
        let expected_next_signers = epoch_settings.next_signers.clone();
        assert_eq!(expected_next_signers.len(), next_signers.len());
        for signer in next_signers {
            assert_eq!(
                stake_distribution.get(&signer.party_id).unwrap(),
                &signer.stake
            );
        }

        // TODO check: epoch + current protocol parameters
    }

    // TODO check update of signer after register_epoch_settings call
    // TODO should we provide a "reset" function ?
}
