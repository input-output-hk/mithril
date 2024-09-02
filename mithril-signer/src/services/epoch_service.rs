use std::sync::Arc;

use async_trait::async_trait;
use mithril_common::entities::Epoch;
use mithril_common::entities::ProtocolParameters;
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
    /// Inform the service a new epoch has been detected, telling it to update its
    /// internal state for the new epoch.
    async fn inform_epoch_settings(&mut self, epoch_settings: EpochSettings) -> StdResult<()>;

    /// Get the current epoch for which the data stored in this service are computed.
    fn epoch_of_current_data(&self) -> StdResult<Epoch>;

    /// Get next protocol parameters used in next epoch (associated with the actual epoch)
    fn next_protocol_parameters(&self) -> StdResult<&ProtocolParameters>;

    /// Get signers for the current epoch
    fn current_signers(&self) -> StdResult<&Vec<Signer>>;

    /// Get signers for the next epoch
    fn next_signers(&self) -> StdResult<&Vec<Signer>>;

    /// Get signers with stake for the current epoch
    async fn current_signers_with_stake(&self) -> StdResult<Vec<SignerWithStake>>;

    /// Get signers with stake for the next epoch
    async fn next_signers_with_stake(&self) -> StdResult<Vec<SignerWithStake>>;
}

struct EpochData {
    epoch: Epoch,
    next_protocol_parameters: ProtocolParameters,
    current_signers: Vec<Signer>,
    next_signers: Vec<Signer>,
}

/// Implementation of the [epoch service][EpochService].
pub struct MithrilEpochService {
    stake_storer: Arc<dyn StakeStorer>,

    epoch_data: Option<EpochData>,
}

impl MithrilEpochService {
    /// Create a new service instance
    pub fn new(stake_storer: Arc<dyn StakeStorer>) -> Self {
        Self {
            stake_storer,
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
    async fn inform_epoch_settings(&mut self, epoch_settings: EpochSettings) -> StdResult<()> {
        debug!(
            "EpochService: register_epoch_settings: {:?}",
            epoch_settings
        );

        self.epoch_data = Some(EpochData {
            epoch: epoch_settings.epoch,
            next_protocol_parameters: epoch_settings.next_protocol_parameters,
            current_signers: epoch_settings.current_signers,
            next_signers: epoch_settings.next_signers,
        });

        Ok(())
    }

    fn epoch_of_current_data(&self) -> StdResult<Epoch> {
        Ok(self.unwrap_data()?.epoch)
    }

    fn next_protocol_parameters(&self) -> StdResult<&ProtocolParameters> {
        Ok(&self.unwrap_data()?.next_protocol_parameters)
    }

    fn current_signers(&self) -> StdResult<&Vec<Signer>> {
        Ok(&self.unwrap_data()?.current_signers)
    }

    fn next_signers(&self) -> StdResult<&Vec<Signer>> {
        Ok(&self.unwrap_data()?.next_signers)
    }

    async fn current_signers_with_stake(&self) -> StdResult<Vec<SignerWithStake>> {
        let current_epoch = self.epoch_of_current_data()?;
        let (retrieval_epoch, _next_retrieval_epoch) = (
            current_epoch.offset_to_signer_retrieval_epoch()?,
            current_epoch.offset_to_next_signer_retrieval_epoch(),
        );
        let current_signers = self.current_signers()?;
        self.associate_signers_with_stake(retrieval_epoch, &current_signers)
            .await
    }

    async fn next_signers_with_stake(&self) -> StdResult<Vec<SignerWithStake>> {
        let current_epoch = self.epoch_of_current_data()?;
        let (_retrieval_epoch, _next_retrieval_epoch) = (
            current_epoch.offset_to_signer_retrieval_epoch()?,
            current_epoch.offset_to_next_signer_retrieval_epoch(),
        );
        let next_signers = self.next_signers()?;
        self.associate_signers_with_stake(_next_retrieval_epoch, &next_signers)
            .await
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;

    use mithril_common::{
        entities::{Epoch, EpochSettings, StakeDistribution},
        test_utils::fake_data,
    };
    use mithril_persistence::store::{adapter::DumbStoreAdapter, StakeStore, StakeStorer};

    #[tokio::test]
    async fn test_signers_are_not_available_before_register_epoch_settings_was_call() {
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
        let service = MithrilEpochService::new(stake_store);
        assert!(service.current_signers().is_err());
        assert!(service.next_signers().is_err());
    }

    #[tokio::test]
    async fn test_signers_are_available_after_register_epoch_settings_call() {
        let epoch = Epoch(12);
        // Signers and stake distribution
        let signers = fake_data::signers(10);

        // Init stake_store
        let stake_store = Arc::new(StakeStore::new(Box::new(DumbStoreAdapter::new()), None));

        // Epoch settings
        let epoch_settings = EpochSettings {
            epoch,
            current_signers: signers[2..5].to_vec(),
            next_signers: signers[3..7].to_vec(),
            ..fake_data::epoch_settings().clone()
        };

        // Build service and register epoch settings
        let mut service = MithrilEpochService::new(stake_store);

        service
            .inform_epoch_settings(epoch_settings.clone())
            .await
            .unwrap();

        // Check current_signers
        let current_signers = service.current_signers().unwrap();
        let expected_current_signers = epoch_settings.current_signers.clone();
        assert_eq!(expected_current_signers, *current_signers);

        // Check next_signers
        let next_signers = service.next_signers().unwrap();
        let expected_next_signers = epoch_settings.next_signers.clone();
        assert_eq!(expected_next_signers, *next_signers);

        assert_eq!(
            epoch_settings.epoch,
            service.epoch_of_current_data().unwrap()
        );
        assert_eq!(
            epoch_settings.next_protocol_parameters,
            *service.next_protocol_parameters().unwrap()
        );
    }

    // TODO try to simplify this test
    #[tokio::test]
    async fn test_signers_with_stake_are_available_after_register_epoch_settings_call() {
        let epoch = Epoch(12);
        // Signers and stake distribution
        let signers = fake_data::signers(10);

        // Init stake_store
        let stake_store = Arc::new(StakeStore::new(Box::new(DumbStoreAdapter::new()), None));

        // Epoch settings
        let epoch_settings = fake_data::epoch_settings();
        let epoch_settings = EpochSettings {
            epoch,
            current_signers: signers[2..5].to_vec(),
            next_signers: signers[3..7].to_vec(),
            ..epoch_settings.clone()
        };
        // Build service and register epoch settings
        let mut service = MithrilEpochService::new(stake_store.clone());
        service
            .inform_epoch_settings(epoch_settings.clone())
            .await
            .unwrap();

        // Check current_signers
        let stake_distribution: StakeDistribution = signers
            .iter()
            .enumerate()
            .map(|(i, signer)| (signer.party_id.clone(), (i + 1) as u64 * 100))
            .collect();
        stake_store
            .save_stakes(
                epoch.offset_to_signer_retrieval_epoch().unwrap(),
                stake_distribution.clone(),
            )
            .await
            .expect("save_stakes should not fail");
        let current_signers = service.current_signers_with_stake().await.unwrap();
        let expected_current_signers = epoch_settings.current_signers.clone();
        assert_eq!(expected_current_signers.len(), current_signers.len());
        for signer in current_signers {
            assert_eq!(
                stake_distribution.get(&signer.party_id).unwrap(),
                &signer.stake
            );
        }

        // Check next_signers
        let next_stake_distribution: StakeDistribution = signers
            .iter()
            .enumerate()
            .map(|(i, signer)| (signer.party_id.clone(), (i + 1) as u64 * 1000))
            .collect();
        stake_store
            .save_stakes(
                epoch.offset_to_next_signer_retrieval_epoch(),
                next_stake_distribution.clone(),
            )
            .await
            .expect("save_stakes should not fail");
        let next_signers = service.next_signers_with_stake().await.unwrap();
        let expected_next_signers = epoch_settings.next_signers.clone();
        assert_eq!(expected_next_signers.len(), next_signers.len());
        for signer in next_signers {
            assert_eq!(
                next_stake_distribution.get(&signer.party_id).unwrap(),
                &signer.stake
            );
        }
    }

    // TODO check update of signer after register_epoch_settings call
    // TODO should we provide a "reset" function ?
}
