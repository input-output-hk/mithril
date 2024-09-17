use std::sync::Arc;

use async_trait::async_trait;
use mithril_common::crypto_helper::ProtocolInitializer;
use mithril_common::entities::CardanoTransactionsSigningConfig;
use mithril_common::entities::Epoch;
use mithril_common::entities::PartyId;
use mithril_common::entities::ProtocolParameters;
use mithril_common::entities::Signer;
use mithril_persistence::store::StakeStorer;
use slog_scope::{debug, trace};
use thiserror::Error;

use mithril_common::entities::EpochSettings;
use mithril_common::entities::SignerWithStake;
use mithril_common::StdResult;

use crate::RunnerError;

/// Errors dedicated to the EpochService.
#[derive(Debug, Error)]
pub enum EpochServiceError {
    /// Raised when service has not collected data at least once.
    #[error("Epoch service was not initialized, the function `inform_epoch_settings` must be called first")]
    NotYetInitialized,
}

/// Service that aggregates all data that don't change in a given epoch.
#[async_trait]
pub trait EpochService: Sync + Send {
    /// Inform the service a new epoch has been detected, telling it to update its
    /// internal state for the new epoch.
    fn inform_epoch_settings(&mut self, epoch_settings: EpochSettings) -> StdResult<()>;

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

    /// Get the cardano transactions signing configuration for the current epoch
    fn current_cardano_transactions_signing_config(
        &self,
    ) -> StdResult<&Option<CardanoTransactionsSigningConfig>>;

    /// Check if a signer is included in the current stake distribution
    fn is_signer_included_in_current_stake_distribution(
        &self,
        party_id: PartyId,
        protocol_initializer: ProtocolInitializer,
    ) -> StdResult<bool>;
}

struct EpochData {
    epoch: Epoch,
    next_protocol_parameters: ProtocolParameters,
    current_signers: Vec<Signer>,
    next_signers: Vec<Signer>,
    current_cardano_transactions_signing_config: Option<CardanoTransactionsSigningConfig>,
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
    fn inform_epoch_settings(&mut self, epoch_settings: EpochSettings) -> StdResult<()> {
        debug!(
            "EpochService: register_epoch_settings: {:?}",
            epoch_settings
        );

        self.epoch_data = Some(EpochData {
            epoch: epoch_settings.epoch,
            next_protocol_parameters: epoch_settings.next_protocol_parameters,
            current_signers: epoch_settings.current_signers,
            next_signers: epoch_settings.next_signers,
            current_cardano_transactions_signing_config: epoch_settings
                .current_cardano_transactions_signing_config,
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
        self.associate_signers_with_stake(
            current_epoch.offset_to_signer_retrieval_epoch()?,
            self.current_signers()?,
        )
        .await
    }

    async fn next_signers_with_stake(&self) -> StdResult<Vec<SignerWithStake>> {
        let current_epoch = self.epoch_of_current_data()?;
        self.associate_signers_with_stake(
            current_epoch.offset_to_next_signer_retrieval_epoch(),
            self.next_signers()?,
        )
        .await
    }

    fn current_cardano_transactions_signing_config(
        &self,
    ) -> StdResult<&Option<CardanoTransactionsSigningConfig>> {
        Ok(&self
            .unwrap_data()?
            .current_cardano_transactions_signing_config)
    }

    fn is_signer_included_in_current_stake_distribution(
        &self,
        party_id: PartyId,
        protocol_initializer: ProtocolInitializer,
    ) -> StdResult<bool> {
        Ok(self.current_signers()?.iter().any(|s| {
            s.party_id == party_id
                && s.verification_key == protocol_initializer.verification_key().into()
        }))
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::services::MithrilProtocolInitializerBuilder;

    use super::*;

    use mithril_common::{
        entities::{Epoch, EpochSettings, StakeDistribution},
        test_utils::{
            fake_data::{self},
            MithrilFixtureBuilder,
        },
    };
    use mithril_persistence::store::{
        adapter::{DumbStoreAdapter, MemoryAdapter},
        StakeStore, StakeStorer,
    };

    #[test]
    fn test_is_signer_included_in_current_stake_distribution_returns_error_when_epoch_settings_is_not_set(
    ) {
        let party_id = "party_id".to_string();
        let protocol_initializer = MithrilProtocolInitializerBuilder::build(
            &100,
            &fake_data::protocol_parameters(),
            None,
            None,
        )
        .unwrap();
        let stake_store = Arc::new(StakeStore::new(Box::new(DumbStoreAdapter::new()), None));
        let service = MithrilEpochService::new(stake_store);

        service
            .is_signer_included_in_current_stake_distribution(party_id, protocol_initializer)
            .expect_err("can_signer_sign should return error when epoch settings is not set");
    }

    #[test]
    fn test_is_signer_included_in_current_stake_distribution_returns_true_when_signer_verification_key_and_pool_id_found(
    ) {
        let fixtures = MithrilFixtureBuilder::default().with_signers(10).build();
        let protocol_initializer = fixtures.signers_fixture()[0]
            .protocol_initializer
            .to_owned();
        let epoch = Epoch(12);
        let signers = fixtures.signers();
        let stake_store = Arc::new(StakeStore::new(Box::new(DumbStoreAdapter::new()), None));
        let epoch_settings = EpochSettings {
            epoch,
            current_signers: signers[..5].to_vec(),
            ..fake_data::epoch_settings().clone()
        };

        let mut service = MithrilEpochService::new(stake_store);
        service
            .inform_epoch_settings(epoch_settings.clone())
            .unwrap();

        let party_id = fixtures.signers_fixture()[0].party_id();
        assert!(service
            .is_signer_included_in_current_stake_distribution(
                party_id.clone(),
                protocol_initializer.clone()
            )
            .unwrap());

        let party_id_not_included = fixtures.signers_fixture()[6].party_id();
        assert!(!service
            .is_signer_included_in_current_stake_distribution(
                party_id_not_included,
                protocol_initializer
            )
            .unwrap());

        let protocol_initializer_not_included = fixtures.signers_fixture()[6]
            .protocol_initializer
            .to_owned();
        assert!(!service
            .is_signer_included_in_current_stake_distribution(
                party_id,
                protocol_initializer_not_included
            )
            .unwrap());
    }

    #[tokio::test]
    async fn test_retrieve_data_return_error_before_register_epoch_settings_was_call() {
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
        assert!(service.epoch_of_current_data().is_err());
        assert!(service.next_protocol_parameters().is_err());
        assert!(service.current_signers().is_err());
        assert!(service.next_signers().is_err());
        assert!(service.current_signers_with_stake().await.is_err());
        assert!(service.next_signers_with_stake().await.is_err());
        assert!(service
            .current_cardano_transactions_signing_config()
            .is_err());
    }

    #[test]
    fn test_data_are_available_after_register_epoch_settings_call() {
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
            .unwrap();

        // Check current_signers
        {
            let current_signers = service.current_signers().unwrap();
            let expected_current_signers = epoch_settings.current_signers.clone();
            assert_eq!(expected_current_signers, *current_signers);
        }
        // Check next_signers
        {
            let next_signers = service.next_signers().unwrap();
            let expected_next_signers = epoch_settings.next_signers.clone();
            assert_eq!(expected_next_signers, *next_signers);
        }

        // Check other data
        assert_eq!(
            epoch_settings.epoch,
            service.epoch_of_current_data().unwrap()
        );
        assert_eq!(
            epoch_settings.next_protocol_parameters,
            *service.next_protocol_parameters().unwrap()
        );

        // Check current_cardano_transactions_signing_config
        assert_eq!(
            epoch_settings.current_cardano_transactions_signing_config,
            *service
                .current_cardano_transactions_signing_config()
                .unwrap()
        )
    }

    #[tokio::test]
    async fn test_signers_with_stake_are_available_after_register_epoch_settings_call() {
        fn build_stake_distribution(signers: &[Signer], first_stake: u64) -> StakeDistribution {
            signers
                .iter()
                .enumerate()
                .map(|(i, signer)| (signer.party_id.clone(), first_stake + i as u64))
                .collect()
        }

        let epoch = Epoch(12);

        // Signers and stake distribution
        let signers = fake_data::signers(10);
        let stake_distribution: StakeDistribution = build_stake_distribution(&signers, 100);
        let next_stake_distribution: StakeDistribution = build_stake_distribution(&signers, 500);

        let stake_store = Arc::new(StakeStore::new(
            Box::new(
                MemoryAdapter::<Epoch, StakeDistribution>::new(Some(vec![
                    (
                        epoch.offset_to_signer_retrieval_epoch().unwrap(),
                        stake_distribution.clone(),
                    ),
                    (
                        epoch.offset_to_next_signer_retrieval_epoch(),
                        next_stake_distribution.clone(),
                    ),
                ]))
                .unwrap(),
            ),
            None,
        ));

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
            .unwrap();

        // Check current signers with stake
        {
            let current_signers = service.current_signers_with_stake().await.unwrap();

            assert_eq!(epoch_settings.current_signers.len(), current_signers.len());
            for signer in current_signers {
                let expected_stake = stake_distribution.get(&signer.party_id).unwrap();
                assert_eq!(expected_stake, &signer.stake);
            }
        }

        // Check next signers with stake
        {
            let next_signers = service.next_signers_with_stake().await.unwrap();

            assert_eq!(epoch_settings.next_signers.len(), next_signers.len());
            for signer in next_signers {
                let expected_stake = next_stake_distribution.get(&signer.party_id).unwrap();
                assert_eq!(expected_stake, &signer.stake);
            }
        }
    }
}
