use std::sync::Arc;

use anyhow::{anyhow, Context};
use async_trait::async_trait;

use mithril_common::{
    entities::{Epoch, Signer, SignerWithStake, StakeDistribution},
    StdResult,
};
use mithril_persistence::store::StakeStorer;

use crate::{
    services::{AggregatorClient, EpochPruningTask},
    SignerRegistrationVerifier, VerificationKeyStorer,
};

use super::{
    SignerRecorder, SignerRegisterer, SignerRegistrationError, SignerRegistrationRound,
    SignerRegistrationRoundOpener, SignerSynchronizer,
};

/// A [MithrilSignerRegistrationSlave] supports signer registrations in a slave aggregator
pub struct MithrilSignerRegistrationSlave {
    /// Verification key store
    verification_key_store: Arc<dyn VerificationKeyStorer>,

    /// Signer recorder
    signer_recorder: Arc<dyn SignerRecorder>,

    /// Signer registration verifier
    signer_registration_verifier: Arc<dyn SignerRegistrationVerifier>,

    /// Master aggregator client
    master_aggregator_client: Arc<dyn AggregatorClient>,

    /// Stake store
    stake_store: Arc<dyn StakeStorer>,

    /// Number of epochs before previous records will be deleted at the next registration round
    /// opening
    verification_key_epoch_retention_limit: Option<u64>,
}

impl MithrilSignerRegistrationSlave {
    /// MithrilSignerRegistererSlave factory
    pub fn new(
        verification_key_store: Arc<dyn VerificationKeyStorer>,
        signer_recorder: Arc<dyn SignerRecorder>,
        signer_registration_verifier: Arc<dyn SignerRegistrationVerifier>,
        master_aggregator_client: Arc<dyn AggregatorClient>,
        stake_store: Arc<dyn StakeStorer>,
        verification_key_epoch_retention_limit: Option<u64>,
    ) -> Self {
        Self {
            verification_key_store,
            signer_recorder,
            signer_registration_verifier,
            master_aggregator_client,
            stake_store,
            verification_key_epoch_retention_limit,
        }
    }

    async fn synchronize_signers(
        &self,
        epoch: Epoch,
        signers: &[Signer],
        stake_distribution: &StakeDistribution,
    ) -> Result<(), SignerRegistrationError> {
        for signer in signers {
            let signer_with_stake = self
                .signer_registration_verifier
                .verify(signer, stake_distribution)
                .await
                .map_err(|e| SignerRegistrationError::FailedSignerRegistration(anyhow!(e)))?;

            self.signer_recorder
                .record_signer_registration(signer_with_stake.party_id.clone())
                .await
                .map_err(|e| SignerRegistrationError::FailedSignerRecorder(e.to_string()))?;

            self
                .verification_key_store
                .save_verification_key(epoch, signer_with_stake.clone())
                .await
                .with_context(|| {
                    format!(
                        "VerificationKeyStorer can not save verification keys for party_id: '{}' for epoch: '{}'",
                        signer_with_stake.party_id,
                        epoch
                    )
                })
                .map_err(|e| SignerRegistrationError::StoreError(anyhow!(e)))?;
        }

        Ok(())
    }
}

#[async_trait]
impl SignerSynchronizer for MithrilSignerRegistrationSlave {
    async fn can_synchronize_signers(&self, epoch: Epoch) -> Result<bool, SignerRegistrationError> {
        let is_slave_aggregator_at_same_epoch_as_master = if let Some(master_epoch_settings) = self
            .master_aggregator_client
            .retrieve_epoch_settings()
            .await
            .with_context(|| "can_synchronize_signers failed")
            .map_err(SignerRegistrationError::FailedFetchingMasterAggregatorEpochSettings)?
        {
            epoch == master_epoch_settings.epoch
        } else {
            false
        };

        Ok(is_slave_aggregator_at_same_epoch_as_master)
    }

    async fn synchronize_all_signers(&self) -> Result<(), SignerRegistrationError> {
        let master_epoch_settings = self
            .master_aggregator_client
            .retrieve_epoch_settings()
            .await
            .with_context(|| "synchronize_all_signers failed")
            .map_err(SignerRegistrationError::FailedFetchingMasterAggregatorEpochSettings)?
            .ok_or(
                SignerRegistrationError::FailedFetchingMasterAggregatorEpochSettings(
                    anyhow::anyhow!("Master aggregator did not return any epoch settings"),
                ),
            )?;
        let registration_epoch = master_epoch_settings
            .epoch
            .offset_to_master_synchronization_epoch();
        let next_signers = master_epoch_settings.next_signers;
        let stake_distribution = self
            .stake_store
            .get_stakes(registration_epoch)
            .await
            .with_context(|| "synchronize_all_signers failed")
            .map_err(SignerRegistrationError::StoreError)?
            .ok_or(SignerRegistrationError::StoreError(anyhow::anyhow!(
                "Slave aggregator did not return any stake distribution"
            )))?;
        self.synchronize_signers(registration_epoch, &next_signers, &stake_distribution)
            .await?;

        Ok(())
    }
}

#[async_trait]
impl SignerRegisterer for MithrilSignerRegistrationSlave {
    async fn register_signer(
        &self,
        _epoch: Epoch,
        _signer: &Signer,
    ) -> Result<SignerWithStake, SignerRegistrationError> {
        Err(SignerRegistrationError::RegistrationRoundAlwaysClosedOnSlaveAggregator)
    }

    async fn get_current_round(&self) -> Option<SignerRegistrationRound> {
        None
    }
}

#[async_trait]
impl SignerRegistrationRoundOpener for MithrilSignerRegistrationSlave {
    async fn open_registration_round(
        &self,
        _registration_epoch: Epoch,
        _stake_distribution: StakeDistribution,
    ) -> StdResult<()> {
        Ok(())
    }

    async fn close_registration_round(&self) -> StdResult<()> {
        Ok(())
    }
}

#[async_trait]
impl EpochPruningTask for MithrilSignerRegistrationSlave {
    fn pruned_data(&self) -> &'static str {
        "Signer registration"
    }

    async fn prune(&self, epoch: Epoch) -> StdResult<()> {
        let registration_epoch = epoch.offset_to_recording_epoch();

        if let Some(retention_limit) = self.verification_key_epoch_retention_limit {
            self.verification_key_store
                .prune_verification_keys(registration_epoch - retention_limit)
                .await
                .with_context(|| {
                    format!(
                        "VerificationKeyStorer can not prune verification keys below epoch: '{}'",
                        registration_epoch - retention_limit
                    )
                })?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use anyhow::anyhow;
    use mockall::predicate::eq;

    use mithril_common::{
        entities::{Epoch, Signer, SignerWithStake},
        messages::{EpochSettingsMessage, SignerMessagePart, TryFromMessageAdapter},
        test_utils::MithrilFixtureBuilder,
    };

    use crate::{
        database::{repository::SignerRegistrationStore, test_helper::main_db_connection},
        message_adapters::FromEpochSettingsAdapter,
        services::{AggregatorClientError, EpochPruningTask, MockAggregatorClient},
        signer_registration::{
            MockSignerRecorder, MockSignerRegistrationVerifier, SignerSynchronizer,
        },
        store::MockVerificationKeyStorer,
        tools::mocks::MockStakeStore,
        MithrilSignerRegistrationSlave, SignerRegisterer, SignerRegistrationRoundOpener,
    };

    #[tokio::test]
    async fn open_registration_always_succeeds() {
        let verification_key_store = Arc::new(SignerRegistrationStore::new(Arc::new(
            main_db_connection().unwrap(),
        )));
        let signer_registerer = MithrilSignerRegistrationSlave::new(
            verification_key_store.clone(),
            Arc::new(MockSignerRecorder::new()),
            Arc::new(MockSignerRegistrationVerifier::new()),
            Arc::new(MockAggregatorClient::new()),
            Arc::new(MockStakeStore::new()),
            None,
        );
        let registration_epoch = Epoch(1);
        let fixture = MithrilFixtureBuilder::default().with_signers(1).build();
        let stake_distribution = fixture.stake_distribution();

        signer_registerer
            .open_registration_round(registration_epoch, stake_distribution)
            .await
            .expect("signer registration round opening should not fail");
    }

    #[tokio::test]
    async fn close_registration_always_succeeds() {
        let verification_key_store = Arc::new(SignerRegistrationStore::new(Arc::new(
            main_db_connection().unwrap(),
        )));
        let signer_registerer = MithrilSignerRegistrationSlave::new(
            verification_key_store.clone(),
            Arc::new(MockSignerRecorder::new()),
            Arc::new(MockSignerRegistrationVerifier::new()),
            Arc::new(MockAggregatorClient::new()),
            Arc::new(MockStakeStore::new()),
            None,
        );

        signer_registerer
            .close_registration_round()
            .await
            .expect("signer registration round opening should not fail");
    }

    #[tokio::test]
    async fn register_signer_always_fails() {
        let verification_key_store = Arc::new(SignerRegistrationStore::new(Arc::new(
            main_db_connection().unwrap(),
        )));
        let signer_registerer = MithrilSignerRegistrationSlave::new(
            verification_key_store.clone(),
            Arc::new(MockSignerRecorder::new()),
            Arc::new(MockSignerRegistrationVerifier::new()),
            Arc::new(MockAggregatorClient::new()),
            Arc::new(MockStakeStore::new()),
            None,
        );
        let registration_epoch = Epoch(1);
        let fixture = MithrilFixtureBuilder::default().with_signers(1).build();
        let signer_to_register: Signer = fixture.signers()[0].to_owned();

        signer_registerer
            .register_signer(registration_epoch, &signer_to_register)
            .await
            .expect_err("signer registration should always fail");
    }

    #[tokio::test]
    async fn synchronize_all_signers_succeeds() {
        let verification_key_store = Arc::new(SignerRegistrationStore::new(Arc::new(
            main_db_connection().unwrap(),
        )));
        let registration_epoch = Epoch(1);
        let fixture = MithrilFixtureBuilder::default()
            .with_signers(5)
            .disable_signers_certification()
            .build();
        let signers = fixture.signers();
        let stake_distribution = fixture.stake_distribution();
        let epoch_settings_message = FromEpochSettingsAdapter::try_adapt(EpochSettingsMessage {
            epoch: registration_epoch,
            next_signers: SignerMessagePart::from_signers(signers),
            ..EpochSettingsMessage::dummy()
        })
        .unwrap();
        let mut signer_recorder = MockSignerRecorder::new();
        signer_recorder
            .expect_record_signer_registration()
            .returning(|_| Ok(()))
            .times(5);
        let mut signer_registration_verifier = MockSignerRegistrationVerifier::new();
        signer_registration_verifier
            .expect_verify()
            .returning(|signer, _| Ok(SignerWithStake::from_signer(signer.to_owned(), 123)))
            .times(5);
        let mut aggregator_client = MockAggregatorClient::new();
        aggregator_client
            .expect_retrieve_epoch_settings()
            .returning(move || Ok(Some(epoch_settings_message.clone())))
            .times(1);
        let mut stake_store = MockStakeStore::new();
        stake_store
            .expect_get_stakes()
            .returning(move |_epoch| Ok(Some(stake_distribution.clone())))
            .times(1);
        let signer_registerer = MithrilSignerRegistrationSlave::new(
            verification_key_store.clone(),
            Arc::new(signer_recorder),
            Arc::new(signer_registration_verifier),
            Arc::new(aggregator_client),
            Arc::new(stake_store),
            None,
        );

        signer_registerer.synchronize_all_signers().await.unwrap();
    }

    #[tokio::test]
    async fn synchronize_all_signers_fails_if_one_signer_registration_fails() {
        let verification_key_store = Arc::new(SignerRegistrationStore::new(Arc::new(
            main_db_connection().unwrap(),
        )));
        let registration_epoch = Epoch(1);
        let fixture = MithrilFixtureBuilder::default()
            .with_signers(5)
            .disable_signers_certification()
            .build();
        let signers = fixture.signers();
        let stake_distribution = fixture.stake_distribution();
        let epoch_settings_message = FromEpochSettingsAdapter::try_adapt(EpochSettingsMessage {
            epoch: registration_epoch,
            next_signers: SignerMessagePart::from_signers(signers),
            ..EpochSettingsMessage::dummy()
        })
        .unwrap();
        let mut signer_recorder = MockSignerRecorder::new();
        signer_recorder
            .expect_record_signer_registration()
            .returning(|_| Ok(()))
            .times(4);
        signer_recorder
            .expect_record_signer_registration()
            .returning(|_| Err(anyhow!("an error")))
            .times(1);
        let mut signer_registration_verifier = MockSignerRegistrationVerifier::new();
        signer_registration_verifier
            .expect_verify()
            .returning(|signer, _| Ok(SignerWithStake::from_signer(signer.to_owned(), 123)))
            .times(5);
        let mut aggregator_client = MockAggregatorClient::new();
        aggregator_client
            .expect_retrieve_epoch_settings()
            .returning(move || Ok(Some(epoch_settings_message.clone())))
            .times(1);
        let mut stake_store = MockStakeStore::new();
        stake_store
            .expect_get_stakes()
            .returning(move |_epoch| Ok(Some(stake_distribution.clone())))
            .times(1);
        let signer_registerer = MithrilSignerRegistrationSlave::new(
            verification_key_store.clone(),
            Arc::new(signer_recorder),
            Arc::new(signer_registration_verifier),
            Arc::new(aggregator_client),
            Arc::new(stake_store),
            None,
        );

        signer_registerer
            .synchronize_all_signers()
            .await
            .expect_err("synchronize_all_signers should fail");
    }

    #[tokio::test]
    async fn synchronize_all_signers_fails_if_fetching_epoch_settings_fails() {
        let verification_key_store = Arc::new(SignerRegistrationStore::new(Arc::new(
            main_db_connection().unwrap(),
        )));
        let signer_recorder = MockSignerRecorder::new();
        let signer_registration_verifier = MockSignerRegistrationVerifier::new();
        let mut aggregator_client = MockAggregatorClient::new();
        aggregator_client
            .expect_retrieve_epoch_settings()
            .returning(move || {
                Err(AggregatorClientError::RemoteServerTechnical(anyhow!(
                    "an error"
                )))
            })
            .times(1);
        let stake_store = MockStakeStore::new();
        let signer_registerer = MithrilSignerRegistrationSlave::new(
            verification_key_store.clone(),
            Arc::new(signer_recorder),
            Arc::new(signer_registration_verifier),
            Arc::new(aggregator_client),
            Arc::new(stake_store),
            None,
        );

        signer_registerer
            .synchronize_all_signers()
            .await
            .expect_err("synchronize_all_signers should fail");
    }

    #[tokio::test]
    async fn synchronize_all_signers_fails_if_fetching_stakes_fails() {
        let verification_key_store = Arc::new(SignerRegistrationStore::new(Arc::new(
            main_db_connection().unwrap(),
        )));
        let registration_epoch = Epoch(1);
        let fixture = MithrilFixtureBuilder::default()
            .with_signers(5)
            .disable_signers_certification()
            .build();
        let signers = fixture.signers();
        let epoch_settings_message = FromEpochSettingsAdapter::try_adapt(EpochSettingsMessage {
            epoch: registration_epoch,
            next_signers: SignerMessagePart::from_signers(signers),
            ..EpochSettingsMessage::dummy()
        })
        .unwrap();
        let signer_recorder = MockSignerRecorder::new();
        let signer_registration_verifier = MockSignerRegistrationVerifier::new();
        let mut aggregator_client = MockAggregatorClient::new();
        aggregator_client
            .expect_retrieve_epoch_settings()
            .returning(move || Ok(Some(epoch_settings_message.clone())))
            .times(1);
        let mut stake_store = MockStakeStore::new();
        stake_store
            .expect_get_stakes()
            .returning(move |_epoch| Err(anyhow!("an error")))
            .times(1);
        let signer_registerer = MithrilSignerRegistrationSlave::new(
            verification_key_store.clone(),
            Arc::new(signer_recorder),
            Arc::new(signer_registration_verifier),
            Arc::new(aggregator_client),
            Arc::new(stake_store),
            None,
        );

        signer_registerer
            .synchronize_all_signers()
            .await
            .expect_err("synchronize_all_signers should fail");
    }

    #[tokio::test]
    async fn prune_epoch_older_than_threshold() {
        const PROTOCOL_INITIALIZER_PRUNE_EPOCH_THRESHOLD: u64 = 10;
        let retention_limit = Some(PROTOCOL_INITIALIZER_PRUNE_EPOCH_THRESHOLD);

        let mut verification_key_store = MockVerificationKeyStorer::new();
        verification_key_store
            .expect_prune_verification_keys()
            .with(eq(Epoch(4).offset_to_recording_epoch()))
            .times(1)
            .returning(|_| Ok(()));
        let signer_registration_verifier = MockSignerRegistrationVerifier::new();

        let signer_registerer = MithrilSignerRegistrationSlave::new(
            Arc::new(verification_key_store),
            Arc::new(MockSignerRecorder::new()),
            Arc::new(signer_registration_verifier),
            Arc::new(MockAggregatorClient::new()),
            Arc::new(MockStakeStore::new()),
            retention_limit,
        );

        let current_epoch = Epoch(4) + PROTOCOL_INITIALIZER_PRUNE_EPOCH_THRESHOLD;
        signer_registerer.prune(current_epoch).await.unwrap();
    }

    #[tokio::test]
    async fn without_threshold_nothing_is_pruned() {
        let retention_limit = None;

        let mut verification_key_store = MockVerificationKeyStorer::new();
        verification_key_store
            .expect_prune_verification_keys()
            .never();
        let signer_registration_verifier = MockSignerRegistrationVerifier::new();

        let signer_registerer = MithrilSignerRegistrationSlave::new(
            Arc::new(verification_key_store),
            Arc::new(MockSignerRecorder::new()),
            Arc::new(signer_registration_verifier),
            Arc::new(MockAggregatorClient::new()),
            Arc::new(MockStakeStore::new()),
            retention_limit,
        );

        signer_registerer.prune(Epoch(100)).await.unwrap();
    }
}
