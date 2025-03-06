use std::sync::Arc;

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use tokio::sync::RwLock;

use mithril_common::{
    entities::{Epoch, Signer, SignerWithStake, StakeDistribution},
    StdResult,
};

use crate::{services::EpochPruningTask, SignerRegistrationVerifier, VerificationKeyStorer};

use super::{
    SignerRecorder, SignerRegisterer, SignerRegistrationError, SignerRegistrationRound,
    SignerRegistrationRoundOpener, SignerSynchronizer,
};

/// Implementation of a [SignerRegisterer]
pub struct MithrilSignerRegistererMaster {
    /// Current signer registration round
    current_round: RwLock<Option<SignerRegistrationRound>>,

    /// Verification key store
    verification_key_store: Arc<dyn VerificationKeyStorer>,

    /// Signer recorder
    signer_recorder: Arc<dyn SignerRecorder>,

    /// Signer registration verifier
    signer_registration_verifier: Arc<dyn SignerRegistrationVerifier>,

    /// Number of epochs before previous records will be deleted at the next registration round
    /// opening
    verification_key_epoch_retention_limit: Option<u64>,
}

impl MithrilSignerRegistererMaster {
    /// MithrilSignerRegistererMaster factory
    pub fn new(
        verification_key_store: Arc<dyn VerificationKeyStorer>,
        signer_recorder: Arc<dyn SignerRecorder>,
        signer_registration_verifier: Arc<dyn SignerRegistrationVerifier>,
        verification_key_epoch_retention_limit: Option<u64>,
    ) -> Self {
        Self {
            current_round: RwLock::new(None),
            verification_key_store,
            signer_recorder,
            signer_registration_verifier,
            verification_key_epoch_retention_limit,
        }
    }

    #[cfg(test)]
    pub(crate) async fn get_current_round(&self) -> Option<SignerRegistrationRound> {
        self.current_round.read().await.as_ref().cloned()
    }
}

#[async_trait]
impl SignerRegistrationRoundOpener for MithrilSignerRegistererMaster {
    async fn open_registration_round(
        &self,
        registration_epoch: Epoch,
        stake_distribution: StakeDistribution,
    ) -> StdResult<()> {
        let mut current_round = self.current_round.write().await;
        *current_round = Some(SignerRegistrationRound {
            epoch: registration_epoch,
            stake_distribution,
        });

        Ok(())
    }

    async fn close_registration_round(&self) -> StdResult<()> {
        let mut current_round = self.current_round.write().await;
        *current_round = None;

        Ok(())
    }
}

#[async_trait]
impl EpochPruningTask for MithrilSignerRegistererMaster {
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

#[async_trait]
impl SignerRegisterer for MithrilSignerRegistererMaster {
    async fn register_signer(
        &self,
        epoch: Epoch,
        signer: &Signer,
    ) -> Result<SignerWithStake, SignerRegistrationError> {
        let registration_round = self.current_round.read().await;
        let registration_round = registration_round
            .as_ref()
            .ok_or(SignerRegistrationError::RegistrationRoundNotYetOpened)?;
        if registration_round.epoch != epoch {
            return Err(SignerRegistrationError::RegistrationRoundUnexpectedEpoch {
                current_round_epoch: registration_round.epoch,
                received_epoch: epoch,
            });
        }

        let signer_save = self
            .signer_registration_verifier
            .verify(signer, &registration_round.stake_distribution)
            .await
            .map_err(|e| SignerRegistrationError::FailedSignerRegistration(anyhow!(e)))?;

        self.signer_recorder
            .record_signer_registration(signer_save.party_id.clone())
            .await
            .map_err(|e| SignerRegistrationError::FailedSignerRecorder(e.to_string()))?;

        match self
            .verification_key_store
            .save_verification_key(registration_round.epoch, signer_save.clone())
            .await
            .with_context(|| {
                format!(
                    "VerificationKeyStorer can not save verification keys for party_id: '{}' for epoch: '{}'",
                    signer_save.party_id,
                    registration_round.epoch
                )
            })
            .map_err(|e| SignerRegistrationError::StoreError(anyhow!(e)))?
        {
            Some(_) => Err(SignerRegistrationError::ExistingSigner(Box::new(
                signer_save,
            ))),
            None => Ok(signer_save),
        }
    }

    async fn get_current_round(&self) -> Option<SignerRegistrationRound> {
        self.current_round.read().await.as_ref().cloned()
    }
}

#[async_trait]
impl SignerSynchronizer for MithrilSignerRegistererMaster {
    async fn synchronize_signers(
        &self,
        _epoch: Epoch,
        _signers: &[Signer],
        _stake_distribution: &StakeDistribution,
    ) -> Result<(), SignerRegistrationError> {
        Err(SignerRegistrationError::SignerSynchronizationUnavailableOnMasterAggregator)
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, sync::Arc};

    use mockall::predicate::eq;

    use mithril_common::{
        entities::{Epoch, Signer, SignerWithStake},
        test_utils::MithrilFixtureBuilder,
    };

    use crate::{
        database::{repository::SignerRegistrationStore, test_helper::main_db_connection},
        services::EpochPruningTask,
        signer_registration::{
            MockSignerRecorder, MockSignerRegistrationVerifier, SignerSynchronizer,
        },
        store::MockVerificationKeyStorer,
        MithrilSignerRegistererMaster, SignerRegisterer, SignerRegistrationRoundOpener,
        VerificationKeyStorer,
    };

    #[tokio::test]
    async fn can_register_signer_if_registration_round_is_opened_with_operational_certificate() {
        let verification_key_store = Arc::new(SignerRegistrationStore::new(Arc::new(
            main_db_connection().unwrap(),
        )));

        let mut signer_recorder = MockSignerRecorder::new();
        signer_recorder
            .expect_record_signer_registration()
            .returning(|_| Ok(()))
            .once();
        let mut signer_registration_verifier = MockSignerRegistrationVerifier::new();
        signer_registration_verifier
            .expect_verify()
            .returning(|signer, _| Ok(SignerWithStake::from_signer(signer.to_owned(), 123)))
            .once();
        let signer_registerer = MithrilSignerRegistererMaster::new(
            verification_key_store.clone(),
            Arc::new(signer_recorder),
            Arc::new(signer_registration_verifier),
            None,
        );
        let registration_epoch = Epoch(1);
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signer_to_register: Signer = fixture.signers()[0].to_owned();
        let stake_distribution = fixture.stake_distribution();

        signer_registerer
            .open_registration_round(registration_epoch, stake_distribution)
            .await
            .expect("signer registration round opening should not fail");

        signer_registerer
            .register_signer(registration_epoch, &signer_to_register)
            .await
            .expect("signer registration should not fail");

        let registered_signers = &verification_key_store
            .get_verification_keys(registration_epoch)
            .await
            .expect("registered signers retrieval should not fail");

        assert_eq!(
            &Some(HashMap::from([(
                signer_to_register.party_id.clone(),
                signer_to_register
            )])),
            registered_signers
        );
    }

    #[tokio::test]
    async fn can_register_signer_if_registration_round_is_opened_without_operational_certificate() {
        let verification_key_store = Arc::new(SignerRegistrationStore::new(Arc::new(
            main_db_connection().unwrap(),
        )));

        let mut signer_recorder = MockSignerRecorder::new();
        signer_recorder
            .expect_record_signer_registration()
            .returning(|_| Ok(()))
            .once();
        let mut signer_registration_verifier = MockSignerRegistrationVerifier::new();
        signer_registration_verifier
            .expect_verify()
            .returning(|signer, _| Ok(SignerWithStake::from_signer(signer.to_owned(), 123)))
            .once();
        let signer_registerer = MithrilSignerRegistererMaster::new(
            verification_key_store.clone(),
            Arc::new(signer_recorder),
            Arc::new(signer_registration_verifier),
            None,
        );
        let registration_epoch = Epoch(1);
        let fixture = MithrilFixtureBuilder::default()
            .with_signers(5)
            .disable_signers_certification()
            .build();
        let signer_to_register: Signer = fixture.signers()[0].to_owned();
        let stake_distribution = fixture.stake_distribution();

        signer_registerer
            .open_registration_round(registration_epoch, stake_distribution)
            .await
            .expect("signer registration round opening should not fail");

        signer_registerer
            .register_signer(registration_epoch, &signer_to_register)
            .await
            .expect("signer registration should not fail");

        let registered_signers = &verification_key_store
            .get_verification_keys(registration_epoch)
            .await
            .expect("registered signers retrieval should not fail");

        assert_eq!(
            &Some(HashMap::from([(
                signer_to_register.party_id.clone(),
                signer_to_register
            )])),
            registered_signers
        );
    }

    #[tokio::test]
    async fn cant_register_signer_if_registration_round_is_not_opened() {
        let verification_key_store = Arc::new(SignerRegistrationStore::new(Arc::new(
            main_db_connection().unwrap(),
        )));

        let signer_recorder = MockSignerRecorder::new();
        let signer_registration_verifier = MockSignerRegistrationVerifier::new();
        let signer_registerer = MithrilSignerRegistererMaster::new(
            verification_key_store.clone(),
            Arc::new(signer_recorder),
            Arc::new(signer_registration_verifier),
            None,
        );
        let registration_epoch = Epoch(1);
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signer_to_register: Signer = fixture.signers()[0].to_owned();

        signer_registerer
            .register_signer(registration_epoch, &signer_to_register)
            .await
            .expect_err("signer registration should fail if no round opened");
    }

    #[tokio::test]
    async fn synchronize_signers_always_fails() {
        let verification_key_store = Arc::new(SignerRegistrationStore::new(Arc::new(
            main_db_connection().unwrap(),
        )));

        let signer_recorder = MockSignerRecorder::new();
        let signer_registration_verifier = MockSignerRegistrationVerifier::new();
        let signer_registerer = MithrilSignerRegistererMaster::new(
            verification_key_store.clone(),
            Arc::new(signer_recorder),
            Arc::new(signer_registration_verifier),
            None,
        );
        let registration_epoch = Epoch(1);
        let fixture = MithrilFixtureBuilder::default()
            .with_signers(5)
            .disable_signers_certification()
            .build();
        let signers = fixture.signers();
        let stake_distribution = fixture.stake_distribution();

        signer_registerer
            .synchronize_signers(registration_epoch, &signers, &stake_distribution)
            .await
            .expect_err("synchronize_signers");
    }

    #[tokio::test]
    async fn mock_prune_epoch_older_than_threshold() {
        const PROTOCOL_INITIALIZER_PRUNE_EPOCH_THRESHOLD: u64 = 10;
        let retention_limit = Some(PROTOCOL_INITIALIZER_PRUNE_EPOCH_THRESHOLD);

        let mut verification_key_store = MockVerificationKeyStorer::new();
        verification_key_store
            .expect_prune_verification_keys()
            .with(eq(Epoch(4).offset_to_recording_epoch()))
            .times(1)
            .returning(|_| Ok(()));
        let signer_registration_verifier = MockSignerRegistrationVerifier::new();

        let signer_registerer = MithrilSignerRegistererMaster::new(
            Arc::new(verification_key_store),
            Arc::new(MockSignerRecorder::new()),
            Arc::new(signer_registration_verifier),
            retention_limit,
        );

        let current_epoch = Epoch(4) + PROTOCOL_INITIALIZER_PRUNE_EPOCH_THRESHOLD;
        signer_registerer.prune(current_epoch).await.unwrap();
    }

    #[tokio::test]
    async fn mock_without_threshold_nothing_is_pruned() {
        let retention_limit = None;

        let mut verification_key_store = MockVerificationKeyStorer::new();
        verification_key_store
            .expect_prune_verification_keys()
            .never();
        let signer_registration_verifier = MockSignerRegistrationVerifier::new();

        let signer_registerer = MithrilSignerRegistererMaster::new(
            Arc::new(verification_key_store),
            Arc::new(MockSignerRecorder::new()),
            Arc::new(signer_registration_verifier),
            retention_limit,
        );

        signer_registerer.prune(Epoch(100)).await.unwrap();
    }
}
