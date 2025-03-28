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

/// A [MithrilSignerRegistrationMaster] supports signer registrations in a master aggregator
pub struct MithrilSignerRegistrationMaster {
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

impl MithrilSignerRegistrationMaster {
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
impl SignerRegistrationRoundOpener for MithrilSignerRegistrationMaster {
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
impl EpochPruningTask for MithrilSignerRegistrationMaster {
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
impl SignerRegisterer for MithrilSignerRegistrationMaster {
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
            .map_err(|e| SignerRegistrationError::Store(anyhow!(e)))?
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
impl SignerSynchronizer for MithrilSignerRegistrationMaster {
    async fn can_synchronize_signers(
        &self,
        _epoch: Epoch,
    ) -> Result<bool, SignerRegistrationError> {
        Ok(false)
    }

    async fn synchronize_all_signers(&self) -> Result<(), SignerRegistrationError> {
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
        services::{
            EpochPruningTask, MockSignerRecorder, MockSignerRegistrationVerifier,
            SignerSynchronizer,
        },
        store::MockVerificationKeyStorer,
        MithrilSignerRegistrationMaster, SignerRegisterer, SignerRegistrationRoundOpener,
        VerificationKeyStorer,
    };

    use test_utils::*;

    use super::*;

    mod test_utils {
        use super::*;

        /// MithrilSignerRegistrationMasterBuilder is a test builder for [MithrilSignerRegistrationSlave]
        pub struct MithrilSignerRegistrationMasterBuilder {
            signer_recorder: Arc<dyn SignerRecorder>,
            signer_registration_verifier: Arc<dyn SignerRegistrationVerifier>,
            verification_key_store: Arc<dyn VerificationKeyStorer>,
            verification_key_epoch_retention_limit: Option<u64>,
        }

        impl Default for MithrilSignerRegistrationMasterBuilder {
            fn default() -> Self {
                Self {
                    signer_recorder: Arc::new(MockSignerRecorder::new()),
                    signer_registration_verifier: Arc::new(MockSignerRegistrationVerifier::new()),
                    verification_key_store: Arc::new(SignerRegistrationStore::new(Arc::new(
                        main_db_connection().unwrap(),
                    ))),
                    verification_key_epoch_retention_limit: None,
                }
            }
        }

        impl MithrilSignerRegistrationMasterBuilder {
            pub fn with_verification_key_store(
                self,
                verification_key_store: Arc<dyn VerificationKeyStorer>,
            ) -> Self {
                Self {
                    verification_key_store,
                    ..self
                }
            }

            pub fn with_signer_recorder(self, signer_recorder: Arc<dyn SignerRecorder>) -> Self {
                Self {
                    signer_recorder,
                    ..self
                }
            }

            pub fn with_signer_registration_verifier(
                self,
                signer_registration_verifier: Arc<dyn SignerRegistrationVerifier>,
            ) -> Self {
                Self {
                    signer_registration_verifier,
                    ..self
                }
            }

            pub fn with_verification_key_epoch_retention_limit(
                self,
                verification_key_epoch_retention_limit: Option<u64>,
            ) -> Self {
                Self {
                    verification_key_epoch_retention_limit,
                    ..self
                }
            }

            pub fn build(self) -> MithrilSignerRegistrationMaster {
                MithrilSignerRegistrationMaster {
                    current_round: RwLock::new(None),
                    verification_key_store: self.verification_key_store,
                    signer_recorder: self.signer_recorder,
                    signer_registration_verifier: self.signer_registration_verifier,
                    verification_key_epoch_retention_limit: self
                        .verification_key_epoch_retention_limit,
                }
            }
        }
    }

    #[tokio::test]
    async fn can_register_signer_if_registration_round_is_opened_with_operational_certificate() {
        let registration_epoch = Epoch(1);
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signer_to_register: Signer = fixture.signers()[0].to_owned();
        let stake_distribution = fixture.stake_distribution();
        let signer_registration_master = MithrilSignerRegistrationMasterBuilder::default()
            .with_signer_recorder({
                let mut signer_recorder = MockSignerRecorder::new();
                signer_recorder
                    .expect_record_signer_registration()
                    .returning(|_| Ok(()))
                    .once();

                Arc::new(signer_recorder)
            })
            .with_signer_registration_verifier({
                let mut signer_registration_verifier = MockSignerRegistrationVerifier::new();
                signer_registration_verifier
                    .expect_verify()
                    .returning(|signer, _| Ok(SignerWithStake::from_signer(signer.to_owned(), 123)))
                    .once();

                Arc::new(signer_registration_verifier)
            })
            .build();

        signer_registration_master
            .open_registration_round(registration_epoch, stake_distribution)
            .await
            .expect("signer registration round opening should not fail");

        signer_registration_master
            .register_signer(registration_epoch, &signer_to_register)
            .await
            .expect("signer registration should not fail");

        let registered_signers = &signer_registration_master
            .verification_key_store
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
        let registration_epoch = Epoch(1);
        let fixture = MithrilFixtureBuilder::default()
            .with_signers(5)
            .disable_signers_certification()
            .build();
        let signer_to_register: Signer = fixture.signers()[0].to_owned();
        let stake_distribution = fixture.stake_distribution();
        let signer_registration_master = MithrilSignerRegistrationMasterBuilder::default()
            .with_signer_recorder({
                let mut signer_recorder = MockSignerRecorder::new();
                signer_recorder
                    .expect_record_signer_registration()
                    .returning(|_| Ok(()))
                    .once();

                Arc::new(signer_recorder)
            })
            .with_signer_registration_verifier({
                let mut signer_registration_verifier = MockSignerRegistrationVerifier::new();
                signer_registration_verifier
                    .expect_verify()
                    .returning(|signer, _| Ok(SignerWithStake::from_signer(signer.to_owned(), 123)))
                    .once();

                Arc::new(signer_registration_verifier)
            })
            .build();

        signer_registration_master
            .open_registration_round(registration_epoch, stake_distribution)
            .await
            .expect("signer registration round opening should not fail");

        signer_registration_master
            .register_signer(registration_epoch, &signer_to_register)
            .await
            .expect("signer registration should not fail");

        let registered_signers = &signer_registration_master
            .verification_key_store
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
        let registration_epoch = Epoch(1);
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signer_to_register: Signer = fixture.signers()[0].to_owned();
        let signer_registration_master = MithrilSignerRegistrationMasterBuilder::default().build();

        signer_registration_master
            .register_signer(registration_epoch, &signer_to_register)
            .await
            .expect_err("signer registration should fail if no round opened");
    }

    #[tokio::test]
    async fn synchronize_all_signers_always_fails() {
        let signer_registration_master = MithrilSignerRegistrationMasterBuilder::default().build();

        signer_registration_master
            .synchronize_all_signers()
            .await
            .expect_err("synchronize_signers");
    }

    #[tokio::test]
    async fn prune_epoch_older_than_threshold() {
        const PROTOCOL_INITIALIZER_PRUNE_EPOCH_THRESHOLD: u64 = 10;
        let signer_registration_master = MithrilSignerRegistrationMasterBuilder::default()
            .with_verification_key_store({
                let mut verification_key_store = MockVerificationKeyStorer::new();
                verification_key_store
                    .expect_prune_verification_keys()
                    .with(eq(Epoch(4).offset_to_recording_epoch()))
                    .times(1)
                    .returning(|_| Ok(()));

                Arc::new(verification_key_store)
            })
            .with_verification_key_epoch_retention_limit(Some(
                PROTOCOL_INITIALIZER_PRUNE_EPOCH_THRESHOLD,
            ))
            .build();

        let current_epoch = Epoch(4) + PROTOCOL_INITIALIZER_PRUNE_EPOCH_THRESHOLD;
        signer_registration_master
            .prune(current_epoch)
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn without_threshold_nothing_is_pruned() {
        let signer_registration_master = MithrilSignerRegistrationMasterBuilder::default()
            .with_verification_key_store({
                let mut verification_key_store = MockVerificationKeyStorer::new();
                verification_key_store
                    .expect_prune_verification_keys()
                    .never();

                Arc::new(verification_key_store)
            })
            .with_verification_key_epoch_retention_limit(None)
            .build();

        signer_registration_master.prune(Epoch(100)).await.unwrap();
    }
}
