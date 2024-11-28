use anyhow::{anyhow, Context};
use async_trait::async_trait;
use std::sync::Arc;
use thiserror::Error;
use tokio::sync::RwLock;

use mithril_common::{
    chain_observer::ChainObserver,
    crypto_helper::{KESPeriod, ProtocolKeyRegistration},
    entities::{Epoch, Signer, SignerWithStake, StakeDistribution},
    StdError, StdResult,
};

use crate::{services::EpochPruningTask, VerificationKeyStorer};

use mithril_common::chain_observer::ChainObserverError;

/// Error type for signer registerer service.
#[derive(Error, Debug)]
pub enum SignerRegistrationError {
    /// No signer registration round opened yet
    #[error("a signer registration round is not opened yet, please try again later")]
    RegistrationRoundNotYetOpened,

    /// Registration round for unexpected epoch
    #[error("unexpected signer registration round epoch: current_round_epoch: {current_round_epoch}, received_epoch: {received_epoch}")]
    RegistrationRoundUnexpectedEpoch {
        /// Epoch of the current round
        current_round_epoch: Epoch,
        /// Epoch of the received signer registration
        received_epoch: Epoch,
    },

    /// Chain observer error.
    #[error("chain observer error")]
    ChainObserver(#[from] ChainObserverError),

    /// Signer is already registered.
    #[error("signer already registered")]
    ExistingSigner(Box<SignerWithStake>),

    /// Store error.
    #[error("store error")]
    StoreError(#[source] StdError),

    /// Signer registration failed.
    #[error("signer registration failed")]
    FailedSignerRegistration(#[source] StdError),

    /// Signer recorder failed.
    #[error("signer recorder failed: '{0}'")]
    FailedSignerRecorder(String),
}

/// Represents the information needed to handle a signer registration round
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignerRegistrationRound {
    /// Registration round epoch
    pub epoch: Epoch,

    stake_distribution: StakeDistribution,
}

#[cfg(test)]
impl SignerRegistrationRound {
    pub(crate) fn dummy(epoch: Epoch, stake_distribution: StakeDistribution) -> Self {
        Self {
            epoch,
            stake_distribution,
        }
    }
}

/// Trait to register a signer
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait SignerRegisterer: Sync + Send {
    /// Register a signer
    async fn register_signer(
        &self,
        epoch: Epoch,
        signer: &Signer,
    ) -> Result<SignerWithStake, SignerRegistrationError>;

    /// Get current open round if exists
    async fn get_current_round(&self) -> Option<SignerRegistrationRound>;
}

/// Trait to open a signer registration round
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait SignerRegistrationRoundOpener: Sync + Send {
    /// Open a signer registration round
    async fn open_registration_round(
        &self,
        registration_epoch: Epoch,
        stake_distribution: StakeDistribution,
    ) -> StdResult<()>;

    /// Close a signer registration round
    async fn close_registration_round(&self) -> StdResult<()>;
}

/// Signer recorder trait
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait SignerRecorder: Sync + Send {
    /// Record a signer registration
    async fn record_signer_registration(&self, signer_id: String) -> StdResult<()>;
}

/// Implementation of a [SignerRegisterer]
pub struct MithrilSignerRegisterer {
    /// Current signer registration round
    current_round: RwLock<Option<SignerRegistrationRound>>,

    /// Chain observer service.
    chain_observer: Arc<dyn ChainObserver>,

    /// Verification key store
    verification_key_store: Arc<dyn VerificationKeyStorer>,

    /// Signer recorder
    signer_recorder: Arc<dyn SignerRecorder>,

    /// Number of epochs before previous records will be deleted at the next registration round
    /// opening
    verification_key_epoch_retention_limit: Option<u64>,
}

impl MithrilSignerRegisterer {
    /// MithrilSignerRegisterer factory
    pub fn new(
        chain_observer: Arc<dyn ChainObserver>,
        verification_key_store: Arc<dyn VerificationKeyStorer>,
        signer_recorder: Arc<dyn SignerRecorder>,
        verification_key_epoch_retention_limit: Option<u64>,
    ) -> Self {
        Self {
            current_round: RwLock::new(None),
            chain_observer,
            verification_key_store,
            signer_recorder,
            verification_key_epoch_retention_limit,
        }
    }

    #[cfg(test)]
    pub(crate) async fn get_current_round(&self) -> Option<SignerRegistrationRound> {
        self.current_round.read().await.as_ref().cloned()
    }
}

#[async_trait]
impl SignerRegistrationRoundOpener for MithrilSignerRegisterer {
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
impl EpochPruningTask for MithrilSignerRegisterer {
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
impl SignerRegisterer for MithrilSignerRegisterer {
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

        let mut key_registration = ProtocolKeyRegistration::init(
            &registration_round
                .stake_distribution
                .iter()
                .map(|(k, v)| (k.to_owned(), *v))
                .collect::<Vec<_>>(),
        );
        let party_id_register = match signer.party_id.as_str() {
            "" => None,
            party_id => Some(party_id.to_string()),
        };
        let kes_period = match &signer.operational_certificate {
            Some(operational_certificate) => Some(
                self.chain_observer
                    .get_current_kes_period(operational_certificate)
                    .await?
                    .unwrap_or_default()
                    - operational_certificate.start_kes_period as KESPeriod,
            ),
            None => None,
        };
        let party_id_save = key_registration
            .register(
                party_id_register.clone(),
                signer.operational_certificate.clone(),
                signer.verification_key_signature,
                kes_period,
                signer.verification_key,
            )
            .with_context(|| {
                format!(
                    "KeyRegwrapper can not register signer with party_id: '{:?}'",
                    party_id_register
                )
            })
            .map_err(|e| SignerRegistrationError::FailedSignerRegistration(anyhow!(e)))?;
        let mut signer_save = SignerWithStake::from_signer(
            signer.to_owned(),
            *registration_round
                .stake_distribution
                .get(&party_id_save)
                .unwrap(),
        );
        signer_save.party_id.clone_from(&party_id_save);

        self.signer_recorder
            .record_signer_registration(party_id_save)
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

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, sync::Arc};

    use mithril_common::{
        chain_observer::FakeObserver,
        entities::{Epoch, Signer},
        test_utils::MithrilFixtureBuilder,
    };
    use mockall::predicate::eq;

    use crate::{
        database::{repository::SignerRegistrationStore, test_helper::main_db_connection},
        MithrilSignerRegisterer, SignerRegisterer, SignerRegistrationRoundOpener,
        VerificationKeyStorer,
    };
    use crate::{services::EpochPruningTask, store::MockVerificationKeyStorer};

    use super::MockSignerRecorder;

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
        let signer_registerer = MithrilSignerRegisterer::new(
            Arc::new(FakeObserver::default()),
            verification_key_store.clone(),
            Arc::new(signer_recorder),
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
        let signer_registerer = MithrilSignerRegisterer::new(
            Arc::new(FakeObserver::default()),
            verification_key_store.clone(),
            Arc::new(signer_recorder),
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
        let signer_registerer = MithrilSignerRegisterer::new(
            Arc::new(FakeObserver::default()),
            verification_key_store.clone(),
            Arc::new(signer_recorder),
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
    async fn mock_prune_epoch_older_than_threshold() {
        const PROTOCOL_INITIALIZER_PRUNE_EPOCH_THRESHOLD: u64 = 10;
        let retention_limit = Some(PROTOCOL_INITIALIZER_PRUNE_EPOCH_THRESHOLD);

        let mut verification_key_store = MockVerificationKeyStorer::new();
        verification_key_store
            .expect_prune_verification_keys()
            .with(eq(Epoch(4).offset_to_recording_epoch()))
            .times(1)
            .returning(|_| Ok(()));

        let signer_registerer = MithrilSignerRegisterer::new(
            Arc::new(FakeObserver::default()),
            Arc::new(verification_key_store),
            Arc::new(MockSignerRecorder::new()),
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

        let signer_registerer = MithrilSignerRegisterer::new(
            Arc::new(FakeObserver::default()),
            Arc::new(verification_key_store),
            Arc::new(MockSignerRecorder::new()),
            retention_limit,
        );

        signer_registerer.prune(Epoch(100)).await.unwrap();
    }
}
