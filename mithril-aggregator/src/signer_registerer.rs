use async_trait::async_trait;
use std::sync::Arc;
use thiserror::Error;
use tokio::sync::RwLock;

use crate::{VerificationKeyStore, VerificationKeyStorer};
use mithril_common::{
    chain_observer::ChainObserver,
    crypto_helper::{
        key_decode_hex, KESPeriod, ProtocolKeyRegistration, ProtocolRegistrationError,
    },
    entities::{Epoch, Signer, StakeDistribution},
    store::StoreError,
};

#[cfg(test)]
use mockall::automock;

/// Error type for signer registerer service.
#[derive(Error, Debug)]
pub enum SignerRegistrationError {
    /// No signer registration round opened yet
    #[error("a signer registration round has not yet to be opened")]
    RegistrationRoundNotYetOpened,

    /// Codec error.
    #[error("codec error: '{0}'")]
    Codec(String),

    /// Chain observer error.
    #[error("chain observer error: '{0}'")]
    ChainObserver(String),

    /// Signer is already registered.
    #[error("signer already registered")]
    ExistingSigner(),

    /// Store error.
    #[error("store error: {0}")]
    StoreError(#[from] StoreError),

    /// Signer registration failed.
    #[error("signer registration failed")]
    FailedSignerRegistration(#[from] ProtocolRegistrationError),
}

/// Represents the information needed to handle a signer registration round
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignerRegistrationRound {
    epoch: Epoch,
    stake_distribution: StakeDistribution,
}

#[cfg(test)]
impl SignerRegistrationRound {
    pub fn dummy(epoch: Epoch, stake_distribution: StakeDistribution) -> Self {
        Self {
            epoch,
            stake_distribution,
        }
    }
}

/// Trait to register a signer
#[cfg_attr(test, automock)]
#[async_trait]
pub trait SignerRegisterer: Sync + Send {
    /// Register a signer
    async fn register_signer(&self, signer: &Signer) -> Result<(), SignerRegistrationError>;
}

/// Trait to open a signer registration round
#[cfg_attr(test, automock)]
#[async_trait]
pub trait SignerRegistrationRoundOpener: Sync + Send {
    /// Open a signer registration round
    async fn open_registration_round(
        &self,
        registration_epoch: Epoch,
        stake_distribution: StakeDistribution,
    ) -> Result<(), SignerRegistrationError>;
}

/// Implementation of a [SignerRegisterer]
pub struct MithrilSignerRegisterer {
    /// Current signer registration round
    current_round: RwLock<Option<SignerRegistrationRound>>,

    /// Chain observer service.
    chain_observer: Arc<dyn ChainObserver>,

    /// Verification key store
    verification_key_store: Arc<VerificationKeyStore>,
}

impl MithrilSignerRegisterer {
    /// MithrilSignerRegisterer factory
    pub fn new(
        chain_observer: Arc<dyn ChainObserver>,
        verification_key_store: Arc<VerificationKeyStore>,
    ) -> Self {
        Self {
            current_round: RwLock::new(None),
            chain_observer,
            verification_key_store,
        }
    }

    #[cfg(test)]
    pub async fn get_current_round(&self) -> Option<SignerRegistrationRound> {
        self.current_round.read().await.as_ref().cloned()
    }
}

#[async_trait]
impl SignerRegistrationRoundOpener for MithrilSignerRegisterer {
    async fn open_registration_round(
        &self,
        registration_epoch: Epoch,
        stake_distribution: StakeDistribution,
    ) -> Result<(), SignerRegistrationError> {
        let mut current_round = self.current_round.write().await;
        *current_round = Some(SignerRegistrationRound {
            epoch: registration_epoch,
            stake_distribution,
        });

        Ok(())
    }
}

#[async_trait]
impl SignerRegisterer for MithrilSignerRegisterer {
    async fn register_signer(&self, signer: &Signer) -> Result<(), SignerRegistrationError> {
        let registration_round = self.current_round.read().await;
        let registration_round = registration_round
            .as_ref()
            .ok_or(SignerRegistrationError::RegistrationRoundNotYetOpened)?;

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
        let verification_key =
            key_decode_hex(&signer.verification_key).map_err(SignerRegistrationError::Codec)?;
        let verification_key_signature = match &signer.verification_key_signature {
            Some(verification_key_signature) => Some(
                key_decode_hex(verification_key_signature)
                    .map_err(SignerRegistrationError::Codec)?,
            ),
            _ => None,
        };
        let operational_certificate = match &signer.operational_certificate {
            Some(operational_certificate) => Some(
                key_decode_hex(operational_certificate).map_err(SignerRegistrationError::Codec)?,
            ),
            _ => None,
        };
        let kes_period = match &operational_certificate {
            Some(operational_certificate) => Some(
                self.chain_observer
                    .get_current_kes_period(operational_certificate)
                    .await
                    .map_err(|e| SignerRegistrationError::ChainObserver(e.to_string()))?
                    .unwrap_or_default()
                    - operational_certificate.start_kes_period as KESPeriod,
            ),
            None => None,
        };
        let party_id_save = key_registration.register(
            party_id_register.clone(),
            operational_certificate,
            verification_key_signature,
            kes_period,
            verification_key,
        )?;
        let mut signer_save = signer.to_owned();
        signer_save.party_id = party_id_save;

        match self
            .verification_key_store
            .save_verification_key(registration_round.epoch, signer_save)
            .await?
        {
            Some(_) => Err(SignerRegistrationError::ExistingSigner()),
            None => Ok(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, sync::Arc};

    use mithril_common::{
        chain_observer::FakeObserver,
        entities::{Epoch, PartyId, Signer},
        store::adapter::MemoryAdapter,
        test_utils::MithrilFixtureBuilder,
    };

    use crate::{
        MithrilSignerRegisterer, SignerRegisterer, SignerRegistrationRoundOpener,
        VerificationKeyStore, VerificationKeyStorer,
    };

    #[tokio::test]
    async fn can_register_signer_if_registration_round_is_opened_with_operational_certificate() {
        let chain_observer = FakeObserver::default();
        let verification_key_store = Arc::new(VerificationKeyStore::new(
            Box::new(MemoryAdapter::<Epoch, HashMap<PartyId, Signer>>::new(None).unwrap()),
            None,
        ));
        let signer_registerer =
            MithrilSignerRegisterer::new(Arc::new(chain_observer), verification_key_store.clone());
        let registration_epoch = Epoch(1);
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signer_to_register: Signer = fixture.signers()[0].to_owned();
        let stake_distribution = fixture.stake_distribution();

        signer_registerer
            .open_registration_round(registration_epoch, stake_distribution)
            .await
            .expect("signer registration round opening should not fail");

        signer_registerer
            .register_signer(&signer_to_register)
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
        let chain_observer = FakeObserver::default();
        let verification_key_store = Arc::new(VerificationKeyStore::new(
            Box::new(MemoryAdapter::<Epoch, HashMap<PartyId, Signer>>::new(None).unwrap()),
            None,
        ));
        let signer_registerer =
            MithrilSignerRegisterer::new(Arc::new(chain_observer), verification_key_store.clone());
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
            .register_signer(&signer_to_register)
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
        let chain_observer = FakeObserver::default();
        let verification_key_store = Arc::new(VerificationKeyStore::new(
            Box::new(MemoryAdapter::<Epoch, HashMap<PartyId, Signer>>::new(None).unwrap()),
            None,
        ));
        let signer_registerer =
            MithrilSignerRegisterer::new(Arc::new(chain_observer), verification_key_store.clone());
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signer_to_register: Signer = fixture.signers()[0].to_owned();

        signer_registerer
            .register_signer(&signer_to_register)
            .await
            .expect_err("signer registration should fail if no round opened");
    }
}
