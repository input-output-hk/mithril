use async_trait::async_trait;
use chrono::prelude::*;
use hex::ToHex;
use slog_scope::{debug, warn};
use thiserror::Error;

use mithril_common::crypto_helper::{
    key_decode_hex, key_encode_hex, ProtocolAggregateVerificationKey, ProtocolClerk,
    ProtocolKeyRegistration, ProtocolLotteryIndex, ProtocolMultiSignature, ProtocolParameters,
    ProtocolPartyId, ProtocolSignerVerificationKey, ProtocolSingleSignature, ProtocolStake,
    ProtocolStakeDistribution,
};
use mithril_common::entities;
use mithril_common::store::stake_store::{StakeStoreError, StakeStorer};

use super::beacon_store::BeaconStoreError;
use super::dependency::{
    BeaconStoreWrapper, SingleSignatureStoreWrapper, StakeStoreWrapper, VerificationKeyStoreWrapper,
};
use super::store::{
    SingleSignatureStoreError, SingleSignatureStorer, VerificationKeyStoreError,
    VerificationKeyStorer,
};

#[cfg(test)]
use mockall::automock;

#[derive(Error, Debug)]
pub enum ProtocolError {
    #[error("signer already registered")]
    ExistingSigner(),

    #[error("single signature already recorded")]
    ExistingSingleSignature(ProtocolLotteryIndex),

    #[error("codec error: '{0}'")]
    Codec(String),

    #[error("core error: '{0}'")]
    Core(String),

    #[error("no message available")]
    UnavailableMessage(),

    #[error("no protocol parameters available")]
    UnavailableProtocolParameters(),

    #[error("no clerk available")]
    UnavailableClerk(),

    #[error("no beacon available")]
    UnavailableBeacon(),

    #[error("beacon store error: '{0}'")]
    BeaconStore(#[from] BeaconStoreError),

    #[error("verification key store error: '{0}'")]
    VerificationKeyStore(#[from] VerificationKeyStoreError),

    #[error("stake store error: '{0}'")]
    StakeStore(#[from] StakeStoreError),

    #[error("single signature store error: '{0}'")]
    SingleSignatureStore(#[from] SingleSignatureStoreError),
}

/// MultiSigner is the cryptographic engine in charge of producing multi signatures from individual signatures
#[cfg_attr(test, automock)]
#[async_trait]
pub trait MultiSigner: Sync + Send {
    /// Get current message
    async fn get_current_message(&self) -> Option<String>;

    /// Update current message
    async fn update_current_message(&mut self, message: String) -> Result<(), ProtocolError>;

    /// Get protocol parameters
    async fn get_protocol_parameters(&self) -> Option<ProtocolParameters>;

    /// Update protocol parameters
    async fn update_protocol_parameters(
        &mut self,
        protocol_parameters: &ProtocolParameters,
    ) -> Result<(), ProtocolError>;

    /// Get stake distribution
    async fn get_stake_distribution(&self) -> Result<ProtocolStakeDistribution, ProtocolError>;

    /// Update stake distribution
    async fn update_stake_distribution(
        &mut self,
        stakes: &ProtocolStakeDistribution,
    ) -> Result<(), ProtocolError>;

    /// Register a signer
    async fn register_signer(
        &mut self,
        party_id: ProtocolPartyId,
        verification_key: &ProtocolSignerVerificationKey,
    ) -> Result<(), ProtocolError>;

    /// Get signer
    async fn get_signer(
        &self,
        party_id: ProtocolPartyId,
    ) -> Result<Option<ProtocolSignerVerificationKey>, ProtocolError>;

    /// Get signers
    async fn get_signers(&self) -> Result<Vec<entities::Signer>, ProtocolError> {
        debug!("Get signers");
        Ok(self
            .get_signers_with_stake()
            .await?
            .into_iter()
            .map(|signer| signer.into())
            .collect::<Vec<entities::Signer>>())
    }

    /// Get signers with stake
    async fn get_signers_with_stake(&self)
        -> Result<Vec<entities::SignerWithStake>, ProtocolError>;

    /// Registers a single signature
    async fn register_single_signature(
        &mut self,
        party_id: ProtocolPartyId,
        signature: &ProtocolSingleSignature,
        index: ProtocolLotteryIndex,
    ) -> Result<(), ProtocolError>;

    /// Retrieves a multi signature from a message
    async fn get_multi_signature(&self) -> Result<Option<ProtocolMultiSignature>, ProtocolError>;

    /// Creates a multi signature from single signatures
    async fn create_multi_signature(
        &mut self,
    ) -> Result<Option<ProtocolMultiSignature>, ProtocolError>;

    /// Creates a certificate from a multi signatures
    async fn create_certificate(
        &self,
        beacon: entities::Beacon,
        previous_hash: String,
    ) -> Result<Option<entities::Certificate>, ProtocolError>;
}

/// MultiSignerImpl is an implementation of the MultiSigner
pub struct MultiSignerImpl {
    /// Message that is currently signed
    current_message: Option<String>,

    /// Protocol parameters used for signing
    protocol_parameters: Option<ProtocolParameters>,

    /// Clerk used for verifying the single signatures
    clerk: Option<ProtocolClerk>,

    /// Created multi signature for message signed
    multi_signature: Option<ProtocolMultiSignature>,

    /// Created aggregate verification key
    avk: Option<ProtocolAggregateVerificationKey>,

    /// Beacon store
    beacon_store: BeaconStoreWrapper,

    /// Verification key store
    verification_key_store: VerificationKeyStoreWrapper,

    /// Stake store
    stake_store: StakeStoreWrapper,

    /// Single signature store
    single_signature_store: SingleSignatureStoreWrapper,
}

impl MultiSignerImpl {
    /// MultiSignerImpl factory
    pub fn new(
        beacon_store: BeaconStoreWrapper,
        verification_key_store: VerificationKeyStoreWrapper,
        stake_store: StakeStoreWrapper,
        single_signature_store: SingleSignatureStoreWrapper,
    ) -> Self {
        debug!("New MultiSignerImpl created");
        Self {
            current_message: None,
            protocol_parameters: None,
            clerk: None,
            multi_signature: None,
            avk: None,
            beacon_store,
            verification_key_store,
            stake_store,
            single_signature_store,
        }
    }

    /// Creates a clerk
    pub async fn create_clerk(&self) -> Result<Option<ProtocolClerk>, ProtocolError> {
        debug!("Create clerk");
        let stakes = self.get_stake_distribution().await?;
        let mut key_registration = ProtocolKeyRegistration::init();
        let mut total_signers = 0;
        for (party_id, stake) in &stakes {
            if let Ok(Some(verification_key)) = self.get_signer(party_id.to_owned()).await {
                key_registration
                    .register(*stake, verification_key)
                    .map_err(|e| ProtocolError::Core(e.to_string()))?;
                total_signers += 1;
            }
        }
        match total_signers {
            0 => Ok(None),
            _ => {
                let closed_registration = key_registration.close();
                Ok(Some(ProtocolClerk::from_registration(
                    self.protocol_parameters
                        .ok_or_else(ProtocolError::UnavailableProtocolParameters)?,
                    closed_registration,
                )))
            }
        }
    }
}

#[async_trait]
impl MultiSigner for MultiSignerImpl {
    /// Get current message
    async fn get_current_message(&self) -> Option<String> {
        self.current_message.clone()
    }

    /// Update current message
    async fn update_current_message(&mut self, message: String) -> Result<(), ProtocolError> {
        if self.current_message.clone() != Some(message.clone()) {
            self.multi_signature = None;
            self.clerk = self.create_clerk().await?;
        }
        self.current_message = Some(message);
        Ok(())
    }

    /// Get protocol parameters
    async fn get_protocol_parameters(&self) -> Option<ProtocolParameters> {
        self.protocol_parameters
    }

    /// Update protocol parameters
    async fn update_protocol_parameters(
        &mut self,
        protocol_parameters: &ProtocolParameters,
    ) -> Result<(), ProtocolError> {
        debug!("Update protocol parameters to {:?}", protocol_parameters);
        self.protocol_parameters = Some(protocol_parameters.to_owned());
        Ok(())
    }

    /// Get stake distribution
    async fn get_stake_distribution(&self) -> Result<ProtocolStakeDistribution, ProtocolError> {
        debug!("Get stake distribution");
        #[allow(unused_variables, clippy::identity_op)]
        let epoch = self
            .beacon_store
            .read()
            .await
            .get_current_beacon()
            .await?
            .ok_or_else(ProtocolError::UnavailableBeacon)?
            .epoch
            - 0; // TODO: Should be -1 or -2
        warn!(
            "Epoch computation is not final and needs to be fixed: {}",
            epoch
        );
        let signers = self
            .stake_store
            .read()
            .await
            .get_stakes(epoch)
            .await?
            .unwrap_or_default();
        Ok(signers
            .iter()
            .map(|(party_id, signer)| {
                (
                    party_id.to_owned() as ProtocolPartyId,
                    signer.stake as ProtocolStake,
                )
            })
            .collect::<ProtocolStakeDistribution>())
    }

    /// Update stake distribution
    async fn update_stake_distribution(
        &mut self,
        stakes: &ProtocolStakeDistribution,
    ) -> Result<(), ProtocolError> {
        debug!("Update stake distribution to {:?}", stakes);
        #[allow(unused_variables)]
        let epoch = self
            .beacon_store
            .read()
            .await
            .get_current_beacon()
            .await?
            .ok_or_else(ProtocolError::UnavailableBeacon)?
            .epoch;
        let mut stake_store = self.stake_store.write().await;
        for (party_id, stake) in stakes {
            stake_store
                .save_stake(
                    epoch,
                    entities::SignerWithStake::new(party_id.to_owned(), "".to_string(), *stake),
                )
                .await?;
        }

        Ok(())
    }

    /// Get signer verification key
    async fn get_signer(
        &self,
        party_id: ProtocolPartyId,
    ) -> Result<Option<ProtocolSignerVerificationKey>, ProtocolError> {
        debug!("Get signer {}", party_id);
        #[allow(clippy::identity_op)]
        let epoch = self
            .beacon_store
            .read()
            .await
            .get_current_beacon()
            .await?
            .ok_or_else(ProtocolError::UnavailableBeacon)?
            .epoch
            - 0; // TODO: Should be -1 or -2
        warn!(
            "Epoch computation is not final and needs to be fixed: {}",
            epoch
        );
        let signers = self
            .verification_key_store
            .read()
            .await
            .get_verification_keys(epoch)
            .await?
            .unwrap_or_default();
        match signers.get(&party_id) {
            Some(signer) => Ok(Some(
                key_decode_hex(&signer.verification_key).map_err(ProtocolError::Codec)?,
            )),
            _ => Ok(None),
        }
    }

    /// Get signers with stake
    async fn get_signers_with_stake(
        &self,
    ) -> Result<Vec<entities::SignerWithStake>, ProtocolError> {
        debug!("Get signers with stake");
        #[allow(clippy::identity_op)]
        let epoch = self
            .beacon_store
            .read()
            .await
            .get_current_beacon()
            .await?
            .ok_or_else(ProtocolError::UnavailableBeacon)?
            .epoch
            - 0; // TODO: Should be -1 or -2
        warn!(
            "Epoch computation is not final and needs to be fixed: {}",
            epoch
        );
        let signers = self
            .verification_key_store
            .read()
            .await
            .get_verification_keys(epoch)
            .await?
            .unwrap_or_default();
        Ok(self
            .get_stake_distribution()
            .await?
            .iter()
            .filter_map(|(party_id, stake)| {
                signers.get(party_id).map(|signer| {
                    entities::SignerWithStake::new(
                        party_id.to_owned(),
                        signer.verification_key.clone(),
                        *stake as u64,
                    )
                })
            })
            .collect())
    }

    /// Register a signer
    async fn register_signer(
        &mut self,
        party_id: ProtocolPartyId,
        verification_key: &ProtocolSignerVerificationKey,
    ) -> Result<(), ProtocolError> {
        debug!("Register signer {}", party_id);

        let epoch = self
            .beacon_store
            .read()
            .await
            .get_current_beacon()
            .await?
            .ok_or_else(ProtocolError::UnavailableBeacon)?
            .epoch;
        let result = match self
            .verification_key_store
            .write()
            .await
            .save_verification_key(
                epoch,
                entities::Signer::new(
                    party_id,
                    key_encode_hex(*verification_key).map_err(ProtocolError::Codec)?,
                ),
            )
            .await?
        {
            Some(_) => Err(ProtocolError::ExistingSigner()),
            None => Ok(()),
        };
        // TODO: to remove once epoch offset is activated
        if result.as_ref().ok().is_some() {
            self.clerk = self.create_clerk().await?;
        }
        result
    }

    /// Registers a single signature
    async fn register_single_signature(
        &mut self,
        party_id: ProtocolPartyId,
        signature: &ProtocolSingleSignature,
        index: ProtocolLotteryIndex,
    ) -> Result<(), ProtocolError> {
        debug!(
            "Register single signature from {} at index {}",
            party_id, index
        );

        let message = &self
            .get_current_message()
            .await
            .ok_or_else(ProtocolError::UnavailableMessage)?;
        match signature.verify(
            &self
                .protocol_parameters
                .ok_or_else(ProtocolError::UnavailableProtocolParameters)?,
            &self
                .clerk
                .as_ref()
                .ok_or_else(ProtocolError::UnavailableClerk)?
                .compute_avk(),
            message.as_bytes(),
        ) {
            Ok(_) => {
                // Register single signature
                let beacon = self
                    .beacon_store
                    .read()
                    .await
                    .get_current_beacon()
                    .await?
                    .ok_or_else(ProtocolError::UnavailableBeacon)?;
                match self
                    .single_signature_store
                    .write()
                    .await
                    .save_single_signature(
                        &beacon,
                        &entities::SingleSignature::new(
                            party_id.clone(),
                            index,
                            key_encode_hex(signature.to_owned()).map_err(ProtocolError::Codec)?,
                        ),
                    )
                    .await?
                {
                    Some(_) => Err(ProtocolError::ExistingSingleSignature(index)),
                    None => Ok(()),
                }
            }
            Err(e) => Err(ProtocolError::Core(e.to_string())),
        }
    }

    /// Retrieves a multi signature from a message
    async fn get_multi_signature(&self) -> Result<Option<ProtocolMultiSignature>, ProtocolError> {
        debug!("Get multi signature");
        Ok(self.multi_signature.to_owned())
    }

    /// Creates a multi signature from single signatures
    async fn create_multi_signature(
        &mut self,
    ) -> Result<Option<ProtocolMultiSignature>, ProtocolError> {
        let message = &self
            .get_current_message()
            .await
            .ok_or_else(ProtocolError::UnavailableMessage)?;

        debug!("Create multi signature"; "message" => message.encode_hex::<String>());

        let beacon = self
            .beacon_store
            .read()
            .await
            .get_current_beacon()
            .await?
            .ok_or_else(ProtocolError::UnavailableBeacon)?;
        let signatures: Vec<ProtocolSingleSignature> = self
            .single_signature_store
            .read()
            .await
            .get_single_signatures(&beacon)
            .await?
            .unwrap_or_default()
            .iter()
            .flat_map(|(_party_id, h)| {
                h.iter()
                    .filter_map(|(_idx, single_signature)| {
                        key_decode_hex(&single_signature.signature)
                            .map_err(ProtocolError::Codec)
                            .ok()
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        if self.protocol_parameters.unwrap().k > signatures.len() as u64 {
            // Quorum is not reached
            return Ok(None);
        }
        let clerk = self
            .clerk
            .as_ref()
            .ok_or_else(ProtocolError::UnavailableClerk)?;
        match clerk.aggregate(&signatures, message.as_bytes()) {
            Ok(multi_signature) => {
                self.avk = Some(clerk.compute_avk());
                self.multi_signature = Some(multi_signature.clone());
                Ok(Some(multi_signature))
            }
            Err(err) => Err(ProtocolError::Core(err.to_string())),
        }
    }

    /// Creates a certificate from a multi signature
    // TODO: Clarify what started/completed date represents
    async fn create_certificate(
        &self,
        beacon: entities::Beacon,
        previous_hash: String,
    ) -> Result<Option<entities::Certificate>, ProtocolError> {
        debug!("Create certificate");

        match self.get_multi_signature().await? {
            Some(multi_signature) => {
                let protocol_parameters = self
                    .get_protocol_parameters()
                    .await
                    .ok_or_else(ProtocolError::UnavailableProtocolParameters)?
                    .into();
                let digest = self
                    .get_current_message()
                    .await
                    .ok_or_else(ProtocolError::UnavailableMessage)?;
                let previous_hash = previous_hash;
                let started_at = format!("{:?}", Utc::now());
                let completed_at = started_at.clone();
                let signers = self.get_signers_with_stake().await?;
                let aggregate_verification_key =
                    key_encode_hex(&self.avk.as_ref().unwrap()).map_err(ProtocolError::Codec)?;
                let multi_signature =
                    key_encode_hex(&multi_signature).map_err(ProtocolError::Codec)?;
                Ok(Some(entities::Certificate::new(
                    previous_hash,
                    beacon,
                    protocol_parameters,
                    digest,
                    started_at,
                    completed_at,
                    signers,
                    aggregate_verification_key,
                    multi_signature,
                )))
            }
            None => Ok(None),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::beacon_store::{BeaconStore, MemoryBeaconStore};
    use super::super::store::{SingleSignatureStore, VerificationKeyStore};
    use super::*;

    use mithril_common::crypto_helper::tests_setup::*;
    use mithril_common::entities::{
        Epoch, ImmutableFileNumber, LotteryIndex, PartyId, SingleSignature,
    };
    use mithril_common::fake_data;
    use mithril_common::store::adapter::MemoryAdapter;
    use mithril_common::store::stake_store::StakeStore;

    use std::collections::HashMap;
    use std::sync::Arc;
    use tokio::sync::RwLock;

    async fn setup_multi_signer() -> MultiSignerImpl {
        let beacon = fake_data::beacon();
        let mut beacon_store = MemoryBeaconStore::new();
        beacon_store.set_current_beacon(beacon).await.unwrap();
        let verification_key_store = VerificationKeyStore::new(Box::new(
            MemoryAdapter::<Epoch, HashMap<PartyId, entities::Signer>>::new(None).unwrap(),
        ));
        let stake_store = StakeStore::new(Box::new(
            MemoryAdapter::<Epoch, HashMap<PartyId, entities::SignerWithStake>>::new(None).unwrap(),
        ));
        let single_signature_store = SingleSignatureStore::new(Box::new(
            MemoryAdapter::<
                ImmutableFileNumber,
                HashMap<PartyId, HashMap<LotteryIndex, SingleSignature>>,
            >::new(None)
            .unwrap(),
        ));
        let multi_signer = MultiSignerImpl::new(
            Arc::new(RwLock::new(beacon_store)),
            Arc::new(RwLock::new(verification_key_store)),
            Arc::new(RwLock::new(stake_store)),
            Arc::new(RwLock::new(single_signature_store)),
        );
        multi_signer
    }

    #[tokio::test]
    async fn test_multi_signer_current_message_ok() {
        let mut multi_signer = setup_multi_signer().await;

        let current_message_expected = setup_message();
        multi_signer
            .update_current_message(current_message_expected.clone())
            .await
            .expect("update current message failed");

        let current_message = multi_signer
            .get_current_message()
            .await
            .expect("current message should have been retrieved");
        assert_eq!(current_message_expected, current_message)
    }

    #[tokio::test]
    async fn test_multi_signer_protocol_parameters_ok() {
        let mut multi_signer = setup_multi_signer().await;

        let protocol_parameters_expected = setup_protocol_parameters();
        multi_signer
            .update_protocol_parameters(&protocol_parameters_expected)
            .await
            .expect("update protocol parameters failed");

        let protocol_parameters = multi_signer
            .get_protocol_parameters()
            .await
            .expect("protocol parameters should have been retrieved");
        assert_eq!(protocol_parameters_expected, protocol_parameters)
    }

    #[tokio::test]
    async fn test_multi_signer_stake_distribution_ok() {
        let mut multi_signer = setup_multi_signer().await;

        let mut stake_distribution_expected: ProtocolStakeDistribution = setup_signers(5)
            .iter()
            .map(|(party_id, stake, _, _, _)| (party_id.to_owned(), *stake))
            .collect::<_>();
        stake_distribution_expected.sort_by_key(|k| k.0.clone());
        multi_signer
            .update_stake_distribution(&stake_distribution_expected)
            .await
            .expect("update stake distribution failed");

        let mut stake_distribution = multi_signer
            .get_stake_distribution()
            .await
            .expect("get state distribution failed");
        stake_distribution.sort_by_key(|k| k.0.clone());
        assert_eq!(stake_distribution_expected, stake_distribution)
    }

    #[tokio::test]
    async fn test_multi_signer_register_signer_ok() {
        let mut multi_signer = setup_multi_signer().await;

        let protocol_parameters_expected = setup_protocol_parameters();
        multi_signer
            .update_protocol_parameters(&protocol_parameters_expected)
            .await
            .expect("update protocol parameters failed");

        let signers = setup_signers(5);

        let stake_distribution_expected: ProtocolStakeDistribution = signers
            .iter()
            .map(|(party_id, stake, _, _, _)| (party_id.to_owned(), *stake))
            .collect::<_>();
        multi_signer
            .update_stake_distribution(&stake_distribution_expected)
            .await
            .expect("update stake distribution failed");

        for (party_id, _, verification_key, _, _) in &signers {
            multi_signer
                .register_signer(party_id.to_owned(), &verification_key)
                .await
                .expect("register should have succeeded")
        }

        let mut signers_with_stake_all_expected = Vec::new();
        for (party_id, stake, verification_key_expected, _, _) in &signers {
            let verification_key = multi_signer.get_signer(party_id.to_owned()).await;
            assert!(verification_key.as_ref().unwrap().is_some());
            assert_eq!(
                *verification_key_expected,
                verification_key.unwrap().unwrap()
            );
            signers_with_stake_all_expected.push(entities::SignerWithStake::new(
                party_id.to_owned(),
                key_encode_hex(verification_key_expected).unwrap(),
                *stake,
            ));
        }
        signers_with_stake_all_expected.sort_by_key(|signer| signer.party_id.to_owned());
        let signers_all_expected = signers_with_stake_all_expected
            .clone()
            .into_iter()
            .map(|signer| signer.into())
            .collect::<Vec<entities::Signer>>();

        let mut signers_with_stake_all = multi_signer
            .get_signers_with_stake()
            .await
            .expect("get signers with stake should have been succeeded");
        signers_with_stake_all.sort_by_key(|signer| signer.party_id.to_owned());

        assert_eq!(signers_with_stake_all_expected, signers_with_stake_all);

        let mut signers_all = multi_signer
            .get_signers()
            .await
            .expect("get signers should have been succeeded");
        signers_all.sort_by_key(|signer| signer.party_id.to_owned());

        assert_eq!(signers_all_expected, signers_all);
    }

    #[tokio::test]
    async fn test_multi_signer_multi_signature_ok() {
        let beacon = fake_data::beacon();
        let previous_hash = "prev-hash-123".to_string();

        let mut multi_signer = setup_multi_signer().await;

        let message = setup_message();
        multi_signer
            .update_current_message(message.clone())
            .await
            .expect("update current message failed");

        let protocol_parameters = setup_protocol_parameters();
        multi_signer
            .update_protocol_parameters(&protocol_parameters)
            .await
            .expect("update protocol parameters failed");

        let signers = setup_signers(5);
        let stake_distribution = &signers
            .iter()
            .map(|(party_id, stake, _, _, _)| (party_id.to_owned(), *stake))
            .collect::<_>();
        multi_signer
            .update_stake_distribution(&stake_distribution)
            .await
            .expect("update stake distribution failed");
        for (party_id, _, verification_key, _, _) in &signers {
            multi_signer
                .register_signer(party_id.to_owned(), verification_key)
                .await
                .expect("register should have succeeded")
        }

        let mut signatures = Vec::new();
        for (party_id, _, _, protocol_signer, _) in &signers {
            for i in 1..=protocol_parameters.m {
                if let Some(signature) = protocol_signer.sign(&message.as_bytes(), i) {
                    signatures.push((party_id.to_owned(), signature, i as ProtocolLotteryIndex));
                }
            }
        }

        let quorum_split = protocol_parameters.k as usize - 1;
        assert!(quorum_split > 1, "quorum should be greater");
        multi_signer
            .create_multi_signature()
            .await
            .expect("create multi sgnature should not fail");
        assert!(multi_signer
            .get_multi_signature()
            .await
            .expect("get multi signature should not fail")
            .is_none());
        assert!(multi_signer
            .create_certificate(beacon.clone(), previous_hash.clone())
            .await
            .expect("create_certificate should not fail")
            .is_none());
        for (party_id, signature, index) in &signatures[0..quorum_split] {
            multi_signer
                .register_single_signature(party_id.to_owned(), signature, *index)
                .await
                .expect("register single signature should not fail");
        }
        multi_signer
            .create_multi_signature()
            .await
            .expect("create multi sgnature should not fail");
        assert!(multi_signer
            .get_multi_signature()
            .await
            .expect("get multi signature should not fail")
            .is_none());
        assert!(multi_signer
            .create_certificate(beacon.clone(), previous_hash.clone())
            .await
            .expect("create_certificate should not fail")
            .is_none());
        for (party_id, signature, index) in &signatures[quorum_split..] {
            multi_signer
                .register_single_signature(party_id.to_owned(), signature, *index)
                .await
                .expect("register single signature should not fail");
        }
        multi_signer
            .create_multi_signature()
            .await
            .expect("create multi sgnature should not fail");
        assert!(multi_signer
            .get_multi_signature()
            .await
            .expect("get multi signature should not fail")
            .is_some());
        assert!(multi_signer
            .create_certificate(beacon.clone(), previous_hash.clone())
            .await
            .expect("create_certificate should not fail")
            .is_some());
    }
}
