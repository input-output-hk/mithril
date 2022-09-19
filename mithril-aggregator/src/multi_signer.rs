use std::collections::HashMap;
use std::sync::Arc;

use async_trait::async_trait;
use chrono::prelude::*;
use hex::ToHex;
use slog_scope::{debug, trace, warn};
use thiserror::Error;

use mithril::AggregationError;
use mithril_common::crypto_helper::{
    key_decode_hex, key_encode_hex, ProtocolAggregateVerificationKey, ProtocolClerk,
    ProtocolKeyRegistration, ProtocolMultiSignature, ProtocolParameters, ProtocolPartyId,
    ProtocolSignerVerificationKey, ProtocolSingleSignature, ProtocolStakeDistribution,
    PROTOCOL_VERSION,
};
use mithril_common::entities::{self, PartyId, SignerWithStake};
use mithril_common::store::{StakeStore, StakeStorer, StoreError};
use mithril_common::{
    NEXT_SIGNER_EPOCH_RETRIEVAL_OFFSET, SIGNER_EPOCH_RECORDING_OFFSET,
    SIGNER_EPOCH_RETRIEVAL_OFFSET,
};

use crate::store::{SingleSignatureStorer, VerificationKeyStorer};
use crate::{
    ProtocolParametersStore, ProtocolParametersStorer, SingleSignatureStore, VerificationKeyStore,
};

#[cfg(test)]
use mockall::automock;

/// Error type for multi signer service.
#[derive(Error, Debug)]
pub enum ProtocolError {
    /// Signer is already registered.
    #[error("signer already registered")]
    ExistingSigner(),

    /// Single signature already recorded.
    #[error("single signature already recorded")]
    ExistingSingleSignature(entities::PartyId),

    /// Codec error.
    #[error("codec error: '{0}'")]
    Codec(String),

    /// Mithril Core library returned an error.
    #[error("core error: '{0}'")]
    Core(String),

    /// No message available.
    #[error("no message available")]
    UnavailableMessage(),

    /// No protocol parameters available.
    #[error("no protocol parameters available")]
    UnavailableProtocolParameters(),

    /// No clerk available.
    #[error("no clerk available")]
    UnavailableClerk(),

    /// No beacon available.
    #[error("no beacon available")]
    UnavailableBeacon(),

    /// Store error.
    #[error("store error: {0}")]
    StoreError(#[from] StoreError),

    /// Beacon error.
    #[error("beacon error: '{0}'")]
    Beacon(#[from] entities::EpochError),
}

/// MultiSigner is the cryptographic engine in charge of producing multi signatures from individual signatures
#[cfg_attr(test, automock)]
#[async_trait]
pub trait MultiSigner: Sync + Send {
    /// Get current message
    async fn get_current_message(&self) -> Option<entities::ProtocolMessage>;

    /// Update current message
    async fn update_current_message(
        &mut self,
        message: entities::ProtocolMessage,
    ) -> Result<(), ProtocolError>;

    /// Get current beacon
    async fn get_current_beacon(&self) -> Option<entities::Beacon>;

    /// Update current beacon
    async fn update_current_beacon(
        &mut self,
        beacon: entities::Beacon,
    ) -> Result<(), ProtocolError>;

    /// Get protocol parameters
    async fn get_protocol_parameters(&self) -> Result<Option<ProtocolParameters>, ProtocolError>;

    /// Update protocol parameters
    async fn update_protocol_parameters(
        &mut self,
        protocol_parameters: &ProtocolParameters,
    ) -> Result<(), ProtocolError>;

    /// Get next protocol parameters
    async fn get_next_protocol_parameters(
        &self,
    ) -> Result<Option<ProtocolParameters>, ProtocolError>;

    /// Get stake distribution
    async fn get_stake_distribution(&self) -> Result<ProtocolStakeDistribution, ProtocolError>;

    /// Get next stake distribution
    /// i.e. the stake distribution that will be used at the next epoch
    async fn get_next_stake_distribution(&self)
        -> Result<ProtocolStakeDistribution, ProtocolError>;

    /// Update stake distribution
    async fn update_stake_distribution(
        &mut self,
        stakes: &ProtocolStakeDistribution,
    ) -> Result<(), ProtocolError>;

    /// Compute aggregate verification key from stake distribution
    async fn compute_aggregate_verification_key(
        &self,
        signers_with_stakes: &[SignerWithStake],
        protocol_parameters: &ProtocolParameters,
    ) -> Result<Option<ProtocolAggregateVerificationKey>, ProtocolError>;

    /// Compute stake distribution aggregate verification key
    async fn compute_stake_distribution_aggregate_verification_key(
        &self,
    ) -> Result<Option<String>, ProtocolError> {
        let signers_with_stake = self.get_signers_with_stake().await?;
        let protocol_parameters = self
            .get_protocol_parameters()
            .await?
            .ok_or_else(ProtocolError::UnavailableProtocolParameters)?;
        match self
            .compute_aggregate_verification_key(&signers_with_stake, &protocol_parameters)
            .await?
        {
            Some(avk) => Ok(Some(key_encode_hex(avk).map_err(ProtocolError::Codec)?)),
            None => Ok(None),
        }
    }

    /// Compute next stake distribution aggregate verification key
    async fn compute_next_stake_distribution_aggregate_verification_key(
        &self,
    ) -> Result<Option<String>, ProtocolError> {
        let next_signers_with_stake = self.get_next_signers_with_stake().await?;
        let protocol_parameters = self
            .get_next_protocol_parameters()
            .await?
            .ok_or_else(ProtocolError::UnavailableProtocolParameters)?;
        match self
            .compute_aggregate_verification_key(&next_signers_with_stake, &protocol_parameters)
            .await?
        {
            Some(avk) => Ok(Some(key_encode_hex(avk).map_err(ProtocolError::Codec)?)),
            None => Ok(None),
        }
    }

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

    /// Get signers for the next epoch with their stake
    async fn get_next_signers_with_stake(
        &self,
    ) -> Result<Vec<entities::SignerWithStake>, ProtocolError>;

    /// Registers a single signature
    async fn register_single_signature(
        &mut self,
        signatures: &entities::SingleSignatures,
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
    current_message: Option<entities::ProtocolMessage>,

    /// Beacon that is currently used
    current_beacon: Option<entities::Beacon>,

    /// Signing start datetime of current message
    current_initiated_at: Option<DateTime<Utc>>,

    /// Clerk used for verifying the single signatures
    clerk: Option<ProtocolClerk>,

    /// Created multi signature for message signed
    multi_signature: Option<ProtocolMultiSignature>,

    /// Created aggregate verification key
    avk: Option<ProtocolAggregateVerificationKey>,

    /// Verification key store
    verification_key_store: Arc<VerificationKeyStore>,

    /// Stake store
    stake_store: Arc<StakeStore>,

    /// Single signature store
    single_signature_store: Arc<SingleSignatureStore>,

    /// Protocol parameters store
    protocol_parameters_store: Arc<ProtocolParametersStore>,
}

impl MultiSignerImpl {
    /// MultiSignerImpl factory
    pub fn new(
        verification_key_store: Arc<VerificationKeyStore>,
        stake_store: Arc<StakeStore>,
        single_signature_store: Arc<SingleSignatureStore>,
        protocol_parameters_store: Arc<ProtocolParametersStore>,
    ) -> Self {
        debug!("New MultiSignerImpl created");
        Self {
            current_message: None,
            current_beacon: None,
            current_initiated_at: None,
            clerk: None,
            multi_signature: None,
            avk: None,
            verification_key_store,
            stake_store,
            single_signature_store,
            protocol_parameters_store,
        }
    }

    /// Creates a clerk
    pub async fn create_clerk(
        &self,
        signers_with_stake: &[SignerWithStake],
        protocol_parameters: &ProtocolParameters,
    ) -> Result<Option<ProtocolClerk>, ProtocolError> {
        debug!("Create clerk");
        let mut key_registration = ProtocolKeyRegistration::init();
        let mut total_signers = 0;
        for signer in signers_with_stake {
            if let Ok(Some(verification_key)) = key_decode_hex(&signer.verification_key) {
                key_registration
                    .register(signer.stake, verification_key)
                    .map_err(|e| ProtocolError::Core(e.to_string()))?;
                total_signers += 1;
            }
        }
        match total_signers {
            0 => Ok(None),
            _ => {
                let closed_registration = key_registration.close();
                Ok(Some(ProtocolClerk::from_registration(
                    protocol_parameters,
                    &closed_registration,
                )))
            }
        }
    }

    /// Get stake distribution with epoch offset
    async fn get_stake_distribution_with_epoch_offset(
        &self,
        epoch_offset: i64,
    ) -> Result<ProtocolStakeDistribution, ProtocolError> {
        debug!("Get stake distribution with epoch offset"; "epoch_offset"=>epoch_offset);
        let epoch = self
            .current_beacon
            .as_ref()
            .ok_or_else(ProtocolError::UnavailableBeacon)?
            .epoch
            .offset_by(epoch_offset)?;

        let stakes = self
            .stake_store
            .get_stakes(epoch)
            .await?
            .unwrap_or_default();
        Ok(stakes.into_iter().collect::<ProtocolStakeDistribution>())
    }

    /// Get protocol parameters with epoch offset
    async fn get_protocol_parameters_with_epoch_offset(
        &self,
        epoch_offset: i64,
    ) -> Result<Option<ProtocolParameters>, ProtocolError> {
        debug!("Get protocol parameters with epoch offset"; "epoch_offset"=>epoch_offset);
        let epoch = self
            .current_beacon
            .as_ref()
            .ok_or_else(ProtocolError::UnavailableBeacon)?
            .epoch
            .offset_by(epoch_offset)?;

        match self
            .protocol_parameters_store
            .get_protocol_parameters(epoch)
            .await
        {
            Ok(Some(protocol_parameters)) => Ok(Some(protocol_parameters.into())),
            Ok(None) => Ok(None),
            Err(e) => Err(e.into()),
        }
    }
}

#[async_trait]
impl MultiSigner for MultiSignerImpl {
    /// Get current message
    async fn get_current_message(&self) -> Option<entities::ProtocolMessage> {
        debug!("Get current message");
        self.current_message.clone()
    }

    /// Update current message
    async fn update_current_message(
        &mut self,
        message: entities::ProtocolMessage,
    ) -> Result<(), ProtocolError> {
        debug!("Update current_message"; "protocol_message" =>  #?message, "signed message" => message.compute_hash().encode_hex::<String>());

        self.multi_signature = None;
        let signers_with_stake = self.get_signers_with_stake().await?;
        let protocol_parameters = self
            .get_protocol_parameters()
            .await?
            .ok_or_else(ProtocolError::UnavailableProtocolParameters)?;
        match self
            .create_clerk(&signers_with_stake, &protocol_parameters)
            .await
        {
            Ok(Some(clerk)) => {
                trace!("update_current_message::new_clerk_created");
                self.clerk = Some(clerk)
            }
            Ok(None) => {
                warn!(
                    "update_current_message::no_clerk_created: probably not enough signers with valid verification keys";
                    "signers" => ?signers_with_stake
                );
                self.clerk = None
            }
            Err(ProtocolError::Beacon(err)) => {
                warn!("update_current_message::error"; "error" => ?err);
            }
            Err(e) => return Err(e),
        }
        self.current_initiated_at = Some(Utc::now());
        self.current_message = Some(message);
        Ok(())
    }

    async fn get_current_beacon(&self) -> Option<entities::Beacon> {
        self.current_beacon.clone()
    }

    async fn update_current_beacon(
        &mut self,
        beacon: entities::Beacon,
    ) -> Result<(), ProtocolError> {
        debug!("Update current_beacon to {:?}", beacon);
        self.current_beacon = Some(beacon);
        Ok(())
    }

    // TODO: protocol parameters should ALWAYS be available
    /// Get protocol parameters
    async fn get_protocol_parameters(&self) -> Result<Option<ProtocolParameters>, ProtocolError> {
        debug!("Get protocol parameters");
        self.get_protocol_parameters_with_epoch_offset(SIGNER_EPOCH_RETRIEVAL_OFFSET)
            .await
    }

    /// Get next protocol parameters
    async fn get_next_protocol_parameters(
        &self,
    ) -> Result<Option<ProtocolParameters>, ProtocolError> {
        debug!("Get next protocol parameters");
        self.get_protocol_parameters_with_epoch_offset(NEXT_SIGNER_EPOCH_RETRIEVAL_OFFSET)
            .await
    }

    /// Update protocol parameters
    async fn update_protocol_parameters(
        &mut self,
        protocol_parameters: &ProtocolParameters,
    ) -> Result<(), ProtocolError> {
        debug!("Update protocol parameters to {:?}", protocol_parameters);
        let epoch = self
            .current_beacon
            .as_ref()
            .ok_or_else(ProtocolError::UnavailableBeacon)?
            .epoch
            .offset_by(SIGNER_EPOCH_RECORDING_OFFSET)?;

        self.protocol_parameters_store
            .save_protocol_parameters(epoch, protocol_parameters.to_owned().into())
            .await?;
        Ok(())
    }

    /// Get stake distribution
    async fn get_stake_distribution(&self) -> Result<ProtocolStakeDistribution, ProtocolError> {
        debug!("Get stake distribution");
        self.get_stake_distribution_with_epoch_offset(SIGNER_EPOCH_RETRIEVAL_OFFSET)
            .await
    }

    /// Get next stake distribution
    async fn get_next_stake_distribution(
        &self,
    ) -> Result<ProtocolStakeDistribution, ProtocolError> {
        debug!("Get next stake distribution");
        self.get_stake_distribution_with_epoch_offset(NEXT_SIGNER_EPOCH_RETRIEVAL_OFFSET)
            .await
    }

    /// Update stake distribution
    async fn update_stake_distribution(
        &mut self,
        stakes: &ProtocolStakeDistribution,
    ) -> Result<(), ProtocolError> {
        debug!("Update stake distribution"; "stakes" => #?stakes);
        let epoch = self
            .current_beacon
            .as_ref()
            .ok_or_else(ProtocolError::UnavailableBeacon)?
            .epoch
            .offset_by(SIGNER_EPOCH_RECORDING_OFFSET)?;
        let stakes = HashMap::from_iter(stakes.iter().cloned());
        self.stake_store.save_stakes(epoch, stakes).await?;

        Ok(())
    }

    /// Compute aggregate verification key from stake distribution
    async fn compute_aggregate_verification_key(
        &self,
        signers_with_stakes: &[SignerWithStake],
        protocol_parameters: &ProtocolParameters,
    ) -> Result<Option<ProtocolAggregateVerificationKey>, ProtocolError> {
        match self
            .create_clerk(signers_with_stakes, protocol_parameters)
            .await?
        {
            Some(clerk) => Ok(Some(clerk.compute_avk())),
            None => Ok(None),
        }
    }

    /// Register a signer
    async fn register_signer(
        &mut self,
        party_id: ProtocolPartyId,
        verification_key: &ProtocolSignerVerificationKey,
    ) -> Result<(), ProtocolError> {
        debug!("Register signer {}", party_id);

        let epoch = self
            .current_beacon
            .as_ref()
            .ok_or_else(ProtocolError::UnavailableBeacon)?
            .epoch
            .offset_by(SIGNER_EPOCH_RECORDING_OFFSET)?;

        match self
            .verification_key_store
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
        }
    }

    /// Get signer verification key
    async fn get_signer(
        &self,
        party_id: ProtocolPartyId,
    ) -> Result<Option<ProtocolSignerVerificationKey>, ProtocolError> {
        debug!("Get signer {}", party_id);
        let epoch = self
            .current_beacon
            .as_ref()
            .ok_or_else(ProtocolError::UnavailableBeacon)?
            .epoch
            .offset_by(SIGNER_EPOCH_RETRIEVAL_OFFSET)?;
        let signers = self
            .verification_key_store
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

    async fn get_signers_with_stake(
        &self,
    ) -> Result<Vec<entities::SignerWithStake>, ProtocolError> {
        debug!("Get signers with stake");
        let epoch = self
            .current_beacon
            .as_ref()
            .ok_or_else(ProtocolError::UnavailableBeacon)?
            .epoch
            .offset_by(SIGNER_EPOCH_RETRIEVAL_OFFSET)?;
        let signers = self
            .verification_key_store
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

    async fn get_next_signers_with_stake(
        &self,
    ) -> Result<Vec<entities::SignerWithStake>, ProtocolError> {
        debug!("Get next signers with stake");
        let epoch = self
            .current_beacon
            .as_ref()
            .ok_or_else(ProtocolError::UnavailableBeacon)?
            .epoch
            .offset_by(NEXT_SIGNER_EPOCH_RETRIEVAL_OFFSET)?;
        let signers = self
            .verification_key_store
            .get_verification_keys(epoch)
            .await?
            .unwrap_or_default();
        Ok(self
            .get_next_stake_distribution()
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

    /// Registers a single signature
    async fn register_single_signature(
        &mut self,
        signatures: &entities::SingleSignatures,
    ) -> Result<(), ProtocolError> {
        debug!(
            "Register single signature from {} at indexes {:?}",
            signatures.party_id, signatures.won_indexes
        );

        let message = &self
            .get_current_message()
            .await
            .ok_or_else(ProtocolError::UnavailableMessage)?;
        let protocol_parameters = self
            .get_protocol_parameters()
            .await?
            .ok_or_else(ProtocolError::UnavailableProtocolParameters)?;
        let avk = &self
            .clerk
            .as_ref()
            .ok_or_else(ProtocolError::UnavailableClerk)?
            .compute_avk();

        signatures
            .to_protocol_signature()
            .map_err(ProtocolError::Codec)?
            .verify(&protocol_parameters, avk, message.compute_hash().as_bytes())
            .map_err(|e| ProtocolError::Core(e.to_string()))?;

        // Register single signature
        let beacon = self
            .current_beacon
            .as_ref()
            .ok_or_else(ProtocolError::UnavailableBeacon)?;

        match self
            .single_signature_store
            .save_single_signatures(beacon, signatures)
            .await?
        {
            Some(_) => Err(ProtocolError::ExistingSingleSignature(
                signatures.party_id.clone(),
            )),
            None => Ok(()),
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
        debug!("Create multi signature");
        let message = &self
            .get_current_message()
            .await
            .ok_or_else(ProtocolError::UnavailableMessage)?;

        let beacon = self
            .current_beacon
            .as_ref()
            .ok_or_else(ProtocolError::UnavailableBeacon)?;
        let signatures: Vec<ProtocolSingleSignature> = self
            .single_signature_store
            .get_single_signatures(beacon)
            .await?
            .unwrap_or_default()
            .iter()
            .filter_map(|(_party_id, single_signature)| {
                single_signature
                    .to_protocol_signature()
                    .map_err(ProtocolError::Codec)
                    .ok()
            })
            .collect::<Vec<_>>();

        let clerk = self
            .clerk
            .as_ref()
            .ok_or_else(ProtocolError::UnavailableClerk)?;
        match clerk.aggregate(&signatures, message.compute_hash().as_bytes()) {
            Ok(multi_signature) => {
                self.avk = Some(clerk.compute_avk());
                self.multi_signature = Some(multi_signature.clone());
                Ok(Some(multi_signature))
            }
            Err(AggregationError::NotEnoughSignatures(actual, expected)) => {
                warn!("Could not compute multi-signature: Not enough signatures. Got only {} out of {}.", actual, expected);
                Ok(None)
            }
            Err(err) => Err(ProtocolError::Core(err.to_string())),
        }
    }

    /// Creates a certificate from a multi signature
    async fn create_certificate(
        &self,
        beacon: entities::Beacon,
        previous_hash: String,
    ) -> Result<Option<entities::Certificate>, ProtocolError> {
        debug!("Create certificate");

        match self.get_multi_signature().await? {
            Some(multi_signature) => {
                let protocol_version = PROTOCOL_VERSION.to_string();
                let protocol_parameters = self
                    .get_protocol_parameters()
                    .await?
                    .ok_or_else(ProtocolError::UnavailableProtocolParameters)?
                    .into();
                let initiated_at =
                    format!("{:?}", self.current_initiated_at.unwrap_or_else(Utc::now));
                let sealed_at = format!("{:?}", Utc::now());
                let signatures_party_ids: Vec<PartyId> = self
                    .single_signature_store
                    .get_single_signatures(&beacon)
                    .await?
                    .unwrap_or_default()
                    .into_iter()
                    .map(|(party_id, _single_signature)| party_id)
                    .collect::<Vec<_>>();
                let signers = self
                    .get_signers_with_stake()
                    .await?
                    .into_iter()
                    .filter(|signer| signatures_party_ids.contains(&signer.party_id))
                    .collect::<Vec<_>>();
                let metadata = entities::CertificateMetadata::new(
                    protocol_version,
                    protocol_parameters,
                    initiated_at,
                    sealed_at,
                    signers,
                );
                let protocol_message = self
                    .get_current_message()
                    .await
                    .ok_or_else(ProtocolError::UnavailableMessage)?;
                let aggregate_verification_key =
                    key_encode_hex(&self.avk.as_ref().unwrap()).map_err(ProtocolError::Codec)?;
                let multi_signature =
                    key_encode_hex(&multi_signature).map_err(ProtocolError::Codec)?;
                let genesis_signature = "".to_string();
                Ok(Some(entities::Certificate::new(
                    previous_hash,
                    beacon,
                    metadata,
                    protocol_message,
                    aggregate_verification_key,
                    multi_signature,
                    genesis_signature,
                )))
            }
            None => Ok(None),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::store::{SingleSignatureStore, VerificationKeyStore};
    use crate::ProtocolParametersStore;

    use mithril_common::crypto_helper::tests_setup::*;
    use mithril_common::fake_data;
    use mithril_common::store::adapter::MemoryAdapter;
    use mithril_common::store::StakeStore;

    use std::collections::HashMap;
    use std::sync::Arc;

    async fn setup_multi_signer() -> MultiSignerImpl {
        let beacon = fake_data::beacon();
        let verification_key_store = VerificationKeyStore::new(Box::new(
            MemoryAdapter::<entities::Epoch, HashMap<entities::PartyId, entities::Signer>>::new(
                None,
            )
            .unwrap(),
        ));
        let stake_store = StakeStore::new(
            Box::new(
                MemoryAdapter::<entities::Epoch, HashMap<entities::PartyId, entities::Stake>>::new(
                    None,
                )
                .unwrap(),
            ),
            None,
        );
        let single_signature_store = SingleSignatureStore::new(
            Box::new(
                MemoryAdapter::<
                    entities::Beacon,
                    HashMap<entities::PartyId, entities::SingleSignatures>,
                >::new(None)
                .unwrap(),
            ),
            None,
        );
        let protocol_parameters_store = ProtocolParametersStore::new(
            Box::new(
                MemoryAdapter::<entities::Epoch, entities::ProtocolParameters>::new(Some(vec![
                    (
                        beacon.epoch.offset_to_signer_retrieval_epoch().unwrap(),
                        fake_data::protocol_parameters(),
                    ),
                    (
                        beacon
                            .epoch
                            .offset_to_next_signer_retrieval_epoch()
                            .unwrap(),
                        fake_data::protocol_parameters(),
                    ),
                ]))
                .unwrap(),
            ),
            None,
        );
        let mut multi_signer = MultiSignerImpl::new(
            Arc::new(verification_key_store),
            Arc::new(stake_store),
            Arc::new(single_signature_store),
            Arc::new(protocol_parameters_store),
        );

        multi_signer
            .update_current_beacon(beacon)
            .await
            .expect("update_current_beacon should not fail");
        multi_signer
    }

    async fn offset_epoch(multi_signer: &mut MultiSignerImpl, offset: i64) {
        let mut beacon = multi_signer.get_current_beacon().await.unwrap();
        let epoch_new = beacon.epoch.offset_by(offset).unwrap();
        beacon.epoch = epoch_new;
        multi_signer.update_current_beacon(beacon).await.unwrap();
    }

    fn take_signatures_until_quorum_is_almost_reached(
        signatures: &mut Vec<entities::SingleSignatures>,
        quorum: usize,
    ) -> Vec<entities::SingleSignatures> {
        signatures.sort_by(|l, r| l.won_indexes.len().cmp(&r.won_indexes.len()));

        let mut result = vec![];
        let mut nb_won_indexes = 0;

        while let Some(signature) = signatures.first() {
            if signature.won_indexes.len() + nb_won_indexes >= quorum {
                break;
            }
            nb_won_indexes += signature.won_indexes.len();
            result.push(signatures.remove(0));
        }

        result
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

        offset_epoch(
            &mut multi_signer,
            (SIGNER_EPOCH_RECORDING_OFFSET - SIGNER_EPOCH_RETRIEVAL_OFFSET) as i64,
        )
        .await;

        let protocol_parameters = multi_signer
            .get_protocol_parameters()
            .await
            .expect("protocol parameters should have been retrieved");
        let protocol_parameters: entities::ProtocolParameters = protocol_parameters.unwrap().into();
        let protocol_parameters_expected: entities::ProtocolParameters =
            protocol_parameters_expected.into();
        assert_eq!(protocol_parameters_expected, protocol_parameters);
    }

    #[tokio::test]
    async fn test_multi_signer_stake_distribution_ok() {
        let mut multi_signer = setup_multi_signer().await;

        let mut stake_distribution_expected: ProtocolStakeDistribution =
            setup_signers(5, &setup_protocol_parameters())
                .iter()
                .map(|(party_id, stake, _, _, _)| (party_id.to_owned(), *stake))
                .collect::<_>();
        stake_distribution_expected.sort_by_key(|k| k.0.clone());
        multi_signer
            .update_stake_distribution(&stake_distribution_expected)
            .await
            .expect("update stake distribution failed");

        offset_epoch(
            &mut multi_signer,
            (SIGNER_EPOCH_RECORDING_OFFSET - SIGNER_EPOCH_RETRIEVAL_OFFSET) as i64,
        )
        .await;

        let mut stake_distribution = multi_signer
            .get_stake_distribution()
            .await
            .expect("get state distribution failed");
        stake_distribution.sort_by_key(|k| k.0.clone());
        let stake_distribution_next_expected = multi_signer
            .get_next_stake_distribution()
            .await
            .expect("get next state distribution failed");
        assert_eq!(stake_distribution_expected, stake_distribution);

        offset_epoch(&mut multi_signer, 1).await;

        let mut stake_distribution = multi_signer
            .get_stake_distribution()
            .await
            .expect("get state distribution failed");
        stake_distribution.sort_by_key(|k| k.0.clone());
        assert_eq!(stake_distribution_next_expected, stake_distribution);
    }

    #[tokio::test]
    async fn test_multi_signer_register_signer_ok() {
        let mut multi_signer = setup_multi_signer().await;

        let protocol_parameters_expected = setup_protocol_parameters();
        multi_signer
            .update_protocol_parameters(&protocol_parameters_expected)
            .await
            .expect("update protocol parameters failed");

        let signers = setup_signers(5, &protocol_parameters_expected);

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
                .register_signer(party_id.to_owned(), verification_key)
                .await
                .expect("register should have succeeded")
        }

        offset_epoch(
            &mut multi_signer,
            (SIGNER_EPOCH_RECORDING_OFFSET - SIGNER_EPOCH_RETRIEVAL_OFFSET) as i64,
        )
        .await;

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
        let protocol_parameters = setup_protocol_parameters();
        multi_signer
            .update_protocol_parameters(&protocol_parameters)
            .await
            .expect("update protocol parameters failed");

        let signers = setup_signers(5, &protocol_parameters);
        let stake_distribution = &signers
            .iter()
            .map(|(party_id, stake, _, _, _)| (party_id.to_owned(), *stake))
            .collect::<_>();
        multi_signer
            .update_stake_distribution(stake_distribution)
            .await
            .expect("update stake distribution failed");
        for (party_id, _, verification_key, _, _) in &signers {
            multi_signer
                .register_signer(party_id.to_owned(), verification_key)
                .await
                .expect("register should have succeeded")
        }

        offset_epoch(
            &mut multi_signer,
            (SIGNER_EPOCH_RECORDING_OFFSET - SIGNER_EPOCH_RETRIEVAL_OFFSET) as i64,
        )
        .await;
        // We have to update the current message AFTER we reached the epoch for
        // which the signers registered for the stake distribution to be valid
        // hence the multisigner be able to create a Clerk and being able to
        // register single signatures
        multi_signer
            .update_current_message(message.clone())
            .await
            .expect("update current message failed");
        let mut signatures = Vec::new();

        let mut expected_certificate_signers = Vec::new();
        for (party_id, stake, _, protocol_signer, protocol_initializer) in &signers {
            if let Some(signature) = protocol_signer.sign(message.compute_hash().as_bytes()) {
                let won_indexes = signature.indexes.clone();

                signatures.push(entities::SingleSignatures::new(
                    party_id.to_owned(),
                    key_encode_hex(signature).unwrap(),
                    won_indexes,
                ));

                expected_certificate_signers.push(entities::SignerWithStake::new(
                    party_id.to_owned(),
                    key_encode_hex(protocol_initializer.verification_key()).unwrap(),
                    *stake,
                ))
            }
        }

        let signatures_to_almost_reach_quorum = take_signatures_until_quorum_is_almost_reached(
            &mut signatures,
            protocol_parameters.k as usize,
        );
        assert!(
            !signatures_to_almost_reach_quorum.is_empty(),
            "they should be at least one signature that can be registered without reaching the quorum"
        );

        // No signatures registered: multi-signer can't create the multi-signature
        multi_signer
            .create_multi_signature()
            .await
            .expect("create multi signature should not fail");
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

        // Add some signatures but not enough to reach the quorum: multi-signer should not create the multi-signature
        for signature in signatures_to_almost_reach_quorum {
            multi_signer
                .register_single_signature(&signature)
                .await
                .expect("register single signature should not fail");
        }
        multi_signer
            .create_multi_signature()
            .await
            .expect("create multi signature should not fail");
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

        // Add the remaining signatures to reach the quorum: multi-signer should create a multi-signature
        for signature in &signatures {
            multi_signer
                .register_single_signature(signature)
                .await
                .expect("register single signature should not fail");
        }
        multi_signer
            .create_multi_signature()
            .await
            .expect("create multi signature should not fail");
        assert!(
            multi_signer
                .get_multi_signature()
                .await
                .expect("get multi signature should not fail")
                .is_some(),
            "no multi-signature were computed"
        );

        // A certificate should also be produced with valid AVK and valid signers list
        let beacon = multi_signer.get_current_beacon().await.unwrap();
        let certificate = multi_signer
            .create_certificate(beacon, previous_hash.clone())
            .await
            .expect("create_certificate should not fail")
            .expect("create_certificate should return something");
        let mut expected_certificate_signers_party_ids = expected_certificate_signers
            .iter()
            .map(|s| s.party_id.clone())
            .collect::<Vec<ProtocolPartyId>>();
        expected_certificate_signers_party_ids.sort();
        let mut found_certificate_signers_party_ids = certificate
            .clone()
            .metadata
            .signers
            .iter()
            .map(|s| s.party_id.clone())
            .collect::<Vec<ProtocolPartyId>>();
        found_certificate_signers_party_ids.sort();
        assert_eq!(
            expected_certificate_signers_party_ids,
            found_certificate_signers_party_ids
        );
        let avk_expected = certificate.aggregate_verification_key;
        let avk_computed = multi_signer
            .compute_stake_distribution_aggregate_verification_key()
            .await
            .expect("compute stake distribution aggregate verification key should not fail")
            .expect(
                "compute stake distribution aggregate verification key should return something",
            );
        assert_eq!(avk_expected, avk_computed);
    }
}
