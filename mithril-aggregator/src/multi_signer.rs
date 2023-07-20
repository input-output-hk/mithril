use async_trait::async_trait;
use slog_scope::{debug, warn};
use std::sync::Arc;
use thiserror::Error;

use mithril_common::{
    crypto_helper::{
        key_encode_hex, ProtocolAggregateVerificationKey, ProtocolAggregationError,
        ProtocolMultiSignature, ProtocolParameters, ProtocolRegistrationError,
        ProtocolStakeDistribution,
    },
    entities::{self, Epoch, SignerWithStake, StakeDistribution},
    protocol::{MultiSigner as ProtocolMultiSigner, SignerBuilder},
    store::{StakeStorer, StoreError},
};

use crate::{entities::OpenMessage, store::VerificationKeyStorer, ProtocolParametersStorer};

#[cfg(test)]
use mockall::automock;

/// Error type for multi signer service.
#[derive(Error, Debug)]
pub enum ProtocolError {
    /// Signer is already registered.
    #[error("signer already registered")]
    ExistingSigner(),

    /// Signer was not registered.
    #[error("signer did not register")]
    UnregisteredParty(),

    /// Signer registration failed.
    #[error("signer registration failed")]
    FailedSignerRegistration(#[from] ProtocolRegistrationError),

    /// Single signature already recorded.
    #[error("single signature already recorded")]
    ExistingSingleSignature(entities::PartyId),

    /// Codec error.
    #[error("codec error: '{0}'")]
    Codec(String),

    /// Mithril STM library returned an error.
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
    ) -> Result<ProtocolAggregateVerificationKey, ProtocolError>;

    /// Compute stake distribution aggregate verification key
    async fn compute_stake_distribution_aggregate_verification_key(
        &self,
    ) -> Result<String, ProtocolError> {
        let signers_with_stake = self.get_signers_with_stake().await?;
        let protocol_parameters = self
            .get_protocol_parameters()
            .await?
            .ok_or_else(ProtocolError::UnavailableProtocolParameters)?;
        let avk = self
            .compute_aggregate_verification_key(&signers_with_stake, &protocol_parameters)
            .await?;
        Ok(key_encode_hex(avk).map_err(ProtocolError::Codec)?)
    }

    /// Compute next stake distribution aggregate verification key
    async fn compute_next_stake_distribution_aggregate_verification_key(
        &self,
    ) -> Result<String, ProtocolError> {
        let next_signers_with_stake = self.get_next_signers_with_stake().await?;
        let protocol_parameters = self
            .get_next_protocol_parameters()
            .await?
            .ok_or_else(ProtocolError::UnavailableProtocolParameters)?;
        let next_avk = self
            .compute_aggregate_verification_key(&next_signers_with_stake, &protocol_parameters)
            .await?;
        Ok(key_encode_hex(next_avk).map_err(ProtocolError::Codec)?)
    }

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
    async fn get_signers_with_stake(&self) -> Result<Vec<SignerWithStake>, ProtocolError>;

    /// Get signers for the next epoch with their stake
    async fn get_next_signers_with_stake(&self) -> Result<Vec<SignerWithStake>, ProtocolError>;

    /// Verify a single signature
    async fn verify_single_signature(
        &self,
        message: &entities::ProtocolMessage,
        signatures: &entities::SingleSignatures,
    ) -> Result<(), ProtocolError>;

    /// Creates a multi signature from single signatures
    async fn create_multi_signature(
        &self,
        open_message: &OpenMessage,
    ) -> Result<Option<ProtocolMultiSignature>, ProtocolError>;
}

/// MultiSignerImpl is an implementation of the MultiSigner
pub struct MultiSignerImpl {
    /// Beacon that is currently used
    current_beacon: Option<entities::Beacon>,

    /// Verification key store
    verification_key_store: Arc<dyn VerificationKeyStorer>,

    /// Stake store
    stake_store: Arc<dyn StakeStorer>,

    /// Protocol parameters store
    protocol_parameters_store: Arc<dyn ProtocolParametersStorer>,
}

impl MultiSignerImpl {
    /// MultiSignerImpl factory
    pub fn new(
        verification_key_store: Arc<dyn VerificationKeyStorer>,
        stake_store: Arc<dyn StakeStorer>,
        protocol_parameters_store: Arc<dyn ProtocolParametersStorer>,
    ) -> Self {
        debug!("New MultiSignerImpl created");
        Self {
            current_beacon: None,
            verification_key_store,
            stake_store,
            protocol_parameters_store,
        }
    }

    /// Creates a protocol multi signer
    pub fn create_protocol_multi_signer(
        &self,
        signers_with_stake: &[SignerWithStake],
        protocol_parameters: &ProtocolParameters,
    ) -> Result<ProtocolMultiSigner, ProtocolError> {
        debug!("Create protocol_multi_signer");

        let protocol_multi_signer =
            SignerBuilder::new(signers_with_stake, &(*protocol_parameters).into())
                .map_err(|error| ProtocolError::Core(error.to_string()))?
                .build_multi_signer();

        Ok(protocol_multi_signer)
    }

    /// Get the [stake distribution][ProtocolStakeDistribution] for the given `epoch`
    async fn get_stake_distribution_at_epoch(
        &self,
        epoch: Epoch,
    ) -> Result<ProtocolStakeDistribution, ProtocolError> {
        debug!("Get stake distribution at epoch"; "epoch"=> #?epoch);

        let stakes = self
            .stake_store
            .get_stakes(epoch)
            .await?
            .unwrap_or_default();
        Ok(stakes.into_iter().collect::<ProtocolStakeDistribution>())
    }

    /// Get the [protocol parameters][ProtocolParameters] for the given `epoch`
    async fn get_protocol_parameters_at_epoch(
        &self,
        epoch: Epoch,
    ) -> Result<Option<ProtocolParameters>, ProtocolError> {
        debug!("Get protocol parameters at epoch"; "epoch"=> #?epoch);

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
        let epoch = self
            .current_beacon
            .as_ref()
            .ok_or_else(ProtocolError::UnavailableBeacon)?
            .epoch
            .offset_to_signer_retrieval_epoch()?;
        self.get_protocol_parameters_at_epoch(epoch).await
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
            .offset_to_protocol_parameters_recording_epoch();

        self.protocol_parameters_store
            .save_protocol_parameters(epoch, protocol_parameters.to_owned().into())
            .await?;
        Ok(())
    }

    /// Get next protocol parameters
    async fn get_next_protocol_parameters(
        &self,
    ) -> Result<Option<ProtocolParameters>, ProtocolError> {
        debug!("Get next protocol parameters");
        let epoch = self
            .current_beacon
            .as_ref()
            .ok_or_else(ProtocolError::UnavailableBeacon)?
            .epoch
            .offset_to_next_signer_retrieval_epoch();
        self.get_protocol_parameters_at_epoch(epoch).await
    }

    /// Get stake distribution
    async fn get_stake_distribution(&self) -> Result<ProtocolStakeDistribution, ProtocolError> {
        debug!("Get stake distribution");
        let epoch = self
            .current_beacon
            .as_ref()
            .ok_or_else(ProtocolError::UnavailableBeacon)?
            .epoch
            .offset_to_signer_retrieval_epoch()?;
        self.get_stake_distribution_at_epoch(epoch).await
    }

    /// Get next stake distribution
    async fn get_next_stake_distribution(
        &self,
    ) -> Result<ProtocolStakeDistribution, ProtocolError> {
        debug!("Get next stake distribution");
        let epoch = self
            .current_beacon
            .as_ref()
            .ok_or_else(ProtocolError::UnavailableBeacon)?
            .epoch
            .offset_to_next_signer_retrieval_epoch();
        self.get_stake_distribution_at_epoch(epoch).await
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
            .offset_to_recording_epoch();
        let stakes = StakeDistribution::from_iter(stakes.iter().cloned());
        self.stake_store.save_stakes(epoch, stakes).await?;

        Ok(())
    }

    /// Compute aggregate verification key from stake distribution
    async fn compute_aggregate_verification_key(
        &self,
        signers_with_stakes: &[SignerWithStake],
        protocol_parameters: &ProtocolParameters,
    ) -> Result<ProtocolAggregateVerificationKey, ProtocolError> {
        let protocol_multi_signer =
            self.create_protocol_multi_signer(signers_with_stakes, protocol_parameters)?;
        Ok(protocol_multi_signer.compute_aggregate_verification_key())
    }

    async fn get_signers_with_stake(&self) -> Result<Vec<SignerWithStake>, ProtocolError> {
        debug!("Get signers with stake");
        let epoch = self
            .current_beacon
            .as_ref()
            .ok_or_else(ProtocolError::UnavailableBeacon)?
            .epoch
            .offset_to_signer_retrieval_epoch()?;
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
                    SignerWithStake::new(
                        party_id.to_owned(),
                        signer.verification_key.to_owned(),
                        signer.verification_key_signature.to_owned(),
                        signer.operational_certificate.to_owned(),
                        signer.kes_period.to_owned(),
                        *stake,
                    )
                })
            })
            .collect())
    }

    async fn get_next_signers_with_stake(&self) -> Result<Vec<SignerWithStake>, ProtocolError> {
        debug!("Get next signers with stake");
        let epoch = self
            .current_beacon
            .as_ref()
            .ok_or_else(ProtocolError::UnavailableBeacon)?
            .epoch
            .offset_to_next_signer_retrieval_epoch();
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
                    SignerWithStake::new(
                        party_id.to_owned(),
                        signer.verification_key.to_owned(),
                        signer.verification_key_signature.to_owned(),
                        signer.operational_certificate.to_owned(),
                        signer.kes_period.to_owned(),
                        *stake,
                    )
                })
            })
            .collect())
    }

    /// Verify a single signature
    async fn verify_single_signature(
        &self,
        message: &entities::ProtocolMessage,
        single_signature: &entities::SingleSignatures,
    ) -> Result<(), ProtocolError> {
        debug!(
            "Verify single signature from {} at indexes {:?} for message {:?}",
            single_signature.party_id, single_signature.won_indexes, message
        );

        let protocol_parameters = self
            .get_protocol_parameters()
            .await?
            .ok_or_else(ProtocolError::UnavailableProtocolParameters)?;

        let signers_with_stakes = self.get_signers_with_stake().await?;

        let protocol_multi_signer =
            self.create_protocol_multi_signer(&signers_with_stakes, &protocol_parameters)?;

        protocol_multi_signer
            .verify_single_signature(message, single_signature)
            .map_err(|error| ProtocolError::Core(error.to_string()))
    }

    /// Creates a multi signature from single signatures
    async fn create_multi_signature(
        &self,
        open_message: &OpenMessage,
    ) -> Result<Option<ProtocolMultiSignature>, ProtocolError> {
        debug!("MultiSigner:create_multi_signature({open_message:?})");
        let protocol_parameters = self
            .get_protocol_parameters()
            .await?
            .ok_or_else(ProtocolError::UnavailableProtocolParameters)?;

        let signers_with_stakes = self.get_signers_with_stake().await?;

        let protocol_multi_signer =
            self.create_protocol_multi_signer(&signers_with_stakes, &protocol_parameters)?;

        match protocol_multi_signer.aggregate_single_signatures(
            &open_message.single_signatures,
            &open_message.protocol_message,
        ) {
            Ok(multi_signature) => Ok(Some(multi_signature)),
            Err(ProtocolAggregationError::NotEnoughSignatures(actual, expected)) => {
                warn!("Could not compute multi-signature: Not enough signatures. Got only {} out of {}.", actual, expected);
                Ok(None)
            }
            Err(err) => Err(ProtocolError::Core(err.to_string())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{store::VerificationKeyStore, ProtocolParametersStore};
    use mithril_common::{
        crypto_helper::tests_setup::*,
        entities::{PartyId, SignedEntityType},
        store::{adapter::MemoryAdapter, StakeStore},
        test_utils::{fake_data, MithrilFixtureBuilder},
    };
    use std::{collections::HashMap, sync::Arc};

    async fn setup_multi_signer() -> MultiSignerImpl {
        let beacon = fake_data::beacon();
        let verification_key_store = VerificationKeyStore::new(Box::new(
            MemoryAdapter::<Epoch, HashMap<PartyId, SignerWithStake>>::new(None).unwrap(),
        ));
        let stake_store = StakeStore::new(
            Box::new(MemoryAdapter::<Epoch, StakeDistribution>::new(None).unwrap()),
            None,
        );
        let protocol_parameters_store = ProtocolParametersStore::new(
            Box::new(
                MemoryAdapter::<Epoch, entities::ProtocolParameters>::new(Some(vec![
                    (
                        beacon.epoch.offset_to_signer_retrieval_epoch().unwrap(),
                        fake_data::protocol_parameters(),
                    ),
                    (
                        beacon.epoch.offset_to_next_signer_retrieval_epoch(),
                        fake_data::protocol_parameters(),
                    ),
                    (
                        beacon.epoch.offset_to_next_signer_retrieval_epoch().next(),
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
    async fn test_multi_signer_protocol_parameters_ok() {
        let mut multi_signer = setup_multi_signer().await;

        let protocol_parameters_expected = setup_protocol_parameters();
        multi_signer
            .update_protocol_parameters(&protocol_parameters_expected)
            .await
            .expect("update protocol parameters failed");

        offset_epoch(
            &mut multi_signer,
            Epoch::SIGNER_RECORDING_OFFSET as i64 - Epoch::SIGNER_RETRIEVAL_OFFSET,
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
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let mut stake_distribution_expected = fixture.protocol_stake_distribution();

        stake_distribution_expected.sort_by_key(|k| k.0.clone());
        multi_signer
            .update_stake_distribution(&stake_distribution_expected)
            .await
            .expect("update stake distribution failed");

        offset_epoch(
            &mut multi_signer,
            Epoch::SIGNER_RECORDING_OFFSET as i64 - Epoch::SIGNER_RETRIEVAL_OFFSET,
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
    async fn test_multi_signer_multi_signature_ok() {
        let mut multi_signer = setup_multi_signer().await;
        let verification_key_store = multi_signer.verification_key_store.clone();
        let start_epoch = multi_signer.current_beacon.as_ref().unwrap().epoch;

        let message = setup_message();
        let protocol_parameters = setup_protocol_parameters();
        multi_signer
            .update_protocol_parameters(&protocol_parameters)
            .await
            .expect("update protocol parameters failed");

        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();

        let stake_distribution = fixture.protocol_stake_distribution();

        multi_signer
            .update_stake_distribution(&stake_distribution)
            .await
            .expect("update stake distribution failed");
        for signer_with_stake in &fixture.signers_with_stake() {
            verification_key_store
                .save_verification_key(
                    start_epoch.offset_to_recording_epoch(),
                    signer_with_stake.to_owned(),
                )
                .await
                .expect("register should have succeeded");
        }

        offset_epoch(
            &mut multi_signer,
            Epoch::SIGNER_RECORDING_OFFSET as i64 - Epoch::SIGNER_RETRIEVAL_OFFSET,
        )
        .await;

        let mut signatures = Vec::new();

        let mut expected_certificate_signers: Vec<SignerWithStake> = Vec::new();
        for signer_fixture in fixture.signers_fixture() {
            if let Some(signature) = signer_fixture
                .protocol_signer
                .sign(message.compute_hash().as_bytes())
            {
                let won_indexes = signature.indexes.clone();

                signatures.push(entities::SingleSignatures::new(
                    signer_fixture.signer_with_stake.party_id.to_owned(),
                    key_encode_hex(signature).unwrap(),
                    won_indexes,
                ));

                expected_certificate_signers.push(signer_fixture.signer_with_stake.to_owned())
            }
        }

        for signature in &signatures {
            multi_signer
                .verify_single_signature(&message, signature)
                .await
                .expect("single signature should be valid");
        }

        let signatures_to_almost_reach_quorum = take_signatures_until_quorum_is_almost_reached(
            &mut signatures,
            protocol_parameters.k as usize,
        );
        assert!(
            !signatures_to_almost_reach_quorum.is_empty(),
            "they should be at least one signature that can be registered without reaching the quorum"
        );

        let mut open_message = OpenMessage {
            epoch: start_epoch,
            signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(
                multi_signer.current_beacon.clone().unwrap(),
            ),
            protocol_message: message.clone(),
            is_certified: false,
            single_signatures: Vec::new(),
            ..OpenMessage::dummy()
        };

        // No signatures registered: multi-signer can't create the multi-signature
        assert!(multi_signer
            .create_multi_signature(&open_message)
            .await
            .expect("create multi signature should not fail")
            .is_none());

        // Add some signatures but not enough to reach the quorum: multi-signer should not create the multi-signature
        open_message.single_signatures = signatures_to_almost_reach_quorum;

        assert!(multi_signer
            .create_multi_signature(&open_message)
            .await
            .expect("create multi signature should not fail")
            .is_none());

        // Add the remaining signatures to reach the quorum: multi-signer should create a multi-signature
        open_message.single_signatures.append(&mut signatures);

        assert!(
            multi_signer
                .create_multi_signature(&open_message)
                .await
                .expect("create multi signature should not fail")
                .is_some(),
            "no multi-signature were computed"
        );
    }
}
