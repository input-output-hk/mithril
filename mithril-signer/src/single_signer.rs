use hex::ToHex;
use slog_scope::{info, trace, warn};
use std::path::PathBuf;
use thiserror::Error;

use mithril_common::crypto_helper::{
    key_decode_hex, key_encode_hex, KESPeriod, ProtocolClerk, ProtocolInitializer,
    ProtocolKeyRegistration, ProtocolPartyId, ProtocolRegistrationError, ProtocolSigner,
    ProtocolStakeDistribution,
};
use mithril_common::entities::{
    PartyId, ProtocolMessage, ProtocolParameters, SignerWithStake, SingleSignatures, Stake,
};

#[cfg(test)]
use mockall::automock;

/// MithrilProtocolInitializerBuilder error structure.
#[derive(Error, Debug)]
pub enum MithrilProtocolInitializerBuilderError {
    /// Could not parse a Cardano crypto file
    #[error("the cardano cryptographic file could not be parsed.")]
    CardanoCryptoParse,
}

/// This is responsible of creating new instances of ProtocolInitializer.
#[derive(Default)]
pub struct MithrilProtocolInitializerBuilder {}

impl MithrilProtocolInitializerBuilder {
    /// Create a new MithrilProtocolInitializerBuilder instance.
    pub fn new() -> Self {
        Self {}
    }

    /// Create a ProtocolInitializer instance.
    pub fn build(
        &self,
        stake: &Stake,
        protocol_parameters: &ProtocolParameters,
        kes_secret_key_path: Option<PathBuf>,
        kes_period: Option<KESPeriod>,
    ) -> Result<ProtocolInitializer, MithrilProtocolInitializerBuilderError> {
        let mut rng = rand_core::OsRng;
        let protocol_initializer = ProtocolInitializer::setup(
            protocol_parameters.to_owned().into(),
            kes_secret_key_path,
            kes_period,
            stake.to_owned(),
            &mut rng,
        )
        .map_err(|_| MithrilProtocolInitializerBuilderError::CardanoCryptoParse)?;

        Ok(protocol_initializer)
    }
}

/// The SingleSigner is the structure responsible of issuing SingleSignatures.
#[cfg_attr(test, automock)]
pub trait SingleSigner: Sync + Send {
    /// Computes single signatures
    fn compute_single_signatures(
        &self,
        protocol_message: &ProtocolMessage,
        signers_with_stake: &[SignerWithStake],
        protocol_initializer: &ProtocolInitializer,
    ) -> Result<Option<SingleSignatures>, SingleSignerError>;

    /// Compute aggregate verification key from stake distribution
    fn compute_aggregate_verification_key(
        &self,
        signers_with_stake: &[SignerWithStake],
        protocol_initializer: &ProtocolInitializer,
    ) -> Result<Option<String>, SingleSignerError>;

    /// Get party id
    fn get_party_id(&self) -> ProtocolPartyId;
}

/// SingleSigner error structure.
#[derive(Error, Debug, PartialEq, Eq)]
pub enum SingleSignerError {
    /// This signer has not registered for this Epoch hence cannot participate to the signature.
    #[error("the signer verification key is not registered in the stake distribution")]
    UnregisteredVerificationKey(),

    /// No stake is associated with this signer.
    #[error("the signer party id is not registered in the stake distribution")]
    UnregisteredPartyId(),

    /// Cryptographic Signer creation error.
    #[error("the protocol signer creation failed: `{0}`")]
    ProtocolSignerCreationFailure(String),

    /// Could not fetch a protocol initializer for this Epoch.
    #[error("the protocol initializer is missing")]
    ProtocolInitializerMissing(),

    /// Could not fetch a signer from a protocol initializer.
    #[error("the protocol initializer is not registered")]
    ProtocolInitializerNotRegistered(#[from] ProtocolRegistrationError),

    /// Encoding / Decoding error.
    #[error("codec error: '{0}'")]
    Codec(String),
}

/// Implementation of the SingleSigner.
pub struct MithrilSingleSigner {
    party_id: PartyId,
}

impl MithrilSingleSigner {
    /// Create a new instance of the MithrilSingleSigner.
    pub fn new(party_id: PartyId) -> Self {
        Self { party_id }
    }

    /// Create a protocol key registration.
    fn create_protocol_key_registration(
        &self,
        signers_with_stake: &[SignerWithStake],
    ) -> Result<ProtocolKeyRegistration, SingleSignerError> {
        let signers = signers_with_stake
            .iter()
            .filter(|signer| !signer.verification_key.is_empty())
            .collect::<Vec<&SignerWithStake>>();

        if signers.is_empty() {
            return Err(SingleSignerError::ProtocolSignerCreationFailure(
                "no signer".to_string(),
            ));
        }

        let stake_distribution = signers
            .iter()
            .map(|&s| s.into())
            .collect::<ProtocolStakeDistribution>();
        let mut key_reg = ProtocolKeyRegistration::init(&stake_distribution);
        for s in signers {
            let operational_certificate = match &s.operational_certificate {
                Some(operational_certificate) => {
                    key_decode_hex(operational_certificate).map_err(SingleSignerError::Codec)?
                }
                _ => None,
            };
            let verification_key =
                key_decode_hex(&s.verification_key).map_err(SingleSignerError::Codec)?;
            let kes_signature = match &s.verification_key_signature {
                Some(verification_key_signature) => Some(
                    key_decode_hex(verification_key_signature).map_err(SingleSignerError::Codec)?,
                ),
                _ => None,
            };
            let kes_period = s.kes_period;
            key_reg
                .register(
                    Some(s.party_id.to_owned()),
                    operational_certificate,
                    kes_signature,
                    kes_period,
                    verification_key,
                )
                .map_err(|e| SingleSignerError::ProtocolSignerCreationFailure(e.to_string()))?;
        }
        Ok(key_reg)
    }

    /// Create a protocol signer.
    fn create_protocol_signer(
        &self,
        signers_with_stake: &[SignerWithStake],
        protocol_initializer: &ProtocolInitializer,
    ) -> Result<ProtocolSigner, SingleSignerError> {
        let key_reg = self.create_protocol_key_registration(signers_with_stake)?;
        let closed_reg = key_reg.close();

        Ok(protocol_initializer.to_owned().new_signer(closed_reg)?)
    }

    /// Create a cryptographic clerk.
    fn create_protocol_clerk(
        &self,
        signers_with_stake: &[SignerWithStake],
        protocol_initializer: &ProtocolInitializer,
    ) -> Result<ProtocolClerk, SingleSignerError> {
        let key_reg = self.create_protocol_key_registration(signers_with_stake)?;
        let closed_reg = key_reg.close();

        Ok(ProtocolClerk::from_registration(
            &protocol_initializer.get_protocol_parameters(),
            &closed_reg,
        ))
    }
}

impl SingleSigner for MithrilSingleSigner {
    fn compute_single_signatures(
        &self,
        protocol_message: &ProtocolMessage,
        signers_with_stake: &[SignerWithStake],
        protocol_initializer: &ProtocolInitializer,
    ) -> Result<Option<SingleSignatures>, SingleSignerError> {
        let protocol_signer =
            self.create_protocol_signer(signers_with_stake, protocol_initializer)?;
        let message = protocol_message.compute_hash().as_bytes().to_vec();

        info!("Signing protocol message"; "protocol_message" =>  #?protocol_message, "signed message" => protocol_message.compute_hash().encode_hex::<String>());

        match protocol_signer.sign(&message) {
            Some(signature) => {
                trace!(
                    "Party #{}: lottery #{:?} won",
                    self.party_id,
                    &signature.indexes
                );
                let encoded_signature =
                    key_encode_hex(&signature).map_err(SingleSignerError::Codec)?;
                let won_indexes = signature.indexes;

                Ok(Some(SingleSignatures::new(
                    self.party_id.clone(),
                    encoded_signature,
                    won_indexes,
                )))
            }
            None => {
                warn!("no signature computed, all lotteries were lost");
                Ok(None)
            }
        }
    }

    /// Compute aggregate verification key from stake distribution
    fn compute_aggregate_verification_key(
        &self,
        signers_with_stake: &[SignerWithStake],
        protocol_initializer: &ProtocolInitializer,
    ) -> Result<Option<String>, SingleSignerError> {
        match self.create_protocol_clerk(signers_with_stake, protocol_initializer) {
            Ok(clerk) => Ok(Some(
                key_encode_hex(clerk.compute_avk()).map_err(SingleSignerError::Codec)?,
            )),
            Err(SingleSignerError::ProtocolSignerCreationFailure(err)) => {
                warn!("compute_aggregate_verification_key::protocol_signer_creation_failure"; "error" => err);
                Ok(None)
            }
            Err(e) => Err(e),
        }
    }

    /// Get party id
    fn get_party_id(&self) -> ProtocolPartyId {
        self.party_id.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use mithril_common::{
        crypto_helper::{key_decode_hex, tests_setup::*, ProtocolClerk, ProtocolSingleSignature},
        entities::ProtocolMessagePartKey,
        test_utils::MithrilFixtureBuilder,
    };

    #[test]
    fn compute_single_signature_success() {
        let snapshot_digest = "digest".to_string();
        let protocol_parameters = setup_protocol_parameters();
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signers_with_stake = fixture.signers_with_stake();
        let current_signer = &fixture.signers_fixture()[0];
        let single_signer =
            MithrilSingleSigner::new(current_signer.signer_with_stake.party_id.to_owned());
        let clerk = ProtocolClerk::from_signer(&current_signer.protocol_signer);
        let avk = clerk.compute_avk();
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(ProtocolMessagePartKey::SnapshotDigest, snapshot_digest);
        let expected_message = protocol_message.compute_hash().as_bytes().to_vec();

        let sign_result = single_signer
            .compute_single_signatures(
                &protocol_message,
                &signers_with_stake,
                &current_signer.protocol_initializer,
            )
            .expect("single signer should not fail")
            .expect("single signer should produce a signature here");

        let decoded_sig: ProtocolSingleSignature = key_decode_hex(&sign_result.signature).unwrap();
        assert!(
            decoded_sig
                .verify(
                    &protocol_parameters,
                    &current_signer.protocol_signer.verification_key(),
                    &current_signer.protocol_signer.get_stake(),
                    &avk,
                    &expected_message
                )
                .is_ok(),
            "produced single signature should be valid"
        );
    }

    #[test]
    fn compute_aggregate_verification_key_success() {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signers_with_stake = fixture.signers_with_stake();
        let current_signer = &fixture.signers_fixture()[0];
        let single_signer =
            MithrilSingleSigner::new(current_signer.signer_with_stake.party_id.to_owned());

        single_signer
            .compute_aggregate_verification_key(
                &signers_with_stake,
                &current_signer.protocol_initializer,
            )
            .expect("compute aggregate verification signature should not fail")
            .expect("aggregate verification signature should not be empty");
    }
}
