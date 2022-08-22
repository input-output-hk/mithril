use hex::ToHex;
use slog_scope::{info, trace, warn};
use thiserror::Error;

use mithril_common::crypto_helper::{
    key_decode_hex, key_encode_hex, ProtocolClerk, ProtocolInitializer, ProtocolKeyRegistration,
    ProtocolSigner,
};
use mithril_common::entities::{
    PartyId, ProtocolMessage, ProtocolParameters, SignerWithStake, SingleSignatures, Stake,
};

#[cfg(test)]
use mockall::automock;

use crate::AsyncError;

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
    ) -> Result<ProtocolInitializer, AsyncError> {
        let mut rng = rand_core::OsRng;
        let protocol_initializer = ProtocolInitializer::setup(
            protocol_parameters.to_owned().into(),
            stake.to_owned(),
            &mut rng,
        );

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

    /// Create a cryptographic signer.
    fn create_protocol_signer(
        &self,
        signers_with_stake: &[SignerWithStake],
        protocol_initializer: &ProtocolInitializer,
    ) -> Result<ProtocolSigner, SingleSignerError> {
        let mut key_reg = ProtocolKeyRegistration::init();
        let signers = signers_with_stake
            .iter()
            .filter(|signer| !signer.verification_key.is_empty())
            .collect::<Vec<&SignerWithStake>>();

        if signers.is_empty() {
            return Err(SingleSignerError::ProtocolSignerCreationFailure(
                "no signer".to_string(),
            ));
        }

        for s in signers {
            let decoded_key =
                key_decode_hex(&s.verification_key).map_err(SingleSignerError::Codec)?;
            key_reg
                .register(s.stake, decoded_key)
                .map_err(|e| SingleSignerError::ProtocolSignerCreationFailure(e.to_string()))?;
        }
        let closed_reg = key_reg.close();

        Ok(protocol_initializer.to_owned().new_signer(closed_reg))
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
        match self.create_protocol_signer(signers_with_stake, protocol_initializer) {
            Ok(protocol_signer) => {
                let clerk = ProtocolClerk::from_signer(&protocol_signer);
                Ok(Some(
                    key_encode_hex(clerk.compute_avk()).map_err(SingleSignerError::Codec)?,
                ))
            }
            Err(SingleSignerError::ProtocolSignerCreationFailure(err)) => {
                warn!("compute_aggregate_verification_key::protocol_signer_creation_failure"; "error" => err);
                Ok(None)
            }
            Err(e) => Err(e),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use mithril_common::crypto_helper::tests_setup::*;
    use mithril_common::crypto_helper::{key_decode_hex, ProtocolClerk, ProtocolSingleSignature};
    use mithril_common::entities::ProtocolMessagePartKey;

    #[test]
    fn compute_single_signature_success() {
        let snapshot_digest = "digest".to_string();
        let protocol_parameters = setup_protocol_parameters();
        let signers = setup_signers(5);
        let signers_with_stake = signers
            .iter()
            .map(
                |(party_id, stake, verification_key, _protocol_signer, _protocol_initializer)| {
                    SignerWithStake::new(
                        party_id.to_owned(),
                        key_encode_hex(verification_key).unwrap(),
                        *stake,
                    )
                },
            )
            .collect::<Vec<SignerWithStake>>();
        let current_signer = &signers[0];
        let single_signer = MithrilSingleSigner::new(current_signer.0.to_owned());
        let protocol_signer = &current_signer.3;
        let clerk = ProtocolClerk::from_signer(protocol_signer);
        let avk = clerk.compute_avk();
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(ProtocolMessagePartKey::SnapshotDigest, snapshot_digest);
        let expected_message = protocol_message.compute_hash().as_bytes().to_vec();

        let sign_result = single_signer
            .compute_single_signatures(&protocol_message, &signers_with_stake, &current_signer.4)
            .expect("single signer should not fail")
            .expect("single signer should produce a signature here");

        let decoded_sig: ProtocolSingleSignature = key_decode_hex(&sign_result.signature).unwrap();
        assert!(
            decoded_sig
                .verify(&protocol_parameters, &avk, &expected_message)
                .is_ok(),
            "produced single signature should be valid"
        );
        //TODO: decoded_sig.pk should probably be a StmVerificationKeyPoP, uncomment once fixed
        //assert_eq!(current_signer.2, decoded_sig.pk);
    }

    #[test]
    fn compute_aggregate_verification_key_success() {
        let signers = setup_signers(5);
        let signers_with_stake = signers
            .iter()
            .map(
                |(party_id, stake, verification_key, _protocol_signer, _protocol_initializer)| {
                    SignerWithStake::new(
                        party_id.to_owned(),
                        key_encode_hex(verification_key).unwrap(),
                        *stake,
                    )
                },
            )
            .collect::<Vec<SignerWithStake>>();
        let current_signer = &signers[0];
        let single_signer = MithrilSingleSigner::new(current_signer.0.to_owned());
        let protocol_initializer = &current_signer.4;
        let avk = single_signer
            .compute_aggregate_verification_key(&signers_with_stake, protocol_initializer)
            .expect("compute aggregate verification signature should not fail")
            .expect("aggregate verification signature should not be empty");

        assert_eq!("7b226d745f636f6d6d69746d656e74223a7b22726f6f74223a5b38332c3132352c3230332c37332c3139372c3233312c3136312c37372c37332c37342c37392c34322c312c3138302c3230352c36302c3138392c32312c3139392c3136382c3134322c3131392c33382c31302c3139372c3233372c39392c36392c37302c3133312c3131322c36352c36392c31342c3234362c3132302c3136362c3131372c34372c31392c3137362c3135372c3131382c39352c3131352c3139322c3134322c3138382c3138302c3133392c34302c3137392c3138392c35382c35372c3137352c32362c39342c3234342c3130332c3230302c3135302c3131322c3136375d2c22686173686572223a6e756c6c7d2c22746f74616c5f7374616b65223a323438367d", avk);
    }
}
