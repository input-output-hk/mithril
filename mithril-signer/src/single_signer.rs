use hex::ToHex;
use slog_scope::{info, trace};
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

pub struct MithrilProtocolInitializerBuilder {
    party_id: PartyId,
}

impl MithrilProtocolInitializerBuilder {
    pub fn new(party_id: PartyId) -> Self {
        Self { party_id }
    }

    pub fn build(
        &self,
        stake: &Stake,
        protocol_parameters: &ProtocolParameters,
    ) -> Result<ProtocolInitializer, AsyncError> {
        // TODO: Since the stake distribution is now updated, we can't cache only one protocol initializer
        // When the protocol initalizer store is implemented, we should get the protocol initializer based on its associated epoch
        // The use of this cache leads to 'The path of the Merkle Tree is invalid.' error when the signer creates a single signature
        // and is source of flakiness of the CI

        // TODO: Uncomment next line and remove the 4 following lines with deterministic random generator when the protocol initializer store is created
        //let mut rng = rand_core::OsRng;
        use rand_chacha::ChaCha20Rng;
        use rand_core::SeedableRng;
        // 32 chars are appended after the party ID to ensure the length is at least 32 while still grants some uniqueness
        let seed: [u8; 32] = format!("{}azerazerazerazerazerazerazerazer", self.party_id)
            .as_bytes()[..32]
            .try_into()?;
        let mut rng = ChaCha20Rng::from_seed(seed);
        //
        let protocol_initializer = ProtocolInitializer::setup(
            protocol_parameters.to_owned().into(),
            stake.to_owned(),
            &mut rng,
        );

        Ok(protocol_initializer)
    }
}

#[cfg_attr(test, automock)]
pub trait SingleSigner: Sync + Send {
    /// Computes single signatures
    fn compute_single_signatures(
        &self,
        protocol_message: &ProtocolMessage,
        signers_with_stake: &Vec<SignerWithStake>,
        protocol_initializer: &ProtocolInitializer,
    ) -> Result<Option<SingleSignatures>, SingleSignerError>;

    /// Compute aggregate verification key from stake distribution
    fn compute_aggregate_verification_key(
        &self,
        signers_with_stake: &Vec<SignerWithStake>,
        protocol_initializer: &ProtocolInitializer,
    ) -> Result<Option<String>, SingleSignerError>;
}

#[derive(Error, Debug, PartialEq)]
pub enum SingleSignerError {
    #[error("the signer verification key is not registered in the stake distribution")]
    UnregisteredVerificationKey(),

    #[error("the signer party id is not registered in the stake distribution")]
    UnregisteredPartyId(),

    #[error("the protocol signer creation failed: `{0}`")]
    ProtocolSignerCreationFailure(String),

    #[error("the protocol initializer is missing")]
    ProtocolInitializerMissing(),

    #[error("codec error: '{0}'")]
    Codec(String),
}

pub struct MithrilSingleSigner {
    party_id: PartyId,
}

impl MithrilSingleSigner {
    pub fn new(party_id: PartyId) -> Self {
        Self { party_id }
    }

    fn create_protocol_signer(
        &self,
        signers_with_stake: &Vec<SignerWithStake>,
        protocol_initializer: &ProtocolInitializer,
    ) -> Result<ProtocolSigner, SingleSignerError> {
        let mut key_reg = ProtocolKeyRegistration::init();
        let signers = signers_with_stake
            .iter()
            .filter(|signer| !signer.verification_key.is_empty())
            .collect::<Vec<&SignerWithStake>>();

        if signers.len() == 0 {
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
        signers_with_stake: &Vec<SignerWithStake>,
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
            None => Ok(None),
        }
    }

    /// Compute aggregate verification key from stake distribution
    fn compute_aggregate_verification_key(
        &self,
        signers_with_stake: &Vec<SignerWithStake>,
        protocol_initializer: &ProtocolInitializer,
    ) -> Result<Option<String>, SingleSignerError> {
        match self.create_protocol_signer(signers_with_stake, protocol_initializer) {
            Ok(protocol_signer) => {
                let clerk = ProtocolClerk::from_signer(&protocol_signer);
                Ok(Some(
                    key_encode_hex(clerk.compute_avk()).map_err(SingleSignerError::Codec)?,
                ))
            }
            Err(SingleSignerError::ProtocolSignerCreationFailure(_)) => Ok(None),
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
        protocol_message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            snapshot_digest.clone(),
        );
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

        assert_eq!("7b226d745f636f6d6d69746d656e74223a7b22726f6f74223a5b3133392c3136312c31362c322c3235352c3137392c33382c36342c3231312c34322c31392c36322c3133312c3132352c3230382c3134382c3230362c32372c31362c39382c3234372c372c3138362c3230352c3133352c39352c3134352c3130312c39332c3233362c33312c32332c3132372c322c3232302c3135302c3137322c35372c36342c3133382c3136362c3132302c3137372c3132372c3133342c3137342c33332c3134382c3230322c3134342c3132372c3135372c3131302c392c38332c31332c33342c36302c33392c3230372c33352c3134342c332c36335d2c22686173686572223a6e756c6c7d2c22746f74616c5f7374616b65223a343133307d", avk);
    }
}
