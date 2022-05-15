use hex::ToHex;
use slog_scope::{trace, warn};
use thiserror::Error;

use mithril_common::crypto_helper::{
    key_decode_hex, key_encode_hex, Bytes, ProtocolInitializer, ProtocolKeyRegistration,
    ProtocolPartyId, ProtocolSigner, ProtocolStake,
};
use mithril_common::entities::{self, SignerWithStake, SingleSignature};

#[cfg(test)]
use mockall::automock;

#[cfg_attr(test, automock)]
pub trait SingleSigner {
    // Get party id
    fn get_party_id(&self) -> ProtocolPartyId;

    // Get currently setup protocol initializer
    fn get_protocol_initializer(&self) -> Option<ProtocolInitializer>;

    /// Computes single signatures
    fn compute_single_signatures(
        &self,
        message: Bytes,
        stake_distribution: Vec<SignerWithStake>,
        protocol_parameters: &entities::ProtocolParameters,
    ) -> Result<Vec<SingleSignature>, SingleSignerError>;
}

#[derive(Error, Debug, PartialEq)]
pub enum SingleSignerError {
    #[error("the signer verification key is not registered in the stake distribution")]
    UnregisteredVerificationKey(),

    #[error("the signer party id is not registered in the stake distribution")]
    UnregisteredPartyId(),

    #[error("the protocol signer creation failed: `{0}`")]
    ProtocolSignerCreationFailure(String),

    #[error("codec error: '{0}'")]
    Codec(String),
}

pub struct MithrilSingleSigner {
    party_id: u64,
    protocol_initializer: Option<ProtocolInitializer>,
}

impl MithrilSingleSigner {
    pub fn new(party_id: u64, protocol_initializer_encoded: &str) -> Self {
        let protocol_initializer = key_decode_hex(protocol_initializer_encoded).ok();
        Self {
            party_id,
            protocol_initializer,
        }
    }

    fn create_protocol_signer(
        &self,
        current_player_stake: ProtocolStake,
        stake_distribution: &[SignerWithStake], // TODO : use a hmap to prevent party id duplication
        protocol_parameters: &entities::ProtocolParameters,
    ) -> Result<ProtocolSigner, SingleSignerError> {
        let players = stake_distribution
            .iter()
            .map(|s| (s.party_id as ProtocolPartyId, s.stake as ProtocolStake))
            .collect::<Vec<_>>();

        let mut key_reg = ProtocolKeyRegistration::init(&players);
        for s in stake_distribution {
            let decoded_key =
                key_decode_hex(&s.verification_key).map_err(SingleSignerError::Codec)?;
            key_reg
                .register(s.party_id as ProtocolPartyId, decoded_key)
                .map_err(|e| SingleSignerError::ProtocolSignerCreationFailure(e.to_string()))?;
        }
        let closed_reg = key_reg.close();

        let protocol_initializer = match &self.protocol_initializer {
            None => {
                let mut rng = rand_core::OsRng;
                ProtocolInitializer::setup(
                    protocol_parameters.to_owned().into(),
                    self.party_id as ProtocolPartyId,
                    current_player_stake,
                    &mut rng,
                )
            }
            Some(protocol_initializer) => protocol_initializer.to_owned(),
        };

        Ok(protocol_initializer.new_signer(closed_reg))
    }
}

impl SingleSigner for MithrilSingleSigner {
    fn get_party_id(&self) -> ProtocolPartyId {
        self.party_id
    }

    fn get_protocol_initializer(&self) -> Option<ProtocolInitializer> {
        self.protocol_initializer.clone()
    }

    fn compute_single_signatures(
        &self,
        message: Bytes,
        stake_distribution: Vec<SignerWithStake>, // TODO : use a hmap to prevent party id duplication
        protocol_parameters: &entities::ProtocolParameters,
    ) -> Result<Vec<SingleSignature>, SingleSignerError> {
        let current_signer_with_stake = stake_distribution
            .iter()
            .find(|s| s.party_id == self.party_id)
            .ok_or(SingleSignerError::UnregisteredPartyId())?;

        if current_signer_with_stake.verification_key.is_empty() {
            return Err(SingleSignerError::UnregisteredVerificationKey());
        }

        let protocol_signer = self.create_protocol_signer(
            current_signer_with_stake.stake,
            &stake_distribution,
            protocol_parameters,
        )?;

        trace!(
            "Party #{}: sign message {}",
            self.party_id,
            message.encode_hex::<String>()
        );

        let mut signatures = Vec::new();
        for i in 1..=protocol_parameters.m {
            if let Some(signature) = protocol_signer.sign(&message, i) {
                trace!("Party #{}: lottery #{} won", self.party_id, i,);
                let encoded_signature = key_encode_hex(&signature);

                if encoded_signature.is_err() {
                    warn!("couldn't compute signature: `{:?}`", encoded_signature); // TODO: structured log
                    continue;
                }

                signatures.push(SingleSignature::new(
                    self.party_id,
                    i,
                    encoded_signature.unwrap(),
                ));
            }
        }
        Ok(signatures)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use mithril_common::crypto_helper::tests_setup::*;
    use mithril_common::crypto_helper::{key_decode_hex, ProtocolClerk};

    #[test]
    fn cant_compute_if_signer_verification_key_is_not_registered() {
        let message = setup_message();
        let protocol_parameters = setup_protocol_parameters();
        let signers = setup_signers(5);
        let stake_distribution = signers[..]
            .iter()
            .map(
                |(party_id, stake, verification_key, _protocol_signer, _protocol_initializer)| {
                    let verification_key = match party_id {
                        0 => "".to_string(),
                        _ => key_encode_hex(verification_key).unwrap(),
                    };
                    SignerWithStake::new(*party_id, verification_key, *stake)
                },
            )
            .collect::<Vec<SignerWithStake>>();
        let current_signer = &signers[0];
        let single_signer = MithrilSingleSigner::new(
            current_signer.0,
            &key_encode_hex(&current_signer.4).unwrap(),
        );

        let sign_result = single_signer.compute_single_signatures(
            message.clone(),
            stake_distribution,
            &protocol_parameters.into(),
        );

        assert_eq!(
            SingleSignerError::UnregisteredVerificationKey(),
            sign_result.unwrap_err()
        )
    }

    #[test]
    fn should_produce_a_single_signature() {
        let message = setup_message();
        let protocol_parameters = setup_protocol_parameters();
        let signers = setup_signers(5);
        let stake_distribution = signers
            .iter()
            .map(
                |(party_id, stake, verification_key, _protocol_signer, _protocol_initializer)| {
                    SignerWithStake::new(
                        *party_id,
                        key_encode_hex(verification_key).unwrap(),
                        *stake,
                    )
                },
            )
            .collect::<Vec<SignerWithStake>>();
        let current_signer = &signers[0];
        let single_signer = MithrilSingleSigner::new(
            current_signer.0,
            &key_encode_hex(&current_signer.4).unwrap(),
        );

        let sign_result = single_signer.compute_single_signatures(
            message.clone(),
            stake_distribution,
            &protocol_parameters.into(),
        );

        let protocol_signer = &current_signer.3;
        let clerk = ProtocolClerk::from_signer(&protocol_signer);

        assert!(&single_signer.get_protocol_initializer().is_some());
        assert!(!sign_result.as_ref().unwrap().is_empty());
        for sig in sign_result.unwrap() {
            let decoded_sig = key_decode_hex(&sig.signature).unwrap();
            assert!(clerk.verify_sig(&decoded_sig, &message).is_ok());
        }
    }
}
