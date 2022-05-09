// TODO: remove this
#![allow(dead_code)]

use hex::{FromHex, ToHex};
use serde::de::DeserializeOwned;
use serde::Serialize;
use slog_scope::{trace, warn};
use thiserror::Error;

use mithril::key_reg::KeyReg;
use mithril::msp::{MspPk, MspSk};
use mithril::stm::{
    Index, PartyId, Stake, StmClerk, StmInitializer, StmMultiSig, StmParameters, StmSig, StmSigner,
};
use mithril_aggregator::entities::{self, SignerWithStake, SingleSignature};

pub type Bytes = Vec<u8>;

// Protocol types alias
type D = blake2::Blake2b;
pub type ProtocolPartyId = PartyId;
pub type ProtocolStake = Stake;
pub type ProtocolStakeDistribution = Vec<(ProtocolPartyId, ProtocolStake)>;
pub type ProtocolParameters = StmParameters;
pub type ProtocolLotteryIndex = Index;
pub type ProtocolSigner = StmSigner<D>;
pub type ProtocolInitializer = StmInitializer;
pub type ProtocolClerk = StmClerk<D>;
pub type ProtocolKeyRegistration = KeyReg;
pub type ProtocolSingleSignature = StmSig<D>;
pub type ProtocolMultiSignature = StmMultiSig<D>;
pub type ProtocolSignerVerificationKey = MspPk;
pub type ProtocolSignerSecretKey = MspSk;

#[cfg(test)]
use mockall::automock;

#[cfg_attr(test, automock)]
pub trait SingleSigner {
    fn compute_single_signatures(
        &self,
        message: Bytes,
        stake_distribution: Vec<entities::SignerWithStake>,
        protocol_parameters: &entities::ProtocolParameters,
    ) -> Result<Vec<entities::SingleSignature>, SingleSignerError>;
}

#[derive(Error, Debug, PartialEq)]
pub enum SingleSignerError {
    #[error("the signer verification key is not registered in the stake distribution")]
    UnregisteredVerificationKey(),

    #[error("the signer party id is not registered in the stake distribution")]
    UnregisteredPartyId(),

    #[error("the protocol signer creation failed: `{0}`")]
    ProtocolSignerCreationFailure(String),
}

pub struct MithrilSingleSigner {
    party_id: u64,
    secret_key: ProtocolSignerSecretKey,
}

impl MithrilSingleSigner {
    pub fn new(party_id: u64, secret_key: ProtocolSignerSecretKey) -> Self {
        Self {
            party_id,
            secret_key,
        }
    }

    pub fn create_protocol_signer(
        &self,
        current_player_stake: ProtocolStake,
        stake_distribution: &[SignerWithStake], // @todo : use a hmap to prevent party id duplication
        protocol_parameters: &entities::ProtocolParameters,
    ) -> Result<ProtocolSigner, String> {
        let players = stake_distribution
            .iter()
            .map(|s| (s.party_id as ProtocolPartyId, s.stake as ProtocolStake))
            .collect::<Vec<_>>();
        let protocol_parameters = ProtocolParameters {
            k: protocol_parameters.k,
            m: protocol_parameters.m,
            phi_f: protocol_parameters.phi_f as f64,
        };

        let mut key_reg = KeyReg::new(&players);
        for s in stake_distribution {
            let decoded_key = key_decode_hex(&s.verification_key)?;
            key_reg
                .register(s.party_id as ProtocolPartyId, decoded_key)
                .unwrap();
        }
        let closed_reg = key_reg.close();

        let mut rng = rand_core::OsRng;
        let mut initializer = StmInitializer::setup(
            protocol_parameters,
            self.party_id as ProtocolPartyId,
            current_player_stake,
            &mut rng,
        );
        initializer.set_key(self.secret_key.clone());

        Ok(initializer.new_signer(closed_reg))
    }
}

impl SingleSigner for MithrilSingleSigner {
    fn compute_single_signatures(
        &self,
        message: Bytes,
        stake_distribution: Vec<SignerWithStake>, // @todo : use a hmap to prevent party id duplication
        protocol_parameters: &entities::ProtocolParameters,
    ) -> Result<Vec<SingleSignature>, SingleSignerError> {
        let current_signer_with_stake = stake_distribution
            .iter()
            .find(|s| s.party_id == self.party_id)
            .ok_or(SingleSignerError::UnregisteredPartyId())?;

        if current_signer_with_stake.verification_key.is_empty() {
            return Err(SingleSignerError::UnregisteredVerificationKey());
        }

        let protocol_signer = self
            .create_protocol_signer(
                current_signer_with_stake.stake,
                &stake_distribution,
                protocol_parameters,
            )
            .map_err(SingleSignerError::ProtocolSignerCreationFailure)?;

        trace!(
            "Party #{}: sign message {}",
            self.party_id,
            message.encode_hex::<String>()
        );

        let mut signatures = Vec::new();
        for i in 1..=protocol_parameters.m {
            if let Some(signature) = protocol_signer.sign(&message, i) {
                trace!("Party #{}: lottery #{} won", self.party_id, i,);
                let encoded_signature = key_encode_hex_sig(&signature);

                if encoded_signature.is_err() {
                    warn!("couldn't compute signature: `{:?}`", encoded_signature); // @todo: structured log
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

// TODO: To remove once 'ProtocolSingleSignature' implements `Serialize`
pub fn key_encode_hex_sig(from: &ProtocolSingleSignature) -> Result<String, String> {
    Ok(from.to_bytes().encode_hex::<String>())
}

// TODO: To remove once 'ProtocolSingleSignature' implements `Deserialize`
pub fn key_decode_hex_sig(from: &str) -> Result<ProtocolSingleSignature, String> {
    let from_vec = Vec::from_hex(from).map_err(|e| format!("can't parse from hex: {}", e))?;
    ProtocolSingleSignature::from_bytes(&from_vec)
        .map_err(|e| format!("can't decode multi signature: {}", e))
}

/// Encode key to hex helper
pub fn key_encode_hex<T>(from: T) -> Result<String, String>
where
    T: Serialize,
{
    Ok(serde_json::to_string(&from)
        .map_err(|e| format!("can't convert to hex: {}", e))?
        .encode_hex::<String>())
}

/// Decode key from hex helper
pub fn key_decode_hex<T>(from: &str) -> Result<T, String>
where
    T: DeserializeOwned,
{
    let from_vec = Vec::from_hex(from).map_err(|e| format!("can't parse from hex: {}", e))?;
    serde_json::from_slice(from_vec.as_slice()).map_err(|e| format!("can't deserialize: {}", e))
}

#[cfg(test)]
mod tests {
    use super::*;
    use mithril_aggregator::fake_data;

    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    fn setup_protocol_parameters() -> ProtocolParameters {
        ProtocolParameters {
            m: 10,
            k: 5,
            phi_f: 0.65,
        }
    }

    #[test]
    fn test_key_encode_decode_hex() {
        let protocol_params = setup_protocol_parameters();
        let party_id = 123;
        let stake = 100;
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let protocol_initializer: ProtocolInitializer =
            StmInitializer::setup(protocol_params, party_id, stake, &mut rng);
        let verification_key: ProtocolSignerVerificationKey =
            protocol_initializer.verification_key();
        let secret_key: ProtocolSignerSecretKey = protocol_initializer.secret_key();
        let verification_key_hex =
            key_encode_hex(verification_key).expect("unexpected hex encoding error");
        let secret_key_hex = key_encode_hex(&secret_key).expect("unexpected hex encoding error");
        let verification_key_restored =
            key_decode_hex(&verification_key_hex).expect("unexpected hex decoding error");
        let secret_key_restored: ProtocolSignerSecretKey =
            key_decode_hex(&secret_key_hex).expect("unexpected hex decoding error");
        assert_eq!(verification_key, verification_key_restored);
        assert_eq!(secret_key.to_bytes(), secret_key_restored.to_bytes());
    }

    #[test]
    fn cant_compute_if_signer_verification_key_is_not_registered() {
        let signer_with_keys = fake_data::signer_keys(0).unwrap();
        let single_signer = MithrilSingleSigner::new(
            signer_with_keys.party_id,
            key_decode_hex(&signer_with_keys.secret_key).unwrap(),
        );
        let stake_distribution = fake_data::signers_with_stakes(5)
            .into_iter()
            .map(|s| {
                if s.party_id == signer_with_keys.party_id {
                    SignerWithStake {
                        party_id: s.party_id,
                        verification_key: "".to_string(),
                        stake: s.stake,
                    }
                } else {
                    s
                }
            })
            .collect();
        let protocol_parameters = fake_data::protocol_parameters();

        let sign_result = single_signer.compute_single_signatures(
            "message".as_bytes().to_vec(),
            stake_distribution,
            &protocol_parameters,
        );

        assert_eq!(
            SingleSignerError::UnregisteredVerificationKey(),
            sign_result.unwrap_err()
        )
    }

    #[test]
    fn should_produce_a_single_signature() {
        let signer_with_keys = fake_data::signer_keys(0).unwrap();
        let single_signer = MithrilSingleSigner::new(
            signer_with_keys.party_id,
            key_decode_hex(&signer_with_keys.secret_key).unwrap(),
        );
        let stake_distribution = fake_data::signers_with_stakes(5);
        let protocol_parameters = fake_data::protocol_parameters();
        let protocol_signer: ProtocolSigner = single_signer
            .create_protocol_signer(
                signer_with_keys.stake,
                &stake_distribution,
                &protocol_parameters,
            )
            .unwrap();
        let clerk = StmClerk::from_signer(&protocol_signer);

        let message = "message".as_bytes();
        let sign_result = single_signer.compute_single_signatures(
            message.to_vec(),
            stake_distribution,
            &protocol_parameters,
        );

        assert!(!sign_result.as_ref().unwrap().is_empty());
        for sig in sign_result.unwrap() {
            let decoded_sig = key_decode_hex_sig(&sig.signature).unwrap();
            assert!(clerk.verify_sig(&decoded_sig, message).is_ok());
            assert_eq!(
                decoded_sig.pk,
                key_decode_hex(&signer_with_keys.verification_key).unwrap()
            );
        }
    }
}
