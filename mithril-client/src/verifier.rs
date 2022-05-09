// TODO: to be removed later
#![allow(dead_code)]

use hex::{FromHex, ToHex};
use log::debug;
use serde::de::DeserializeOwned;
use serde::Serialize;
use thiserror::Error;

use mithril::key_reg::KeyReg;
use mithril::msp::{MspPk, MspSk};
use mithril::stm::{
    Index, PartyId, Stake, StmClerk, StmInitializer, StmMultiSig, StmParameters, StmSig, StmSigner,
};

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

use crate::entities;

#[cfg(test)]
use mockall::automock;

#[derive(Error, Debug)]
pub enum ProtocolError {
    #[error("key encode failed")]
    KeyEncodeFailed(String),
    #[error("key decode failed")]
    KeyDecodeFailed(String),
    #[error("multi signature verification failed")]
    VerifyMultiSignatureError(String),
}

/// Verifier is the cryptographic engine in charge of verifying multi signatures and certificates
#[cfg_attr(test, automock)]
pub trait Verifier {
    /// Verify a multi signature
    fn verify_multi_signature(
        &self,
        message: &Bytes,
        multi_signature: &str,
        signers_with_stakes: &[entities::SignerWithStake],
        protocol_parameters: &entities::ProtocolParameters,
    ) -> Result<(), ProtocolError>;
}

/// VerifierImpl is an implementation of the Verifier
pub struct VerifierImpl {}

impl VerifierImpl {
    /// VerifierImpl factory
    pub fn new() -> Self {
        debug!("New VerifierImpl created");
        Self {}
    }

    /// Creates a clerk
    pub fn create_clerk(
        &self,
        signers_with_stakes: &[entities::SignerWithStake],
        protocol_parameters: &entities::ProtocolParameters,
    ) -> Result<ProtocolClerk, String> {
        let protocol_parameters = ProtocolParameters {
            k: protocol_parameters.k,
            m: protocol_parameters.m,
            phi_f: protocol_parameters.phi_f as f64,
        };
        let stakes = signers_with_stakes
            .iter()
            .map(|signer| {
                (
                    signer.party_id as ProtocolPartyId,
                    signer.stake as ProtocolStake,
                )
            })
            .collect::<ProtocolStakeDistribution>();
        let mut key_registration: ProtocolKeyRegistration = KeyReg::new(&stakes);
        signers_with_stakes.iter().for_each(|signer| {
            if let Ok(verification_key) = key_decode_hex(&signer.verification_key) {
                key_registration
                    .register(signer.party_id as ProtocolPartyId, verification_key)
                    .unwrap();
            }
        });
        let closed_registration = key_registration.close();
        Ok(StmClerk::from_registration(
            protocol_parameters,
            closed_registration,
        ))
    }
}

impl Verifier for VerifierImpl {
    /// Verify a multi signature
    fn verify_multi_signature(
        &self,
        message: &Bytes,
        multi_signature: &str,
        signers_with_stakes: &[entities::SignerWithStake],
        protocol_parameters: &entities::ProtocolParameters,
    ) -> Result<(), ProtocolError> {
        debug!("Verify multi signature for {:?}", message);
        let clerk = self.create_clerk(signers_with_stakes, protocol_parameters);
        let multi_signature: ProtocolMultiSignature = key_decode_hex_multisig(multi_signature)
            .map_err(|e| ProtocolError::VerifyMultiSignatureError(e.to_string()))?;
        clerk
            .as_ref()
            .unwrap()
            .verify_msig(&multi_signature, message)
            .map_err(|e| ProtocolError::VerifyMultiSignatureError(e.to_string()))
    }
}

// TODO: To remove once 'ProtocolMultiSignature' implements `Serialize`
pub fn key_encode_hex_multisig(from: &ProtocolMultiSignature) -> Result<String, String> {
    Ok(from.to_bytes().encode_hex::<String>())
}

// TODO: To remove once 'ProtocolMultiSignature' implements `Deserialize`
pub fn key_decode_hex_multisig(from: &str) -> Result<ProtocolMultiSignature, String> {
    let from_vec = Vec::from_hex(from).map_err(|e| format!("can't parse from hex: {}", e))?;
    ProtocolMultiSignature::from_bytes(&from_vec)
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

    use mithril_aggregator::multi_signer::key_encode_hex_multisig;
    use rand_chacha::ChaCha20Rng;
    use rand_core::{RngCore, SeedableRng};

    fn message() -> Bytes {
        Vec::from_hex("7724e03fb8d84a376a43b8f41518a11c").unwrap()
    }

    fn setup_protocol_parameters() -> ProtocolParameters {
        ProtocolParameters {
            m: 10,
            k: 5,
            phi_f: 0.65,
        }
    }

    fn setup_signers(
        total: u64,
    ) -> Vec<(
        ProtocolPartyId,
        ProtocolStake,
        ProtocolSignerVerificationKey,
        ProtocolSigner,
    )> {
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let protocol_parameters = setup_protocol_parameters();
        let signers = (0..total)
            .into_iter()
            .map(|party_id| {
                let stake = 1 + rng.next_u64() % 999;
                let protocol_initializer: ProtocolInitializer = StmInitializer::setup(
                    protocol_parameters.clone(),
                    party_id as ProtocolPartyId,
                    stake,
                    &mut rng,
                );
                (
                    party_id as ProtocolPartyId,
                    stake as ProtocolStake,
                    protocol_initializer,
                )
            })
            .collect::<Vec<(ProtocolPartyId, ProtocolStake, ProtocolInitializer)>>();

        let mut key_registration = KeyReg::new(
            &signers
                .iter()
                .map(|(party_id, stake, _)| (*party_id, *stake))
                .collect::<Vec<_>>(),
        );
        signers
            .iter()
            .for_each(|(party_id, _, protocol_initializer)| {
                key_registration
                    .register(*party_id, protocol_initializer.verification_key())
                    .expect("key registration should have succeeded");
            });
        let closed_key_registration = key_registration.close();
        signers
            .into_iter()
            .map(|(party_id, stake, protocol_initializer)| {
                (
                    party_id,
                    stake,
                    protocol_initializer.verification_key(),
                    protocol_initializer.new_signer(closed_key_registration.clone()),
                )
            })
            .collect::<_>()
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
    fn test_multi_signer_multi_signature_ok() {
        let protocol_parameters = setup_protocol_parameters();
        let signers = setup_signers(5);
        let message = message();

        let mut single_signatures = Vec::new();
        signers.iter().for_each(|(_, _, _, protocol_signer)| {
            for i in 1..=protocol_parameters.m {
                if let Some(signature) = protocol_signer.sign(&message, i) {
                    single_signatures.push(signature);
                }
            }
        });

        let first_signer = &signers.first().unwrap().3;
        let clerk = StmClerk::from_signer(&first_signer);
        let multi_signature = clerk.aggregate(&single_signatures, &message).unwrap();

        let verifier = VerifierImpl::new();
        let protocol_parameters = entities::ProtocolParameters {
            k: protocol_parameters.k,
            m: protocol_parameters.m,
            phi_f: protocol_parameters.phi_f as f32,
        };
        let signers_with_stakes = signers
            .iter()
            .map(|(party_id, stake, verification_key, _)| {
                entities::SignerWithStake::new(
                    *party_id as u64,
                    key_encode_hex(verification_key).unwrap(),
                    *stake as u64,
                )
            })
            .collect::<Vec<entities::SignerWithStake>>();
        let message_tampered = message[1..].to_vec();
        assert!(
            verifier
                .verify_multi_signature(
                    &message_tampered,
                    &key_encode_hex_multisig(&multi_signature).unwrap(),
                    &signers_with_stakes,
                    &protocol_parameters,
                )
                .is_err(),
            "multi signature verification should have failed"
        );
        verifier
            .verify_multi_signature(
                &message,
                &key_encode_hex_multisig(&multi_signature).unwrap(),
                &signers_with_stakes,
                &protocol_parameters,
            )
            .expect("multi signature verification should have succeeded");
    }
}
