use crate::{StdError, StdResult};
use anyhow::anyhow;
use ed25519_dalek::{Signer, SigningKey};
#[cfg(feature = "random")]
use rand_chacha::rand_core;
use rand_chacha::rand_core::{CryptoRng, RngCore, SeedableRng};
use rand_chacha::ChaCha20Rng;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use super::{ProtocolGenesisSecretKey, ProtocolGenesisSignature, ProtocolGenesisVerificationKey};

#[derive(Error, Debug)]
/// [ProtocolGenesisSigner] and [ProtocolGenesisVerifier] related errors.
#[error("genesis signature verification error")]
pub struct ProtocolGenesisError(#[source] StdError);

/// A protocol Genesis Signer that is responsible for signing the
/// [Genesis Certificate](https://mithril.network/doc/mithril/mithril-protocol/certificates#the-certificate-chain-design)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProtocolGenesisSigner {
    /// Protocol Genesis secret key
    secret_key: ProtocolGenesisSecretKey,
}

impl ProtocolGenesisSigner {
    /// [ProtocolGenesisSigner] factory
    pub fn create_test_genesis_signer<R>(mut rng: R) -> Self
    where
        R: CryptoRng + RngCore,
    {
        let secret_key = SigningKey::generate(&mut rng);
        Self::from_secret_key(secret_key.into())
    }

    /// [ProtocolGenesisSigner] deterministic
    pub fn create_deterministic_genesis_signer() -> Self {
        let rng = ChaCha20Rng::from_seed([0u8; 32]);
        Self::create_test_genesis_signer(rng)
    }

    cfg_random! {
        /// [ProtocolGenesisSigner] non deterministic
        pub fn create_non_deterministic_genesis_signer() -> Self {
            let rng = rand_core::OsRng;
            Self::create_test_genesis_signer(rng)
        }
    }

    /// Get the [ProtocolGenesisSecretKey]
    pub fn secret_key(&self) -> ProtocolGenesisSecretKey {
        self.secret_key.clone()
    }

    /// Get the [ProtocolGenesisVerificationKey]
    pub fn verification_key(&self) -> ProtocolGenesisVerificationKey {
        self.secret_key.verifying_key().into()
    }

    /// [ProtocolGenesisSigner] from [ProtocolGenesisSecretKey]
    pub fn from_secret_key(secret_key: ProtocolGenesisSecretKey) -> Self {
        Self { secret_key }
    }

    /// Create a [ProtocolGenesisVerifier]
    pub fn create_genesis_verifier(&self) -> ProtocolGenesisVerifier {
        ProtocolGenesisVerifier::from_verification_key(self.verification_key())
    }

    /// Signs a message and returns a [ProtocolGenesisSignature]
    pub fn sign(&self, message: &[u8]) -> ProtocolGenesisSignature {
        self.secret_key.sign(message).into()
    }
}

/// A protocol Genesis Verifier that is responsible for verifying the
/// [Genesis Certificate](https://mithril.network/doc/mithril/mithril-protocol/certificates#the-certificate-chain-design)
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ProtocolGenesisVerifier {
    pub(crate) verification_key: ProtocolGenesisVerificationKey,
}

impl ProtocolGenesisVerifier {
    /// [ProtocolGenesisVerifier] from [ProtocolGenesisVerificationKey]
    pub fn from_verification_key(verification_key: ProtocolGenesisVerificationKey) -> Self {
        Self { verification_key }
    }

    /// [ProtocolGenesisVerifier] to [ProtocolGenesisVerificationKey]
    pub fn to_verification_key(&self) -> ProtocolGenesisVerificationKey {
        self.verification_key
    }

    /// Verifies the genesis signature of a message
    pub fn verify(&self, message: &[u8], signature: &ProtocolGenesisSignature) -> StdResult<()> {
        self.verification_key.verify(message, signature)
    }
}

impl ProtocolGenesisVerificationKey {
    /// Verifies the genesis signature of a message
    pub fn verify(&self, message: &[u8], signature: &ProtocolGenesisSignature) -> StdResult<()> {
        Ok(self
            .verify_strict(message, signature)
            .map_err(|e| ProtocolGenesisError(anyhow!(e)))?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_test_deterministic_genesis_keypair() {
        let genesis_signer = ProtocolGenesisSigner::create_deterministic_genesis_signer();
        let genesis_verifier = genesis_signer.create_genesis_verifier();
        let genesis_signer_2 = ProtocolGenesisSigner::create_deterministic_genesis_signer();
        let genesis_verifier_2 = genesis_signer.create_genesis_verifier();
        assert_eq!(
            genesis_signer.secret_key.to_bytes(),
            genesis_signer_2.secret_key.to_bytes()
        );
        assert_eq!(
            genesis_verifier.verification_key.as_bytes(),
            genesis_verifier_2.verification_key.as_bytes()
        );

        println!(
            "Deterministic Genesis Verification Key={}",
            genesis_verifier.verification_key.to_json_hex().unwrap()
        );
        println!(
            "Deterministic Genesis Secret Key=={}",
            genesis_signer.secret_key.to_json_hex().unwrap()
        );
    }

    cfg_random! {
        #[test]
        fn test_generate_test_non_deterministic_genesis_keypair() {
            let genesis_signer = ProtocolGenesisSigner::create_non_deterministic_genesis_signer();
            let genesis_verifier = genesis_signer.create_genesis_verifier();

            println!(
                "Non Deterministic Genesis Verification Key={}",
                genesis_verifier.verification_key.to_json_hex().unwrap()
            );
            println!(
                "Non Deterministic Genesis Secret Key=={}",
                genesis_signer.secret_key.to_json_hex().unwrap()
            );
        }
    }

    #[test]
    fn test_codec_genesis_keypair() {
        let genesis_signer = ProtocolGenesisSigner::create_deterministic_genesis_signer();
        let genesis_verifier = genesis_signer.create_genesis_verifier();
        let secret_key_encoded = genesis_signer.secret_key.to_json_hex().unwrap();
        let verification_key_encoded = genesis_verifier.verification_key.to_json_hex().unwrap();
        let secret_key_decoded: ProtocolGenesisSecretKey = secret_key_encoded.try_into().unwrap();
        let verification_key_decoded: ProtocolGenesisVerificationKey =
            verification_key_encoded.try_into().unwrap();
        let genesis_signer_decoded = ProtocolGenesisSigner::from_secret_key(secret_key_decoded);
        let genesis_verifier_decoded =
            ProtocolGenesisVerifier::from_verification_key(verification_key_decoded);

        let message: &[u8] = b"some message.";
        let signature = genesis_signer_decoded.sign(message);
        let verify_signature = genesis_verifier_decoded.verify(message, &signature);
        assert!(
            verify_signature.is_ok(),
            "genesis signature verification should not fail"
        );
    }
}
