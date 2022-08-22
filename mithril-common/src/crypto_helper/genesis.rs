use ed25519_dalek::{ExpandedSecretKey, SignatureError};
use rand_chacha_dalek_compat::rand_core;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use super::{ProtocolGenesisSecretKey, ProtocolGenesisSignature, ProtocolGenesisVerificationKey};

#[derive(Error, Debug)]
/// [ProtocolGenesisSigner] and [ProtocolGenesisVerifier] related errors.
pub enum ProtocolGenesisError {
    /// Error raised when a Genesis Signature verification fail
    #[error("genesis signature verification error: '{0}'")]
    SignatureVerification(#[from] SignatureError),
}

/// A protocol Genesis Signer that is responsible for signing the [Genesis Certificate](https://mithril.network/doc/mithril/mithril-protocol/certificates#the-certificate-chain-design)
#[derive(Debug, Serialize, Deserialize)]
pub struct ProtocolGenesisSigner {
    pub(crate) secret_key: ProtocolGenesisSecretKey,
}

impl ProtocolGenesisSigner {
    /// ProtocolGenesisSigner factory
    pub fn create_test_genesis_signer() -> Self {
        let mut rng = rand_core::OsRng;
        let secret_key = ProtocolGenesisSecretKey::generate(&mut rng);
        Self::from_secret_key(secret_key)
    }

    /// ProtocolGenesisSigner from ProtocolGenesisSecretKey
    pub fn from_secret_key(secret_key: ProtocolGenesisSecretKey) -> Self {
        ProtocolGenesisSigner { secret_key }
    }

    /// Create a an expanded secret key
    fn create_expanded_secret_key(&self) -> ExpandedSecretKey {
        ExpandedSecretKey::from(&self.secret_key)
    }

    /// Create a ProtocolGenesisVerificationKey
    fn create_verification_key(
        &self,
        expanded_secret_key: &ExpandedSecretKey,
    ) -> ProtocolGenesisVerificationKey {
        let verification_key: ProtocolGenesisVerificationKey = expanded_secret_key.into();
        verification_key
    }

    /// Create a ProtocolGenesisVerifier
    pub fn create_genesis_verifier(&self) -> ProtocolGenesisVerifier {
        let expanded_secret_key = self.create_expanded_secret_key();
        let verification_key = self.create_verification_key(&expanded_secret_key);
        ProtocolGenesisVerifier::from_verification_key(verification_key)
    }

    /// Signs a message and returns a ProtocolGenesisSignature
    pub fn sign(&self, message: &[u8]) -> ProtocolGenesisSignature {
        let expanded_secret_key = self.create_expanded_secret_key();
        let verification_key = self.create_verification_key(&expanded_secret_key);
        expanded_secret_key.sign(message, &verification_key)
    }
}

/// A protocol Genesis Verifier that is responsible for verifying the [Genesis Certificate](https://mithril.network/doc/mithril/mithril-protocol/certificates#the-certificate-chain-design)
#[derive(Debug, Serialize, Deserialize)]
pub struct ProtocolGenesisVerifier {
    pub(crate) verification_key: ProtocolGenesisVerificationKey,
}

impl ProtocolGenesisVerifier {
    /// ProtocolGenesisVerifier from ProtocolGenesisVerificationKey
    pub fn from_verification_key(verification_key: ProtocolGenesisVerificationKey) -> Self {
        ProtocolGenesisVerifier { verification_key }
    }

    /// Verifies the signature of a message
    pub fn verify(
        &self,
        message: &[u8],
        signature: &ProtocolGenesisSignature,
    ) -> Result<(), ProtocolGenesisError> {
        Ok(self.verification_key.verify_strict(message, signature)?)
    }
}

#[cfg(test)]
mod tests {
    use super::super::codec::{key_decode_hex, key_encode_hex};
    use super::*;

    #[test]
    fn test_generate_test_genesis_keypair() {
        let genesis_signer = ProtocolGenesisSigner::create_test_genesis_signer();
        let genesis_verifier = genesis_signer.create_genesis_verifier();
        let secret_key_encoded = key_encode_hex(genesis_signer.secret_key.as_bytes()).unwrap();
        let verification_key_encoded =
            key_encode_hex(genesis_verifier.verification_key.as_bytes()).unwrap();

        println!("Genesis Verification Key={}", verification_key_encoded);
        println!("Genesis Secret Key=={}", secret_key_encoded);
    }

    #[test]
    fn test_codec_genesis_keypair() {
        let genesis_signer = ProtocolGenesisSigner::create_test_genesis_signer();
        let genesis_verifier = genesis_signer.create_genesis_verifier();
        let secret_key_encoded = key_encode_hex(genesis_signer.secret_key.as_bytes()).unwrap();
        let verification_key_encoded =
            key_encode_hex(genesis_verifier.verification_key.as_bytes()).unwrap();
        let secret_key_decoded: ProtocolGenesisSecretKey =
            key_decode_hex(&secret_key_encoded).unwrap();
        let verification_key_decoded: ProtocolGenesisVerificationKey =
            key_decode_hex(&verification_key_encoded).unwrap();
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
