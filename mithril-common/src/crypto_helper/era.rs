use ed25519_dalek::{ExpandedSecretKey, SignatureError};
use rand_chacha_dalek_compat::rand_core::{self, CryptoRng, RngCore, SeedableRng};
use rand_chacha_dalek_compat::ChaCha20Rng;
use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Alias of [Ed25519:PublicKey](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.PublicKey.html).
pub type EraMarkersVerifierVerificationKey = ed25519_dalek::PublicKey;

/// Alias of [Ed25519:SecretKey](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.SecretKey.html).
pub type EraMarkersVerifierSecretKey = ed25519_dalek::SecretKey;

/// Alias of [Ed25519:Signature](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.Signature.html).
pub type EraMarkersVerifierSignature = ed25519_dalek::Signature;

#[derive(Error, Debug)]
/// [EraMarkersSigner] and [EraMarkersVerifier] related errors.
pub enum EraMarkersVerifierError {
    /// Error raised when a Signature verification fail
    #[error("era markers signature verification error: '{0}'")]
    SignatureVerification(#[from] SignatureError),
}

/// A cryptographic signer that is responsible for signing the EreMarkers
#[derive(Debug, Serialize, Deserialize)]
pub struct EraMarkersSigner {
    pub(crate) secret_key: EraMarkersVerifierSecretKey,
}

impl EraMarkersSigner {
    /// EraMarkersSigner factory
    pub fn create_test_signer<R>(mut rng: R) -> Self
    where
        R: CryptoRng + RngCore,
    {
        let secret_key = EraMarkersVerifierSecretKey::generate(&mut rng);
        Self::from_secret_key(secret_key)
    }

    /// EraMarkersSigner deterministic
    pub fn create_deterministic_signer() -> Self {
        let rng = ChaCha20Rng::from_seed([0u8; 32]);
        Self::create_test_signer(rng)
    }

    /// EraMarkersSigner non deterministic
    pub fn create_non_deterministic_signer() -> Self {
        let rng = rand_core::OsRng;
        Self::create_test_signer(rng)
    }

    /// EraMarkersSigner from EraMarkersVerifierSecretKey
    pub fn from_secret_key(secret_key: EraMarkersVerifierSecretKey) -> Self {
        Self { secret_key }
    }

    /// Create a an expanded secret key
    fn create_expanded_secret_key(&self) -> ExpandedSecretKey {
        ExpandedSecretKey::from(&self.secret_key)
    }

    /// Create a EraMarkersVerifierVerificationKey
    fn create_verification_key(
        &self,
        expanded_secret_key: &ExpandedSecretKey,
    ) -> EraMarkersVerifierVerificationKey {
        let verification_key: EraMarkersVerifierVerificationKey = expanded_secret_key.into();
        verification_key
    }

    /// Create a EraMarkersVerifier
    pub fn create_verifier(&self) -> EraMarkersVerifier {
        let expanded_secret_key = self.create_expanded_secret_key();
        let verification_key = self.create_verification_key(&expanded_secret_key);
        EraMarkersVerifier::from_verification_key(verification_key)
    }

    /// Signs a message and returns a EraMarkersVerifierSignature
    pub fn sign(&self, message: &[u8]) -> EraMarkersVerifierSignature {
        let expanded_secret_key = self.create_expanded_secret_key();
        let verification_key = self.create_verification_key(&expanded_secret_key);
        expanded_secret_key.sign(message, &verification_key)
    }
}

/// An era markers verifier that checks the authenticity of era markers stored on the chain
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct EraMarkersVerifier {
    pub(crate) verification_key: EraMarkersVerifierVerificationKey,
}

impl EraMarkersVerifier {
    /// EraMarkersVerifier from EraMarkersVerifierVerificationKey
    pub fn from_verification_key(verification_key: EraMarkersVerifierVerificationKey) -> Self {
        Self { verification_key }
    }

    /// EraMarkersVerifier to EraMarkersVerifierVerificationKey
    pub fn to_verification_key(&self) -> EraMarkersVerifierVerificationKey {
        self.verification_key
    }

    /// Verifies the signature of a message
    pub fn verify(
        &self,
        message: &[u8],
        signature: &EraMarkersVerifierSignature,
    ) -> Result<(), EraMarkersVerifierError> {
        Ok(self.verification_key.verify_strict(message, signature)?)
    }
}

#[cfg(test)]
mod tests {
    use super::super::codec::{key_decode_hex, key_encode_hex};
    use super::*;

    #[test]
    fn test_generate_test_deterministic_keypair() {
        let signer = EraMarkersSigner::create_deterministic_signer();
        let verifier = signer.create_verifier();
        let signer_2 = EraMarkersSigner::create_deterministic_signer();
        let verifier_2 = signer.create_verifier();
        assert_eq!(signer.secret_key.as_bytes(), signer_2.secret_key.as_bytes());
        assert_eq!(
            verifier.verification_key.as_bytes(),
            verifier_2.verification_key.as_bytes()
        );

        println!(
            "Deterministic Verification Key={}",
            key_encode_hex(verifier.verification_key.as_bytes()).unwrap()
        );
        println!(
            "Deterministic Secret Key=={}",
            key_encode_hex(signer.secret_key.as_bytes()).unwrap()
        );
    }

    #[test]
    fn test_generate_test_non_deterministic_keypair() {
        let signer = EraMarkersSigner::create_non_deterministic_signer();
        let verifier = signer.create_verifier();

        println!(
            "Non Deterministic Verification Key={}",
            key_encode_hex(verifier.verification_key.as_bytes()).unwrap()
        );
        println!(
            "Non Deterministic Secret Key=={}",
            key_encode_hex(signer.secret_key.as_bytes()).unwrap()
        );
    }

    #[test]
    fn test_codec_keypair() {
        let signer = EraMarkersSigner::create_deterministic_signer();
        let verifier = signer.create_verifier();
        let secret_key_encoded = key_encode_hex(signer.secret_key.as_bytes()).unwrap();
        let verification_key_encoded =
            key_encode_hex(verifier.verification_key.as_bytes()).unwrap();
        let secret_key_decoded: EraMarkersVerifierSecretKey =
            key_decode_hex(&secret_key_encoded).unwrap();
        let verification_key_decoded: EraMarkersVerifierVerificationKey =
            key_decode_hex(&verification_key_encoded).unwrap();
        let signer_decoded = EraMarkersSigner::from_secret_key(secret_key_decoded);
        let verifier_decoded = EraMarkersVerifier::from_verification_key(verification_key_decoded);

        let message: &[u8] = b"some message.";
        let signature = signer_decoded.sign(message);
        let verify_signature = verifier_decoded.verify(message, &signature);
        assert!(
            verify_signature.is_ok(),
            "signature verification should not fail"
        );
    }
}
