use ed25519_dalek::{Signer, SigningKey};
use rand_chacha::rand_core::{self, CryptoRng, RngCore, SeedableRng};
use rand_chacha::ChaCha20Rng;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::{StdError, StdResult};

use super::ProtocolKey;

/// Wrapper of [Ed25519:PublicKey](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.VerifyingKey.html).
pub type EraMarkersVerifierVerificationKey = ProtocolKey<ed25519_dalek::VerifyingKey>;

/// Wrapper of [Ed25519:SigningKey](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.SigningKey.html).
pub type EraMarkersVerifierSecretKey = ProtocolKey<ed25519_dalek::SigningKey>;

/// Wrapper of [Ed25519:Signature](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.Signature.html).
pub type EraMarkersVerifierSignature = ProtocolKey<ed25519_dalek::Signature>;

#[derive(Error, Debug)]
/// [EraMarkersSigner] and [EraMarkersVerifier] related errors.
pub enum EraMarkersVerifierError {
    /// Error raised when a Signature verification fail
    #[error("era markers signature verification error: '{0}'")]
    SignatureVerification(StdError),
}

/// A cryptographic signer that is responsible for signing the EraMarkers
#[derive(Debug, Serialize, Deserialize)]
pub struct EraMarkersSigner {
    pub(crate) secret_key: EraMarkersVerifierSecretKey,
}

impl EraMarkersSigner {
    /// [EraMarkersSigner] factory
    pub fn create_test_signer<R>(mut rng: R) -> Self
    where
        R: CryptoRng + RngCore,
    {
        let secret_key = SigningKey::generate(&mut rng);
        Self::from_secret_key(secret_key.into())
    }

    /// [EraMarkersSigner] deterministic
    pub fn create_deterministic_signer() -> Self {
        let rng = ChaCha20Rng::from_seed([0u8; 32]);
        Self::create_test_signer(rng)
    }

    /// [EraMarkersSigner] non deterministic
    pub fn create_non_deterministic_signer() -> Self {
        let rng = rand_core::OsRng;
        Self::create_test_signer(rng)
    }

    /// [EraMarkersSigner] from [EraMarkersVerifierSecretKey]
    pub fn from_secret_key(secret_key: EraMarkersVerifierSecretKey) -> Self {
        Self { secret_key }
    }

    /// Create a [EraMarkersVerifier]
    pub fn create_verifier(&self) -> EraMarkersVerifier {
        EraMarkersVerifier::from_verification_key(self.secret_key.verifying_key().into())
    }

    /// Signs a message and returns a [EraMarkersVerifierSignature]
    pub fn sign(&self, message: &[u8]) -> EraMarkersVerifierSignature {
        self.secret_key.sign(message).into()
    }
}

/// An era markers verifier that checks the authenticity of era markers stored on the chain
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct EraMarkersVerifier {
    pub(crate) verification_key: EraMarkersVerifierVerificationKey,
}

impl EraMarkersVerifier {
    /// [EraMarkersVerifier] from [EraMarkersVerifierVerificationKey]
    pub fn from_verification_key(verification_key: EraMarkersVerifierVerificationKey) -> Self {
        Self { verification_key }
    }

    /// [EraMarkersVerifier] to [EraMarkersVerifierVerificationKey]
    pub fn to_verification_key(&self) -> EraMarkersVerifierVerificationKey {
        self.verification_key
    }

    /// Verifies the signature of a message
    pub fn verify(&self, message: &[u8], signature: &EraMarkersVerifierSignature) -> StdResult<()> {
        Ok(self.verification_key.verify_strict(message, signature)?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const GOLDEN_ERA_MARKERS_VERIFICATION_KEY: &str =
        "5b32332c32372c3131322c362c35372c38342c3138302c342c3135302c3233322c3233372c3132362c3131392c\
        3231342c33352c35342c38312c3230382c3231372c39392c3137302c3233312c3133392c362c3132322c39342c3\
        9322c3137322c32332c3130322c3135372c3136375d";
    const GOLDEN_ERA_MARKERS_SECRET_KEY: &str =
        "5b34332c3133322c3232312c3138382c3235332c3132372c3235352c38362c3136322c3133312c3233332c3131\
        362c3134322c3233352c3131312c3133332c3134312c3138332c302c33392c3132302c3139372c39322c3133302\
        c3233342c34362c3135372c32352c3133322c31352c3234312c3235345d";

    #[test]
    fn golden_master() {
        EraMarkersVerifierVerificationKey::from_json_hex(GOLDEN_ERA_MARKERS_VERIFICATION_KEY)
            .expect("Decoding golden verification key should not fail");

        EraMarkersVerifierSecretKey::from_json_hex(GOLDEN_ERA_MARKERS_SECRET_KEY)
            .expect("Decoding golden secret key should not fail");
    }

    #[test]
    fn test_generate_test_deterministic_keypair() {
        let signer = EraMarkersSigner::create_deterministic_signer();
        let verifier = signer.create_verifier();
        let signer_2 = EraMarkersSigner::create_deterministic_signer();
        let verifier_2 = signer.create_verifier();
        assert_eq!(signer.secret_key.to_bytes(), signer_2.secret_key.to_bytes());
        assert_eq!(
            verifier.verification_key.as_bytes(),
            verifier_2.verification_key.as_bytes()
        );

        println!(
            "Deterministic Verification Key={}",
            verifier.verification_key.to_json_hex().unwrap()
        );
        println!(
            "Deterministic Secret Key=={}",
            signer.secret_key.to_json_hex().unwrap()
        );
    }

    #[test]
    fn test_generate_test_non_deterministic_keypair() {
        let signer = EraMarkersSigner::create_non_deterministic_signer();
        let verifier = signer.create_verifier();

        println!(
            "Non Deterministic Verification Key={}",
            verifier.verification_key.to_json_hex().unwrap()
        );
        println!(
            "Non Deterministic Secret Key=={}",
            signer.secret_key.to_json_hex().unwrap()
        );
    }

    #[test]
    fn test_codec_keypair() {
        let signer = EraMarkersSigner::create_deterministic_signer();
        let verifier = signer.create_verifier();
        let secret_key_encoded = signer.secret_key.to_json_hex().unwrap();
        let verification_key_encoded = verifier.verification_key.to_json_hex().unwrap();
        let secret_key_decoded: EraMarkersVerifierSecretKey =
            secret_key_encoded.try_into().unwrap();
        let verification_key_decoded: EraMarkersVerifierVerificationKey =
            verification_key_encoded.try_into().unwrap();
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
