use crate::crypto_helper::ed25519::*;

pub mod era {
    use super::*;

    /// Era markers verifier secret key
    pub type EraMarkersVerifierSecretKey = Ed25519SecretKey;
    /// Era markers verifier verification key
    pub type EraMarkersVerifierVerificationKey = Ed25519VerificationKey;
    /// Era markers verifier signature
    pub type EraMarkersVerifierSignature = Ed25519Signature;
    /// A cryptographic signer that is responsible for signing the EraMarkers
    pub type EraMarkersSigner = Ed25519Signer;
    /// An era markers verifier that checks the authenticity of era markers stored on the chain
    pub type EraMarkersVerifier = Ed25519Verifier;
    /// [EraMarkersSigner] and [EraMarkersVerifier] related errors.
    pub type EraMarkersVerifierError = Ed25519VerifierError;
}
