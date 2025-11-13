use crate::*;

/// Crypto signer trait
pub trait CryptoSigner {
    type SigningKey;
    type VerificationKey;
    type VerificationKeyProofOfPossession;
    type Signature;

    /// Signs a message using the signing key
    fn sign(message: &[u8], signing_key: &Self::SigningKey) -> StdResult<Self::Signature>;
}

/// Crypto verifier trait
pub trait CryptoVerifier {
    type VerificationKey;
    type Signature;

    /// Verifies a signature using the verification key
    fn verify(
        message: &[u8],
        signature: &Self::Signature,
        verification_key: &Self::VerificationKey,
    ) -> StdResult<()>;
}

/// Crypto initializer trait
pub trait CryptoInitializer {
    type SigningKey;
    type VerificationKey;
    type VerificationKeyProofOfPossession;

    /// Returns the signing key
    fn get_signing_key(&self) -> &Self::SigningKey;

    /// Returns the verification key
    fn get_verification_key(&self) -> &Self::VerificationKey;

    /// Returns the verification key proof of possession
    fn get_verification_key_proof_of_possession(&self) -> &Self::VerificationKeyProofOfPossession;
}
