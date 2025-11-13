use crate::*;

use super::interface::*;

/// Jubjub signing key
#[derive(Default, Clone)]
pub struct JubjubSigningKey {}

/// Jubjub verification key
#[derive(Default)]
pub struct JubjubVerificationKey {}

/// Jubjub verification key proof of possession
#[derive(PartialEq, Eq, Clone)]
pub struct JubjubVerificationKeyProofOfPossession {}

impl JubjubVerificationKeyProofOfPossession {
    pub fn into_verification_key(self) -> JubjubVerificationKey {
        JubjubVerificationKey {}
    }
}

/// Jubjub signature
#[derive(Default)]
pub struct JubjubSignature {}

/// Schnorr crypto initializer
pub struct SchnorrCryptoInitializer {
    seed: Vec<u8>,
}

impl CryptoInitializer for SchnorrCryptoInitializer {
    type SigningKey = JubjubSigningKey;
    type VerificationKey = JubjubVerificationKey;
    type VerificationKeyProofOfPossession = JubjubVerificationKeyProofOfPossession;

    fn get_signing_key(&self) -> &Self::SigningKey {
        &JubjubSigningKey {}
    }

    fn get_verification_key(&self) -> &Self::VerificationKey {
        &JubjubVerificationKey {}
    }

    fn get_verification_key_proof_of_possession(&self) -> &Self::VerificationKeyProofOfPossession {
        &JubjubVerificationKeyProofOfPossession {}
    }
}

/// Schnorr crypto signer
pub struct SchnorrCryptoSigner {
    pub bls_signing_key: JubjubSigningKey,
}

impl SchnorrCryptoSigner {
    pub fn new(bls_signing_key: JubjubSigningKey) -> Self {
        Self { bls_signing_key }
    }
}

impl CryptoSigner for SchnorrCryptoSigner {
    type SigningKey = JubjubSigningKey;
    type VerificationKey = JubjubVerificationKey;
    type VerificationKeyProofOfPossession = JubjubVerificationKeyProofOfPossession;
    type Signature = JubjubSignature;

    fn sign(message: &[u8], signing_key: &Self::SigningKey) -> StdResult<Self::Signature> {
        Ok(JubjubSignature::default())
    }
}

/// Schnorr crypto verifier
pub struct SchnorrCryptoVerifier {
    pub bls_verification_key: JubjubVerificationKey,
}

impl CryptoVerifier for SchnorrCryptoVerifier {
    type VerificationKey = JubjubVerificationKey;
    type Signature = JubjubSignature;

    fn verify(
        message: &[u8],
        signature: &Self::Signature,
        verification_key: &Self::VerificationKey,
    ) -> StdResult<()> {
        Ok(())
    }
}
