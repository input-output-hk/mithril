use crate::*;

use super::interface::*;

/// BLS signing key
#[derive(Default, Clone)]
pub struct BlsSigningKey {}

/// BLS verification key
pub struct BlsVerificationKey {}

/// BLS verification key proof of possession
#[derive(Default, PartialEq, Eq, Clone)]
pub struct BlsVerificationKeyProofOfPossession {}

impl BlsVerificationKeyProofOfPossession {
    pub fn into_verification_key(self) -> BlsVerificationKey {
        BlsVerificationKey {}
    }
}

/// BLS signature
#[derive(Default)]
pub struct BlsSignature {}

/// BLS crypto initializer
pub struct BlsCryptoInitializer {
    seed: Vec<u8>,
}

impl CryptoInitializer for BlsCryptoInitializer {
    type SigningKey = BlsSigningKey;
    type VerificationKey = BlsVerificationKey;
    type VerificationKeyProofOfPossession = BlsVerificationKeyProofOfPossession;

    fn get_signing_key(&self) -> &Self::SigningKey {
        &BlsSigningKey {}
    }

    fn get_verification_key(&self) -> &Self::VerificationKey {
        &BlsVerificationKey {}
    }

    fn get_verification_key_proof_of_possession(&self) -> &Self::VerificationKeyProofOfPossession {
        &BlsVerificationKeyProofOfPossession {}
    }
}

/// BLS crypto signer
pub struct BlsCryptoSigner {
    pub bls_signing_key: BlsSigningKey,
}

impl BlsCryptoSigner {
    pub fn new(bls_signing_key: BlsSigningKey) -> Self {
        Self { bls_signing_key }
    }
}

impl CryptoSigner for BlsCryptoSigner {
    type SigningKey = BlsSigningKey;
    type VerificationKey = BlsVerificationKey;
    type VerificationKeyProofOfPossession = BlsVerificationKeyProofOfPossession;
    type Signature = BlsSignature;

    fn sign(message: &[u8], signing_key: &Self::SigningKey) -> StdResult<Self::Signature> {
        Ok(BlsSignature::default())
    }
}

/// BLS crypto verifier
pub struct BlsCryptoVerifier {
    pub bls_verification_key: BlsVerificationKey,
}

impl CryptoVerifier for BlsCryptoVerifier {
    type VerificationKey = BlsVerificationKey;
    type Signature = BlsSignature;

    fn verify(
        message: &[u8],
        signature: &Self::Signature,
        verification_key: &Self::VerificationKey,
    ) -> StdResult<()> {
        Ok(())
    }
}
