use super::*;

#[derive(Clone)]
pub struct BlsSigningKey {}

pub struct BlsVerificationKey {}

#[derive(PartialEq, Eq, Clone)]
pub struct BlsVerificationKeyProofOfPossession {}

pub struct BlsSignature {}

pub struct BlsCryptoInitializer {
    seed: Vec<u8>,
}

impl CryptoInitializer for BlsCryptoInitializer {
    type SigningKey = BlsSigningKey;
    type VerificationKey = BlsVerificationKey;
    type VerificationKeyProofOfPossession = BlsVerificationKeyProofOfPossession;

    fn get_signing_key(&self) -> &Self::SigningKey {
        todo!("Implement get_signing_key")
    }

    fn get_verification_key(&self) -> &Self::VerificationKey {
        todo!("Implement get_verification_key")
    }

    fn get_verification_key_proof_of_possession(&self) -> &Self::VerificationKeyProofOfPossession {
        todo!("Implement get_verification_key_proof_of_possession")
    }
}

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
        todo!("Implement BLS sign")
    }
}

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
        todo!("Implement BLS verify")
    }
}
