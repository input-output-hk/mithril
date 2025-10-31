use super::*;

pub struct SchnorrSigningKey {}

pub struct SchnorrVerificationKey {}

#[derive(PartialEq, Eq, Clone)]
pub struct SchnorrVerificationKeyProofOfPossession {}

pub struct SchnorrSignature {}

pub struct SchnorrCryptoInitializer {
    seed: Vec<u8>,
}

impl CryptoInitializer for SchnorrCryptoInitializer {
    type SigningKey = SchnorrSigningKey;
    type VerificationKey = SchnorrVerificationKey;
    type VerificationKeyProofOfPossession = SchnorrVerificationKeyProofOfPossession;

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

pub struct SchnorrCryptoSigner {
    pub bls_signing_key: SchnorrSigningKey,
}

impl CryptoSigner for SchnorrCryptoSigner {
    type SigningKey = SchnorrSigningKey;
    type VerificationKey = SchnorrVerificationKey;
    type VerificationKeyProofOfPossession = SchnorrVerificationKeyProofOfPossession;
    type Signature = SchnorrSignature;

    fn sign(message: &[u8], signing_key: &Self::SigningKey) -> StdResult<Self::Signature> {
        todo!("Implement BLS sign")
    }
}

pub struct SchnorrCryptoVerifier {
    pub bls_verification_key: SchnorrVerificationKey,
}

impl CryptoVerifier for SchnorrCryptoVerifier {
    type VerificationKey = SchnorrVerificationKey;
    type Signature = SchnorrSignature;

    fn verify(
        message: &[u8],
        signature: &Self::Signature,
        verification_key: &Self::VerificationKey,
    ) -> StdResult<()> {
        todo!("Implement BLS verify")
    }
}
