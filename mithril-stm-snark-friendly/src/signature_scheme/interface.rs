use crate::*;

pub trait CryptoSigner {
    type SigningKey;
    type VerificationKey;
    type VerificationKeyProofOfPossession;
    type Signature;

    fn sign(message: &[u8], signing_key: &Self::SigningKey) -> StdResult<Self::Signature>;
}

pub trait CryptoVerifier {
    type VerificationKey;
    type Signature;

    fn verify(
        message: &[u8],
        signature: &Self::Signature,
        verification_key: &Self::VerificationKey,
    ) -> StdResult<()>;
}

pub trait CryptoInitializer {
    type SigningKey;
    type VerificationKey;
    type VerificationKeyProofOfPossession;

    fn get_signing_key(&self) -> &Self::SigningKey;

    fn get_verification_key(&self) -> &Self::VerificationKey;

    fn get_verification_key_proof_of_possession(&self) -> &Self::VerificationKeyProofOfPossession;
}
