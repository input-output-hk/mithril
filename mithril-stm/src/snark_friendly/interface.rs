use super::*;

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

pub trait SignerRegistrationCommitmentGenerator {
    fn create_signer_registration_commitment(&self) -> StdResult<()>;
}

pub trait SignerRegistrationRevealProver {
    type SignerRevealInput;
    type SignerRevealProof;

    fn create_reveal_proof(
        &self,
        reveal: &Self::SignerRevealInput,
    ) -> StdResult<Self::SignerRevealProof>;
}

pub trait SignerRegistrationRevealVerifier {
    type SignerRevealProof;

    fn verify_reveal_proof(&self, proof: &Self::SignerRevealProof) -> StdResult<()>;
}

pub trait ProofSystemSingleSignatureGenerator {
    type ProofSystemSingleSignature;

    fn create_individual_signature(
        &self,
        message: &[u8],
    ) -> StdResult<Self::ProofSystemSingleSignature>;
}
