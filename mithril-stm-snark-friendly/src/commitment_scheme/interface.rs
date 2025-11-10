use crate::*;

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
