use crate::*;

pub trait SignerRegistrationCommitmentGenerator {
    type SignerCommitment;

    fn create_signer_registration_commitment(&self) -> StdResult<Self::SignerCommitment>;
}

pub trait SignerRegistrationRegisterer {
    type SignerRegistrationInput;
    type SignerregistrationIndex;

    fn register_entry(
        &self,
        reveal: &Self::SignerRegistrationInput,
    ) -> StdResult<Self::SignerregistrationIndex>;
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
