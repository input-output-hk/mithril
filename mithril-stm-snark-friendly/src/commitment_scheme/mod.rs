use crate::{
    StdResult,
    commitment_scheme::merkle_tree::MerkleTreeLeaf,
    core::{Digest, key_registration::SignerRegistration},
};

pub mod merkle_tree;
pub mod pedersen_commitment;

/// Signer registration commitment generator trait
pub trait SignerRegistrationCommitmentGenerator {
    type SignerCommitment;

    /// Creates a signer registration commitment
    fn create_signer_registration_commitment(&self) -> StdResult<Self::SignerCommitment>;
}

/// Signer registration registerer trait
pub trait SignerRegistrationRegisterer {
    type RegistrationInput;
    type RegistrationIndex;

    /// Registers an entry and returns its index
    fn register_entry(
        &self,
        reveal: &Self::RegistrationInput,
    ) -> StdResult<Self::RegistrationIndex>;
}

/// Signer registration reveal prover trait
pub trait SignerRegistrationRevealProver {
    type RevealInput;
    type RevealProof;

    /// Creates a reveal proof
    fn create_reveal_proof(&self, reveal: &Self::RevealInput) -> StdResult<Self::RevealProof>;
}

/// Signer registration reveal verifier trait
pub trait SignerRegistrationRevealVerifier {
    type RevealProof;

    /// Verifies a reveal proof
    fn verify_reveal_proof(&self, proof: &Self::RevealProof) -> StdResult<()>;
}

/// CommitmentConstraints defines the associated types needed for commitment schemes
pub trait MembershipCommitmentConstraints {
    type ConcatenationHash: Digest;
    type ConcatenationCommittedData: From<SignerRegistration> + MerkleTreeLeaf;
    #[cfg(feature = "future_snark")]
    type SnarkHash: Digest;
    #[cfg(feature = "future_snark")]
    type SnarkCommittedData: From<SignerRegistration> + MerkleTreeLeaf;
}
