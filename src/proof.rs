//! Prove the validity of aggregated signatures.

pub trait ProofSystem {
    type Proof;
    type Statement;
    type Witness;

    type ProvingKey;
    type VerificationKey;

    /// Given a statement and a witness to its truth, produce a proof
    /// that there exists a witness such that the statement is true.
    fn prove(&self, pk: Self::ProvingKey, stmt: Self::Statement, wit: Self::Witness) -> Self::Proof;

    /// Check that `proof` is indeed a valid proof that there exists a witness to
    /// the truth of `stmt`
    fn verify(&self, vk: Self::VerificationKey, proof: &Self::Proof, stmt: &Self::Statement) -> bool;
}
