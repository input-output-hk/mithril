use crate::*;

/// Proof system single signature generator trait
pub trait ProofSystemSingleSignatureGenerator {
    type ProofSystemSingleSignature;

    /// Creates an individual proof system signature
    fn create_individual_signature(
        &self,
        message: &[u8],
    ) -> StdResult<Self::ProofSystemSingleSignature>;
}

/// Proof system aggregate signature prover trait
pub trait ProofSystemAggregateSignatureProver {
    type ProofSystemAggregateSignature;

    /// Creates an aggregate signature from individual signatures
    fn create_aggregate_signature(
        &self,
        message: &[u8],
        signatures: &[SingleSignature],
    ) -> StdResult<Self::ProofSystemAggregateSignature>;
}

/// Proof system aggregate signature verifier trait
pub trait ProofSystemAggregateSignatureVerifier {
    type ProofSystemAggregateSignature;
    type ProofSystemAggregateVerificationKey;

    fn verify_multi_signature(
        &self,
        message: &[u8],
        multi_signature: &Self::ProofSystemAggregateSignature,
        verification_key: &Self::ProofSystemAggregateVerificationKey,
    ) -> StdResult<()>;
}
