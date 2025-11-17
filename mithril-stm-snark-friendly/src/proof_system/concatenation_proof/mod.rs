pub mod basic;
pub mod full;

use crate::{StdResult, core::Digest, signature_scheme::bls_signature::BlsSignature};

/// Blake digest implementation
pub struct BlakeDigest;

impl Digest for BlakeDigest {
    fn digest(_data: &[u8]) -> Vec<u8> {
        todo!("Implement Blake digest")
    }
}

/// Concatenation proof individual signature structure
#[derive(Default)]
pub struct ConcatenationSingleSignature {
    pub signature: BlsSignature,
    pub lottery_indices: Vec<u64>,
}

/// Aggregate verification key for concatenation proof
pub type ConcatenationAggregateVerificationKey = (); // TODO: to be defined

/// Concatenation proof structure
pub struct ConcatenationProof {}

impl ConcatenationProof {
    /// Verifies the concatenation proof (basic)
    pub fn verify_basic(
        &self,
        message: &[u8],
        verification_key: &ConcatenationAggregateVerificationKey,
    ) -> StdResult<()> {
        todo!("Implement concatenation basic proof verification")
    }

    /// Verifies the concatenation proof (full)
    pub fn verify(
        &self,
        message: &[u8],
        verification_key: &ConcatenationAggregateVerificationKey,
    ) -> StdResult<()> {
        self.verify_basic(message, verification_key)?;
        todo!("Implement concatenation full proof verification")
    }
}
