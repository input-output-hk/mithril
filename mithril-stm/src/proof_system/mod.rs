mod concatenation;
#[cfg(feature = "future_snark")]
mod halo2_snark;

pub use concatenation::{
    AggregateVerificationKeyForConcatenation, ConcatenationClerk, ConcatenationProof,
};
pub(crate) use concatenation::{ConcatenationProofSigner, SingleSignatureForConcatenation};

#[cfg(feature = "future_snark")]
pub use halo2_snark::{AggregateVerificationKeyForSnark, SnarkClerk};
