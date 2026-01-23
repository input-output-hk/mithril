mod concatenation;

pub use concatenation::{
    AggregateVerificationKeyForConcatenation, ConcatenationClerk, ConcatenationProof,
};
pub(crate) use concatenation::{ConcatenationProofSigner, SingleSignatureForConcatenation};
