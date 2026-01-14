mod concatenation;

pub use concatenation::{ConcatenationClerk, ConcatenationProof, AggregateVerificationKeyForConcatenation};
pub(crate) use concatenation::{ConcatenationProofSigner, SingleSignatureForConcatenation};
