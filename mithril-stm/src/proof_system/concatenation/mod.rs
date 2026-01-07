mod proof;
mod signer;
mod single_signature;

pub use proof::ConcatenationProof;
pub(crate) use signer::ConcatenationProofSigner;
pub(crate) use single_signature::SingleSignatureForConcatenation;
