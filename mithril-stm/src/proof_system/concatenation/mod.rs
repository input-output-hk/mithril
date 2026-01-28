mod aggregate_key;
mod clerk;
mod eligibility;
mod proof;
mod signer;
mod single_signature;

pub use aggregate_key::AggregateVerificationKeyForConcatenation;
pub use clerk::ConcatenationClerk;
pub(crate) use eligibility::is_lottery_won;
pub use proof::ConcatenationProof;
pub(crate) use signer::ConcatenationProofSigner;
pub(crate) use single_signature::SingleSignatureForConcatenation;
