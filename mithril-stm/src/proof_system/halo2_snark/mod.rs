mod aggregate_key;
mod eligibility;
mod signer;
mod single_signature;

#[cfg(feature = "future_snark")]
pub(crate) use signer::SnarkProofSigner;
