#![warn(missing_docs)]
#![doc = include_str!("../README.md")]
//! Implementation of Stake-based Threshold Multisignatures

mod bls_multi_signature;
mod eligibility_check;
mod error;
pub mod key_reg;
mod merkle_tree;
mod participant;
#[deprecated(
    since = "0.4.0",
    note = "Use `mithril_stm::` instead of 'mithril_stm::stm`"
)]
pub mod stm;
mod stm_legacy;

pub use crate::error::{
    AggregationError, CoreVerifierError, RegisterError, StmAggregateSignatureError,
    StmSignatureError,
};
pub use participant::{StmInitializer, StmSigner, StmVerificationKey, StmVerificationKeyPoP};
pub use stm_legacy::{
    CoreVerifier, Index, Stake, StmAggrSig, StmAggrVerificationKey, StmClerk, StmParameters,
    StmSig, StmSigRegParty,
};

#[cfg(feature = "benchmark-internals")]
pub use crate::bls_multi_signature::{
    ProofOfPossession, Signature, SigningKey, VerificationKey, VerificationKeyPoP,
};
