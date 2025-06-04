#![doc = include_str!("../README.md")]
//! Implementation of Stake-based Threshold Multisignatures

mod aggregate_signature;
mod bls_multi_signature;
mod eligibility_check;
mod error;
mod key_reg;
mod merkle_tree;
mod participant;
mod single_signature;
mod stm;

pub use aggregate_signature::{CoreVerifier, StmAggrSig, StmAggrVerificationKey, StmClerk};
pub use error::{
    AggregationError, CoreVerifierError, RegisterError, StmAggregateSignatureError,
    StmSignatureError,
};
pub use key_reg::{ClosedKeyReg, KeyReg};
pub use participant::{StmInitializer, StmSigner, StmVerificationKey, StmVerificationKeyPoP};
pub use single_signature::{StmSig, StmSigRegParty};
pub use stm::{Index, Stake, StmParameters};

#[cfg(feature = "benchmark-internals")]
pub use bls_multi_signature::{
    ProofOfPossession, Signature, SigningKey, VerificationKey, VerificationKeyPoP,
};
