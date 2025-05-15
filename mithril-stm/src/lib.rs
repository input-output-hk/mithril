#![doc = include_str!("../README.md")]
//! Implementation of Stake-based Threshold Multisignatures

mod bls_multi_signature;

mod eligibility_check;
mod error;
mod key_reg;
mod merkle_tree;
mod participant;
mod stm;

pub use crate::error::{
    AggregationError, CoreVerifierError, RegisterError, StmAggregateSignatureError,
    StmSignatureError,
};
pub use participant::{StmInitializer, StmSigner, StmVerificationKey, StmVerificationKeyPoP};
pub use stm::{
    CoreVerifier, Index, Stake, StmAggrSig, StmAggrVerificationKey, StmClerk, StmParameters,
    StmSig, StmSigRegParty,
};

#[cfg(feature = "benchmark-internals")]
pub use crate::bls_multi_signature::{
    ProofOfPossession, Signature, SigningKey, VerificationKey, VerificationKeyPoP,
};

pub use key_reg::{ClosedKeyReg, KeyReg};
