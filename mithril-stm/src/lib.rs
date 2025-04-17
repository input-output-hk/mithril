#![warn(missing_docs)]
#![doc = include_str!("../README.md")]
//! Implementation of Stake-based Threshold Multisignatures

extern crate core;

mod eligibility_check;
mod error;
pub mod key_reg;
mod merkle_tree;
pub mod stm;

pub use crate::error::{
    AggregationError, CoreVerifierError, RegisterError, StmAggregateSignatureError,
    StmSignatureError,
};

pub use stm::StmAggrSigType;

#[cfg(feature = "benchmark-internals")]
pub mod bls_multi_signature;

#[cfg(not(feature = "benchmark-internals"))]
mod bls_multi_signature;
mod stm_telescope;
