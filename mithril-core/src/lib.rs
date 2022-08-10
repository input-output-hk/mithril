#![warn(missing_docs)]

//! Implementation of Stake-based Threshold Multisignatures

mod dense_mapping;
mod error;
pub mod key_reg;
mod merkle_tree;
pub mod stm;

#[cfg(not(feature = "zcash"))]
mod multi_sig;
#[cfg(feature = "zcash")]
mod multi_sig_zcash;

pub use crate::error::AggregationError;
