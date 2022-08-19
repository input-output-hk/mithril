#![warn(missing_docs)]

//! Implementation of Stake-based Threshold Multisignatures

mod dense_mapping;
mod error;
pub mod key_reg;
mod merkle_tree;
pub mod stm;

mod multi_sig_blast;

pub use crate::error::AggregationError;
