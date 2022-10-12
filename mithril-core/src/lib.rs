#![warn(missing_docs)]
//! Implementation of Stake-based Threshold Multisignatures

mod dense_mapping;
mod error;
pub mod key_reg;
mod merkle_tree;
pub mod stm;
pub mod stm_batch_compat;

mod multi_sig;

pub use crate::error::{AggregationError, RegisterError};
