#![warn(missing_docs)]

//! Implementation of Mithril Threshold Stake-Based Signatures
//! [[paper](https://eprint.iacr.org/2021/916)].

mod dense_mapping;
pub mod error;
pub mod key_reg;
pub mod merkle_tree;
mod multi_sig;
pub mod stm;
