#![warn(missing_docs)]

//! Implementation of Mithril Threshold Stake-Based Signatures
//! [[paper](https://eprint.iacr.org/2021/916)].

pub mod key_reg;
pub mod merkle_tree;
mod mithril_curves;
pub mod mithril_proof;
pub mod models;
pub mod msp;
pub mod proof;
pub mod stm;
pub mod c_api;
