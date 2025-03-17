//! Merkle tree implementation for STM

#[cfg(feature = "batch")]
pub(super) mod batch_compatible;

#[cfg(not(feature = "batch"))]
pub(super) mod basic;

pub(super) mod leaf;
pub(super) mod tree;
