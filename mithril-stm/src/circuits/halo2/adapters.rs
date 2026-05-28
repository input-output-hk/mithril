//! Compatibility re-exports for STM-to-Halo2 circuit witness adapters.
//!
//! The shared Merkle path adapter now lives in `circuits::common::merkle`. This module keeps the
//! legacy Halo2 adapter error path available for existing callers.

pub use crate::circuits::common::merkle::MerklePathAdapterError;
