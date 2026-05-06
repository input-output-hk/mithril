// Circuits and SNARK work.
//
// This module is compiled only when the `future_snark` feature is enabled.
// The gate is applied in `src/lib.rs`
//
// Currently, we expose the Halo2-based prototype under `circuits::halo2`.

pub mod halo2;
pub mod halo2_ivc;
pub mod trusted_setup;

#[cfg(test)]
pub(crate) mod test_utils;

pub(crate) use halo2::witness::{
    CircuitInstance, CircuitMerkleTreeLeaf, CircuitWitness, MerklePath,
};

/// Constant holding the current path of the cached values related to the circuits
const MITHRIL_CIRCUIT_CACHE_FOLDER: &str = "mithril-circuit";
