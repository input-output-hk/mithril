// Circuits and SNARK work.
//
// This module is compiled only when the `future_snark` feature is enabled.
// The gate is applied in `src/lib.rs`
//
// Currently, we expose the Halo2-based prototype under `circuits::halo2`.

#[cfg(not(target_family = "wasm"))]
pub(crate) mod circuit_key_generator;
#[cfg(not(target_family = "wasm"))]
pub(crate) mod circuit_verification_key_provider;
pub(crate) mod common;
pub mod halo2;
pub mod halo2_ivc;
pub mod key_cache;
pub(crate) mod key_serialization;
pub mod trusted_setup;

#[cfg(test)]
pub(crate) mod test_utils;

pub(crate) use halo2::witness::{
    CircuitInstance, CircuitMerkleTreeLeaf, CircuitWitness, MerklePath,
};

/// Exposes the raw PLONK verifying key wrapped by a per-circuit verifying-key newtype, for the Halo2
/// verification APIs (proof preparation, fixed-base extraction) that operate on the underlying key.
/// Lets helpers shared by the certificate and recursive circuits take the newtypes rather than the
/// raw key.
pub(crate) trait AsPlonkVerifyingKey {
    fn plonk_verifying_key(&self) -> &halo2_ivc::PlonkVerifyingKey;
}

/// Constant holding the current path of the cached values related to the circuits
const MITHRIL_CIRCUIT_CACHE_FOLDER: &str = "mithril-circuit";

#[cfg(all(
    feature = "future_snark",
    not(any(feature = "rustls", feature = "native-tls"))
))]
compile_error!(
    "Enabling `future_snark` requires exactly one of the `rustls` or `native-tls` features."
);
