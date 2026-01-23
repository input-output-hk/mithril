// Circuits and SNARK work.
//
// This module is compiled only when the `future_snark` feature is enabled.
// The gate is applied in `src/lib.rs`
//
// Currently, we expose the Halo2-based prototype under `circuits::halo2`.

pub mod halo2;

#[cfg(all(test, feature = "future_snark"))]
pub(crate) mod test_utils;
