// Circuits module (future work).
//
// This module is only reachable when building with the `future_snark` feature (see `src/lib.rs`).

#[cfg(feature = "future_snark")]
pub mod halo2;
