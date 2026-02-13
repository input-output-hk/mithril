//! Halo2 prototype integration (feature-gated by `future_snark`).

pub mod circuit;
pub mod constants;
pub mod gadgets;
pub(crate) mod utils;
pub(crate) mod error;
pub mod hash;
pub mod off_circuit;
pub mod types;

#[cfg(test)]
pub(crate) mod golden;
