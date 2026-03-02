//! Halo2 prototype integration (feature-gated by `future_snark`).

pub mod circuit;
pub mod constants;
pub(crate) mod errors;
pub mod gadgets;
pub mod types;
pub(crate) mod utils;

#[cfg(test)]
pub(crate) mod golden;
