//! Halo2 prototype integration (feature-gated by `future_snark`).

#[cfg(test)]
pub(crate) mod adapters;
pub mod circuit;
pub(crate) mod errors;
pub(crate) mod gadgets;
pub mod types;

#[cfg(test)]
pub(crate) mod golden;
