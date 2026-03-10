//! Halo2 prototype integration (feature-gated by `future_snark`).

pub mod adapters;
// Temporary while Halo2 production wiring is in progress.
// Remove once these modules are wired into STM.
#[cfg_attr(not(test), allow(dead_code))]
pub(crate) mod circuit;
pub mod errors;
#[cfg_attr(not(test), allow(dead_code))]
pub(crate) mod gadgets;
#[cfg_attr(not(test), allow(dead_code))]
pub(crate) mod types;

#[cfg(test)]
pub(crate) mod tests;
