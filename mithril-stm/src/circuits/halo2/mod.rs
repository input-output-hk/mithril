//! Halo2 prototype integration (feature-gated by `future_snark`).

pub mod adapters;
// TODO(snark): remove `allow(dead_code)` once Halo2 modules are fully wired into STM.
#[cfg_attr(not(test), allow(dead_code))]
pub(crate) mod circuit;
pub mod errors;
#[cfg_attr(not(test), allow(dead_code))]
pub(crate) mod gadgets;
#[cfg_attr(not(test), allow(dead_code))]
pub(crate) mod types;

#[cfg(test)]
pub(crate) mod tests;
