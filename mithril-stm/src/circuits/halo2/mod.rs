//! Halo2 STM circuit integration (feature-gated by `future_snark`).
//!
//! Module map:
//! - `circuit`: relation orchestration and top-level constraint flow
//! - `assignments`: witness assignment into Halo2 layouter values
//! - `witness`: circuit-facing witness and instance contract
//! - `adapters`: STM-to-circuit conversions for boundary types
//! - `gadgets`: reusable constraint logic split by domain
//! - `errors`: typed circuit errors and backend synthesis adaptation
//! - `tests/golden`: end-to-end circuit scenarios
//! - inline `#[cfg(test)]` blocks in `gadgets/*` and `adapters`: focused regression checks
//! - `tests/test_helpers`: shared harness for focused gadget tests

pub mod adapters;
#[cfg_attr(not(test), allow(dead_code))]
pub(crate) mod assignments;
// TODO(snark): remove `allow(dead_code)` once Halo2 modules are fully wired into STM.
#[cfg_attr(not(test), allow(dead_code))]
pub(crate) mod circuit;
pub mod errors;
#[cfg_attr(not(test), allow(dead_code))]
pub(crate) mod gadgets;
#[cfg_attr(not(test), allow(dead_code))]
pub(crate) mod types;
#[cfg_attr(not(test), allow(dead_code))]
pub(crate) mod witness;

#[cfg(test)]
pub(crate) mod tests;
