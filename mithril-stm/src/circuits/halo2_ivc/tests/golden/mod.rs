//! Golden tests lock in recursive Halo2 IVC verification behavior.
//! This subtree also owns the committed assets, asset readers, generators, and
//! scenario/docs for the golden workflow.

pub(crate) const ASSET_SEED: u64 = 42;
pub(crate) const CERTIFICATE_CIRCUIT_DEGREE: u32 = 13;
pub(crate) const RECURSIVE_CIRCUIT_DEGREE: u32 = 19;

mod asset_readers;
mod cases;
mod generators;
mod helpers;
