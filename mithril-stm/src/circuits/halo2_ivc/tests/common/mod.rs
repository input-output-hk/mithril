//! Shared test infrastructure for the recursive Halo2 IVC circuit.
//!
//! This module owns the asset readers, proof helpers, and generator building
//! blocks reused by both the `golden` and `encoding` test suites.

pub(crate) const ASSET_SEED: u64 = 42;
pub(crate) const CERTIFICATE_CIRCUIT_DEGREE: u32 = 13;
pub(crate) const RECURSIVE_CIRCUIT_DEGREE: u32 = 19;

pub(crate) mod asset_readers;
pub(crate) mod generators;
pub(crate) mod helpers;
