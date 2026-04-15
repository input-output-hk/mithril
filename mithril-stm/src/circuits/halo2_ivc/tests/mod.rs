//! Test-only modules for the recursive Halo2 IVC integration.
//!
//! `golden` covers asset-backed recursive verification scenarios. The asset
//! readers and generators support those golden tests and the manual asset
//! workflow.

pub(crate) mod asset_readers;
pub(crate) mod generators;
pub(crate) mod golden;
mod test_certificate;
