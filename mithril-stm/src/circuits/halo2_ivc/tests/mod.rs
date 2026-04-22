//! Test-only modules for the recursive Halo2 IVC integration.
//!
//! `golden` groups the committed assets, asset readers, generators, and test
//! cases used to lock in recursive verification behavior.
//! `encoding` covers Layer A data and encoding invariants.

mod encoding;
mod golden;
mod test_certificate;
