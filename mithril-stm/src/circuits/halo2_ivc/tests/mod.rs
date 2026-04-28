//! Test-only modules for the recursive Halo2 IVC integration.
//!
//! `common` groups the shared test infrastructure: asset readers, proof
//! helpers, and generator building blocks reused by both `golden` and
//! `encoding`.
//! `golden` groups the committed asset generators and golden test cases that
//! lock in recursive verification behavior.
//! `encoding` covers Layer A data and encoding invariants.

mod common;
mod encoding;
mod golden;
mod test_certificate;
