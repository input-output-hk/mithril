//! Tests that the circuit enforces previous IVC proof validity in non-genesis steps.
//!
//! A tampered previous IVC proof must be rejected by the real prover for both
//! same-epoch and next-epoch transitions. These are slow tests that require
//! actual proof generation; MockProver cannot catch proof byte manipulation.
