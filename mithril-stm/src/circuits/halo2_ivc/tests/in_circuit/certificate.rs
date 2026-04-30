//! Tests that the circuit enforces certificate proof validity in non-genesis steps.
//!
//! A tampered certificate proof must be rejected by the real prover for both
//! same-epoch and next-epoch transitions. These are slow tests that require
//! actual proof generation; MockProver cannot catch proof byte manipulation.
