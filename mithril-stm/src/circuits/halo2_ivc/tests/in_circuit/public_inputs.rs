//! Tests that the circuit enforces its public input layout.
//!
//! Fast tests use the asset-based verifier to confirm that tampered global
//! state fields or accumulator bytes are rejected without running the circuit.
//! Slow tests use MockProver to confirm the in-circuit constraints are wired.
