//! Tests that the circuit enforces the accumulator output constraint.
//!
//! Fast tests use the asset-based verifier to confirm that a tampered
//! next_accumulator is rejected for each step type. A slow MockProver check
//! confirms the in-circuit accumulator update constraint is wired correctly.
