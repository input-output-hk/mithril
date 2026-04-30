//! Layer B: state transition rule tests for the recursive Halo2 IVC circuit.
//!
//! These tests validate the chain link rules — genesis, same-epoch, and
//! next-epoch transitions — by verifying stored proofs with correct and
//! tampered public inputs. Most cases are fast (verifier only); slow checks
//! use MockProver to confirm the circuit enforces the rules in-circuit.

mod negative;
mod positive;
