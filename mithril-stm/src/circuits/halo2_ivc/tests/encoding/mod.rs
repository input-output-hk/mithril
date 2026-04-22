//! Layer A: data and encoding invariant tests for the recursive Halo2 IVC circuit.
//!
//! These tests validate deterministic data rules — protocol message preimage
//! layout, byte offsets used by the circuit, state public input formatting, and
//! serialization round-trips — without invoking the prover or MockProver.

mod negative;
mod positive;
