//! Layer A: data and encoding invariant tests for the recursive Halo2 IVC circuit.
//!
//! These tests validate deterministic data rules — protocol message preimage
//! layout, byte offsets used by the circuit, state public input formatting, and
//! serialization round-trips. Most cases exercise these invariants without
//! invoking the prover, while some negative-path checks use MockProver and
//! verifier validation.

mod negative;
mod positive;
