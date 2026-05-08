//! Layer C2: off-circuit accumulator and verification mechanics tests for the
//! recursive Halo2 IVC circuit.
//!
//! These tests validate the off-circuit side of the KZG accumulation pipeline:
//! accumulator construction, fixed-base extraction, collapse, folding, and
//! combined proof verification — as opposed to the in-circuit constraints
//! covered by Layer C1.
//!
//! `accumulator_construction`  — `trivial_acc` structure and public-input encoding.
//! `fixed_base_extraction`     — `extract_fixed_bases` reclassification and encoding effects.
//! `accumulator_collapse`      — `collapse` preserves the `accumulator.check` invariant.
//! `accumulator_verification`  — `accumulator.check` accepts valid stored accumulators and rejects tampered ones.

mod accumulator_collapse;
mod accumulator_construction;
mod accumulator_verification;
mod fixed_base_extraction;
