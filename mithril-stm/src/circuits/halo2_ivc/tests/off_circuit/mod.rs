//! Layer C2: off-circuit accumulator and verification mechanics tests for the
//! recursive Halo2 IVC circuit.
//!
//! These tests validate the off-circuit side of the KZG accumulation pipeline:
//! accumulator construction, fixed-base resolution, collapse, folding, and
//! combined proof verification — as opposed to the in-circuit constraints
//! covered by Layer C1.
//!
//! `accumulator_collapse`        — `collapse` preserves the `accumulator.check` invariant.
//! `accumulator_construction`    — `trivial_accumulator` structure and public-input encoding.
//! `accumulator_update`          — full folding pipeline on stored assets; soundness under wrong previous accumulator.
//! `accumulator_verification`    — `accumulator.check` accepts valid stored accumulators and rejects tampered ones.
//! `certificate_proof_rejection` — `verify_and_prepare_accumulator` rejects garbage bytes and wrong public inputs.
//! `circuit_validation`          — `IvcCircuitData::validate_ivc_verification_key_degree` rejects a VK with degree ≠ RECURSIVE_CIRCUIT_DEGREE.
//! `fixed_base_resolution`       — `resolve_fixed_bases` expansion and encoding effects.
//! `proof_verification`          — `dual_msm.check` + `accumulator.check` combined for same-epoch and chain-state proofs.

mod accumulator_collapse;
mod accumulator_construction;
mod accumulator_update;
mod accumulator_verification;
mod certificate_proof_rejection;
mod circuit_validation;
mod fixed_base_resolution;
mod proof_verification;
