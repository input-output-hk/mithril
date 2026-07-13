//! Tests that `collapse` preserves the `accumulator.check` pairing invariant.
//!
//! The off-circuit pipeline for each proof contribution is:
//! `Accumulator::from_dual_msm()` → `accumulator.check` → `collapse` → `accumulator.check`.
//! These tests lock in that both checkpoints pass for a valid stored certificate proof.

use crate::circuits::halo2_ivc::tests::common::helpers::build_certificate_accumulator_from_assets;

#[test]
fn certificate_accumulator_passes_check_before_collapse() {
    // accumulator.check must pass on a valid certificate proof.
    let (accumulator, certificate_fixed_bases, verif_params) =
        build_certificate_accumulator_from_assets();

    assert!(
        accumulator.check(&verif_params, &certificate_fixed_bases),
        "certificate accumulator should pass accumulator.check, before collapse"
    );
}

#[test]
fn certificate_accumulator_passes_check_after_collapse() {
    // collapse reduces the variable-base MSM to a single (point, 1) term without
    // changing the mathematical value. accumulator.check must still pass afterwards.
    let (mut accumulator, certificate_fixed_bases, verif_params) =
        build_certificate_accumulator_from_assets();

    accumulator.collapse();

    assert!(
        accumulator.check(&verif_params, &certificate_fixed_bases),
        "certificate accumulator should pass accumulator.check after collapse"
    );
}
