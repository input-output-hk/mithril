//! Tests that `collapse` preserves the `accumulator.check` pairing invariant.
//!
//! The off-circuit pipeline for each proof contribution is:
//! `DualMSM::into()` → `extract_fixed_bases` → `accumulator.check` → `collapse` → `accumulator.check`.
//! These tests lock in that both checkpoints pass for a valid stored certificate proof.

use std::collections::BTreeMap;

use midnight_proofs::poly::kzg::params::ParamsVerifierKZG;

use crate::circuits::halo2_ivc::{
    Accumulator, EmulatedCurve, PairingEngine, RecursiveEmulation,
    tests::common::helpers::build_unextracted_certificate_accumulator_from_assets,
};

/// Verifies the stored certificate proof and returns the accumulator after
/// `extract_fixed_bases` but before `collapse`, together with the certificate
/// fixed-base map and `tau_in_g2` needed to call `accumulator.check`.
fn build_extracted_certificate_accumulator() -> (
    Accumulator<RecursiveEmulation>,
    BTreeMap<String, EmulatedCurve>,
    ParamsVerifierKZG<PairingEngine>,
) {
    let (accumulator, certificate_fixed_bases, verif_params) =
        build_unextracted_certificate_accumulator_from_assets();
    (accumulator, certificate_fixed_bases, verif_params)
}

#[test]
fn certificate_accumulator_passes_check_after_extraction_before_collapse() {
    // After extract_fixed_bases the accumulator is in the right shape for the
    // pairing check: fixed-base terms are in fixed_base_scalars, variable-base
    // terms remain. accumulator.check must pass on a valid certificate proof.
    let (accumulator, certificate_fixed_bases, verif_params) =
        build_extracted_certificate_accumulator();

    assert!(
        accumulator.check(&verif_params, &certificate_fixed_bases),
        "certificate accumulator should pass accumulator.check after extraction, before collapse"
    );
}

#[test]
fn certificate_accumulator_passes_check_after_collapse() {
    // collapse reduces the variable-base MSM to a single (point, 1) term without
    // changing the mathematical value. accumulator.check must still pass afterwards.
    let (mut accumulator, certificate_fixed_bases, verif_params) =
        build_extracted_certificate_accumulator();

    accumulator.collapse();

    assert!(
        accumulator.check(&verif_params, &certificate_fixed_bases),
        "certificate accumulator should pass accumulator.check after collapse"
    );
}
