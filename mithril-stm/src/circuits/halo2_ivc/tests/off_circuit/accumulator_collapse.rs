//! Tests that `collapse` preserves the `accumulator.check` pairing invariant.
//!
//! The off-circuit pipeline for each proof contribution is:
//! `DualMSM::into()` → `extract_fixed_bases` → `accumulator.check` → `collapse` → `accumulator.check`.
//! These tests lock in that both checkpoints pass for a valid stored certificate proof.

use std::collections::BTreeMap;

use midnight_curves::pairing::Engine;

use crate::circuits::halo2_ivc::{
    Accumulator, C, E, S,
    tests::common::{
        asset_readers::{
            load_embedded_recursive_chain_state_asset, load_embedded_recursive_step_output_asset,
            load_embedded_verification_context_asset,
        },
        generators::{build_recursive_fixed_bases, certificate_public_inputs_for_step},
        helpers::verify_prepare_poseidon_recursive_proof,
    },
};

/// Verifies the stored certificate proof and returns the accumulator after
/// `extract_fixed_bases` but before `collapse`, together with the certificate
/// fixed-base map and `tau_in_g2` needed to call `accumulator.check`.
///
/// Uses the next-epoch step assets: the certificate proof lives in
/// `recursive_step_output` and its public inputs are derived from
/// `(recursive_chain_state.state, recursive_step_output.next_state)`.
fn build_extracted_certificate_accumulator()
-> (Accumulator<S>, BTreeMap<String, C>, <E as Engine>::G2Affine) {
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let recursive_chain_state = load_embedded_recursive_chain_state_asset()
        .expect("recursive chain state asset should load");
    let recursive_step_output = load_embedded_recursive_step_output_asset()
        .expect("recursive step output asset should load");

    let (certificate_fixed_bases, _, _) = build_recursive_fixed_bases(
        &verification_context.certificate_verifying_key,
        &verification_context.recursive_verifying_key,
    );

    let certificate_public_inputs = certificate_public_inputs_for_step(
        &recursive_chain_state.state,
        &recursive_step_output.next_state,
    );

    let dual_msm = verify_prepare_poseidon_recursive_proof(
        verification_context.certificate_verifying_key.vk(),
        &recursive_step_output.certificate_proof,
        &certificate_public_inputs,
    );

    let mut accumulator: Accumulator<S> = dual_msm.into();
    accumulator.extract_fixed_bases(&certificate_fixed_bases);

    (
        accumulator,
        certificate_fixed_bases,
        verification_context.verifier_tau_in_g2,
    )
}

#[test]
fn certificate_accumulator_passes_check_after_extraction_before_collapse() {
    // After extract_fixed_bases the accumulator is in the right shape for the
    // pairing check: fixed-base terms are in fixed_base_scalars, variable-base
    // terms remain. accumulator.check must pass on a valid certificate proof.
    let (accumulator, certificate_fixed_bases, tau_in_g2) =
        build_extracted_certificate_accumulator();

    assert!(
        accumulator.check(&tau_in_g2, &certificate_fixed_bases),
        "certificate accumulator should pass accumulator.check after extraction, before collapse"
    );
}

#[test]
fn certificate_accumulator_passes_check_after_collapse() {
    // collapse reduces the variable-base MSM to a single (point, 1) term without
    // changing the mathematical value. accumulator.check must still pass afterwards.
    let (mut accumulator, certificate_fixed_bases, tau_in_g2) =
        build_extracted_certificate_accumulator();

    accumulator.collapse();

    assert!(
        accumulator.check(&tau_in_g2, &certificate_fixed_bases),
        "certificate accumulator should pass accumulator.check after collapse"
    );
}
