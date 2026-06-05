//! Tests the composed off-circuit verification path:
//! `verify_prepare` → `dual_msm.check` → `accumulator.check`
//! for the two step types not covered by `golden/positive.rs`.
//!
//! `golden/positive.rs::recursive_step_output_asset_proof_and_accumulator_are_valid`
//! already covers the next-epoch Blake2b proof with both checks in sequence.
//! These tests add the same combined check for:
//!   - the same-epoch Blake2b proof (`same_epoch_step_output`)
//!   - the chain-state Poseidon proof (`recursive_chain_state`)

use midnight_circuits::types::Instantiable;

use crate::circuits::halo2_ivc::{
    AssignedAccumulator,
    tests::common::{
        asset_readers::{
            load_embedded_recursive_chain_state_asset, load_embedded_same_epoch_step_output_asset,
            load_embedded_verification_context_asset,
        },
        helpers::{
            verify_prepare_blake2b_recursive_proof, verify_prepare_poseidon_recursive_proof,
        },
    },
};

#[test]
fn same_epoch_proof_passes_dual_msm_check_and_accumulator_check() {
    // The stored same-epoch Blake2b proof must pass both the KZG opening check
    // (`dual_msm.check`) and the pairing equation check (`accumulator.check`),
    // confirming the stored same-epoch asset is fully self-consistent.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let same_epoch_step_output = load_embedded_same_epoch_step_output_asset()
        .expect("same-epoch step output asset should load");

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        same_epoch_step_output.next_state.as_public_input(),
        AssignedAccumulator::as_public_input(&same_epoch_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        same_epoch_step_output.ivc_proof.as_bytes(),
        &public_inputs,
    );

    assert!(
        dual_msm.check(&verification_context.verifier_params),
        "same-epoch Blake2b proof should pass the KZG opening check"
    );
    assert!(
        same_epoch_step_output.next_accumulator.check(
            &verification_context.verifier_tau_in_g2,
            &verification_context.combined_fixed_bases,
        ),
        "same-epoch next accumulator should satisfy the pairing equation"
    );
}

#[test]
fn chain_state_proof_passes_dual_msm_check_and_accumulator_check() {
    // The stored chain-state Poseidon proof must pass both the KZG opening check
    // (`dual_msm.check`) and the pairing equation check (`accumulator.check`),
    // confirming the stored chain-state asset is fully self-consistent.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let recursive_chain_state = load_embedded_recursive_chain_state_asset()
        .expect("recursive chain state asset should load");

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        recursive_chain_state.state.as_public_input(),
        AssignedAccumulator::as_public_input(&recursive_chain_state.accumulator),
    ]
    .concat();

    let dual_msm = verify_prepare_poseidon_recursive_proof(
        &verification_context.recursive_verifying_key,
        recursive_chain_state.ivc_proof.as_bytes(),
        &public_inputs,
    );

    assert!(
        dual_msm.check(&verification_context.verifier_params),
        "chain-state Poseidon proof should pass the KZG opening check"
    );
    assert!(
        recursive_chain_state.accumulator.check(
            &verification_context.verifier_tau_in_g2,
            &verification_context.combined_fixed_bases,
        ),
        "chain-state accumulator should satisfy the pairing equation"
    );
}
