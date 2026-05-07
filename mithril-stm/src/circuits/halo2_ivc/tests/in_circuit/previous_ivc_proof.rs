//! Tests that the circuit correctly enforces previous IVC proof validity in non-genesis steps.
//!
//! Fast tests confirm that the verifier accepts stored proofs with a valid previous
//! IVC proof for both same-epoch and next-epoch transitions. An asset-based negative
//! test confirms that tampered IVC proof bytes produce an accumulator that fails the
//! off-circuit pairing check — confirming the KZG system catches the forgery.

use midnight_circuits::types::Instantiable;

use crate::StmResult;
use crate::circuits::halo2_ivc::{
    AssignedAccumulator,
    tests::common::{
        asset_readers::{
            RecursiveStepOutputAsset, load_embedded_recursive_chain_state_asset,
            load_embedded_recursive_step_output_asset, load_embedded_same_epoch_step_output_asset,
            load_embedded_verification_context_asset,
        },
        helpers::{
            try_verify_prepare_poseidon_recursive_proof, verify_prepare_blake2b_recursive_proof,
        },
    },
};

/// Loads a step output via `load_step_output` and asserts the verifier accepts
/// the proof, confirming the previous IVC proof is correctly folded into the step.
fn assert_valid_previous_ivc_proof_is_accepted(
    load_step_output: impl FnOnce() -> StmResult<RecursiveStepOutputAsset>,
    load_label: &str,
    acceptance_message: &str,
) {
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let step_output =
        load_step_output().unwrap_or_else(|_| panic!("{load_label} asset should load"));

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        step_output.next_state.as_public_input(),
        AssignedAccumulator::as_public_input(&step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &step_output.proof,
        &public_inputs,
    );

    assert!(
        dual_msm.check(&verification_context.verifier_params),
        "{acceptance_message}"
    );
}

#[test]
fn same_epoch_step_with_valid_previous_ivc_proof_is_accepted() {
    // Asset-based check that the verifier accepts the stored same-epoch proof,
    // confirming a valid previous IVC proof is correctly folded in a same-epoch step.
    assert_valid_previous_ivc_proof_is_accepted(
        load_embedded_same_epoch_step_output_asset,
        "same-epoch step output",
        "same-epoch step with a valid previous IVC proof should be accepted by the verifier",
    );
}

#[test]
fn next_epoch_step_with_valid_previous_ivc_proof_is_accepted() {
    // Asset-based check that the verifier accepts the stored next-epoch proof,
    // confirming a valid previous IVC proof is correctly folded in a next-epoch step.
    assert_valid_previous_ivc_proof_is_accepted(
        load_embedded_recursive_step_output_asset,
        "recursive step output",
        "next-epoch step with a valid previous IVC proof should be accepted by the verifier",
    );
}

#[test]
fn tampered_previous_ivc_proof_produces_invalid_accumulator() {
    // Asset-based check: flips one byte in the stored previous IVC proof bytes and
    // verifies off-circuit. Returns without asserting if the bytes are too malformed
    // to deserialize (forgery caught immediately). If they parse, confirms the KZG
    // pairing check fails — the accumulator correctly rejects tampered opening terms.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let recursive_chain_state = load_embedded_recursive_chain_state_asset()
        .expect("recursive chain state asset should load");

    let previous_public_inputs = [
        verification_context.global_field_elements.clone(),
        recursive_chain_state.state.as_public_input(),
        AssignedAccumulator::as_public_input(&recursive_chain_state.accumulator),
    ]
    .concat();

    let mut tampered_ivc_proof = recursive_chain_state.proof.clone();
    tampered_ivc_proof[0] ^= 0xFF;

    // Try to parse the tampered IVC proof off-circuit. Returns None if the bytes
    // are too malformed to deserialize (forgery caught immediately). Returns
    // Some(dual_msm) if they parse but carry invalid KZG opening terms.
    match try_verify_prepare_poseidon_recursive_proof(
        &verification_context.recursive_verifying_key,
        &tampered_ivc_proof,
        &previous_public_inputs,
    ) {
        None => {} // bytes rejected at deserialization — forgery caught
        Some(tampered_dual_msm) => {
            assert!(
                !tampered_dual_msm.check(&verification_context.verifier_params),
                "tampered IVC proof bytes should fail the KZG accumulator pairing check"
            );
        }
    }
}
