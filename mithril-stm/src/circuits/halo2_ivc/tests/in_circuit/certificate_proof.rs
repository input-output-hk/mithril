//! Tests that the circuit correctly enforces certificate proof validity in non-genesis steps.
//!
//! Fast tests confirm that the verifier accepts stored proofs with a valid certificate
//! proof for both same-epoch and next-epoch transitions.

use midnight_circuits::types::Instantiable;

use crate::StmResult;
use crate::circuits::halo2_ivc::{
    AssignedAccumulator,
    tests::common::{
        asset_readers::{
            RecursiveStepOutputAsset, load_embedded_recursive_step_output_asset,
            load_embedded_same_epoch_step_output_asset, load_embedded_verification_context_asset,
        },
        helpers::verify_prepare_blake2b_recursive_proof,
    },
};

/// Loads a step output via `load_step_output` and asserts the verifier accepts
/// the proof, confirming the certificate proof is correctly folded into the step.
fn assert_valid_certificate_proof_is_accepted(
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
fn same_epoch_step_with_valid_certificate_proof_is_accepted() {
    // Asset-based check that the verifier accepts the stored same-epoch proof,
    // confirming a valid certificate proof is correctly folded in a same-epoch step.
    assert_valid_certificate_proof_is_accepted(
        load_embedded_same_epoch_step_output_asset,
        "same-epoch step output",
        "same-epoch step with a valid certificate proof should be accepted by the verifier",
    );
}

#[test]
fn next_epoch_step_with_valid_certificate_proof_is_accepted() {
    // Asset-based check that the verifier accepts the stored next-epoch proof,
    // confirming a valid certificate proof is correctly folded in a next-epoch step.
    assert_valid_certificate_proof_is_accepted(
        load_embedded_recursive_step_output_asset,
        "recursive step output",
        "next-epoch step with a valid certificate proof should be accepted by the verifier",
    );
}
