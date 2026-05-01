//! Positive transition tests: stored proofs verified against correct public inputs.

use midnight_circuits::types::Instantiable;

use crate::StmResult;
use crate::circuits::halo2_ivc::{
    AssignedAccumulator,
    tests::common::{
        asset_readers::{
            RecursiveStepOutputAsset, load_embedded_genesis_step_output_asset,
            load_embedded_recursive_step_output_asset, load_embedded_same_epoch_step_output_asset,
            load_embedded_verification_context_asset,
        },
        helpers::verify_prepare_blake2b_recursive_proof,
    },
};

/// Loads a step output via `load_step_output` and asserts the verifier accepts
/// the proof against the correct public inputs.
fn assert_step_proof_verifies(
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
fn genesis_step_proof_verifies() {
    // Asset-based check that the stored genesis Blake2b proof verifies against the correct
    // public inputs, confirming the genesis base-case asset is valid.
    assert_step_proof_verifies(
        load_embedded_genesis_step_output_asset,
        "genesis step output",
        "genesis step proof should verify against the correct public inputs",
    );
}

#[test]
fn same_epoch_step_proof_verifies() {
    // Asset-based check that the stored same-epoch Blake2b proof verifies against the correct
    // public inputs, confirming the same-epoch asset is valid.
    assert_step_proof_verifies(
        load_embedded_same_epoch_step_output_asset,
        "same-epoch step output",
        "same-epoch step proof should verify against the correct public inputs",
    );
}

#[test]
fn next_epoch_step_proof_verifies() {
    // Asset-based check that the stored next-epoch Blake2b proof verifies against the correct
    // public inputs, confirming the next-epoch asset is valid.
    assert_step_proof_verifies(
        load_embedded_recursive_step_output_asset,
        "recursive step output",
        "next-epoch step proof should verify against the correct public inputs",
    );
}
