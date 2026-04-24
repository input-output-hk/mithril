use midnight_circuits::types::Instantiable;

use crate::circuits::halo2_ivc::{
    AssignedAccumulator,
    tests::common::{
        asset_readers::{
            load_embedded_genesis_step_output_asset,
            load_embedded_same_epoch_step_output_asset,
            load_embedded_verification_context_asset,
        },
        helpers::verify_and_prepare_blake2b_recursive_proof,
    },
};

#[test]
fn genesis_step_proof_verifies() {
    // Asset-based check that the stored genesis Blake2b proof verifies against
    // the correct public inputs, confirming the genesis base-case asset is valid
    // and the verifier accepts a well-formed genesis transition.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let genesis_step_output =
        load_embedded_genesis_step_output_asset().expect("genesis step output asset should load");

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        genesis_step_output.next_state.as_public_input(),
        AssignedAccumulator::as_public_input(&genesis_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &genesis_step_output.proof,
        &public_inputs,
    );

    assert!(
        dual_msm.check(&verification_context.verifier_params),
        "genesis step proof should verify against the correct public inputs"
    );
}

#[test]
fn same_epoch_step_proof_verifies() {
    // Asset-based check that the stored same-epoch Blake2b proof verifies
    // against the correct public inputs, confirming the same-epoch asset is
    // valid and the verifier accepts a well-formed same-epoch transition.
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

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &same_epoch_step_output.proof,
        &public_inputs,
    );

    assert!(
        dual_msm.check(&verification_context.verifier_params),
        "same-epoch step proof should verify against the correct public inputs"
    );
}
