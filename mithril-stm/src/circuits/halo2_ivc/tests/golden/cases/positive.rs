use midnight_circuits::types::Instantiable;

use crate::circuits::halo2_ivc::tests::{
    asset_readers::{
        load_recursive_chain_state_asset, load_recursive_step_output_asset,
        load_verification_context_asset, recursive_chain_state_asset_path,
        recursive_step_output_asset_path, verification_context_asset_path,
    },
    generators::{
        build_asset_generation_setup, build_genesis_base_case_next_state,
        build_genesis_base_case_witness,
    },
    golden::helpers::{
        assert_recursive_mock_prover_accepts, build_recursive_mock_prover_setup,
        verify_and_prepare_blake2b_recursive_proof, verify_and_prepare_poseidon_recursive_proof,
    },
};
use crate::circuits::halo2_ivc::{AssignedAccumulator, circuit::IvcCircuit, state::State};

#[test]
fn accepts_genesis_base_case_with_mock_prover() {
    let setup = build_asset_generation_setup();
    let mock_prover_setup = build_recursive_mock_prover_setup(&setup);

    let circuit = IvcCircuit::new(
        mock_prover_setup.global.clone(),
        State::genesis(),
        build_genesis_base_case_witness(&setup),
        vec![],
        vec![],
        mock_prover_setup.trivial_accumulator.clone(),
        mock_prover_setup.certificate_verifying_key.vk(),
        &mock_prover_setup.recursive_verifying_key,
    );

    let public_inputs = [
        mock_prover_setup.global.as_public_input(),
        build_genesis_base_case_next_state(&setup, 5u64).as_public_input(),
        AssignedAccumulator::as_public_input(&mock_prover_setup.trivial_accumulator),
    ]
    .concat();

    assert_recursive_mock_prover_accepts(circuit, public_inputs);
}

#[test]
fn verifies_stored_recursive_chain_state() {
    let verification_context = load_verification_context_asset(&verification_context_asset_path())
        .expect("verification context asset should load");
    let recursive_chain_state =
        load_recursive_chain_state_asset(&recursive_chain_state_asset_path())
            .expect("recursive chain state asset should load");

    assert_eq!(
        verification_context.global_field_elements, recursive_chain_state.global_field_elements,
        "verification context and recursive chain state should use the same global public inputs"
    );

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        recursive_chain_state.state.as_public_input(),
        AssignedAccumulator::as_public_input(&recursive_chain_state.accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_poseidon_recursive_proof(
        &verification_context.recursive_verifying_key,
        &recursive_chain_state.proof,
        &public_inputs,
    );

    assert!(
        dual_msm.clone().check(&verification_context.verifier_params),
        "stored recursive chain state proof should verify"
    );
}

#[test]
fn verifies_stored_recursive_step_output() {
    let verification_context = load_verification_context_asset(&verification_context_asset_path())
        .expect("verification context asset should load");
    let recursive_step_output =
        load_recursive_step_output_asset(&recursive_step_output_asset_path())
            .expect("recursive step output asset should load");

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        recursive_step_output.next_state.as_public_input(),
        AssignedAccumulator::as_public_input(&recursive_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &recursive_step_output.proof,
        &public_inputs,
    );

    assert!(
        dual_msm.clone().check(&verification_context.verifier_params),
        "stored recursive step output proof should verify"
    );
    assert!(
        recursive_step_output.next_accumulator.check(
            &verification_context.verifier_tau_in_g2,
            &verification_context.combined_fixed_bases,
        ),
        "stored recursive step output accumulator should verify"
    );
}
