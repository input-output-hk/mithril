use midnight_circuits::types::Instantiable;

use crate::circuits::halo2_ivc::tests::golden::{
    asset_readers::{
        load_recursive_chain_state_asset, load_recursive_step_output_asset,
        load_verification_context_asset, recursive_chain_state_asset_path,
        recursive_step_output_asset_path, verification_context_asset_path,
    },
    generators::{
        build_asset_generation_setup, build_genesis_base_case_next_state,
        build_genesis_base_case_witness, build_next_certificate_asset_data,
        next_message_and_preimage_for_step, next_state_for_step,
    },
    helpers::{
        assert_recursive_mock_prover_accepts, build_recursive_mock_prover_setup,
        compute_exact_next_accumulator_from_assets, compute_expected_next_accumulator,
        verify_and_prepare_blake2b_recursive_proof, verify_and_prepare_poseidon_recursive_proof,
    },
};
use crate::circuits::halo2_ivc::{AssignedAccumulator, circuit::IvcCircuit, state::State};

#[test]
fn genesis_base_case_mock_prover() {
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
fn normal_recursive_step_mock_prover() {
    let setup = build_asset_generation_setup();
    let mock_prover_setup = build_recursive_mock_prover_setup(&setup);
    let recursive_chain_state =
        load_recursive_chain_state_asset(&recursive_chain_state_asset_path())
            .expect("recursive chain state asset should load");

    // Reuse the stored previous recursive artifacts, but build the fresh
    // certificate-side data for the next step inside the test.
    let (certificate_proof, certificate_accumulator, next_state, recursive_witness) =
        build_next_certificate_asset_data(
            &setup,
            &mock_prover_setup.certificate_commitment_parameters,
            &setup.certificate_relation,
            &mock_prover_setup.certificate_verifying_key,
            &recursive_chain_state.state,
            &mut rand_core::OsRng,
        );
    let next_accumulator = compute_expected_next_accumulator(
        &mock_prover_setup,
        &recursive_chain_state,
        certificate_accumulator,
    );

    let circuit = IvcCircuit::new(
        mock_prover_setup.global.clone(),
        recursive_chain_state.state.clone(),
        recursive_witness,
        certificate_proof,
        recursive_chain_state.proof.clone(),
        recursive_chain_state.accumulator.clone(),
        mock_prover_setup.certificate_verifying_key.vk(),
        &mock_prover_setup.recursive_verifying_key,
    );

    let public_inputs = [
        mock_prover_setup.global.as_public_input(),
        next_state.as_public_input(),
        AssignedAccumulator::as_public_input(&next_accumulator),
    ]
    .concat();

    assert_recursive_mock_prover_accepts(circuit, public_inputs);
}

#[test]
fn recursive_chain_state_asset_valid() {
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
fn recursive_step_output_asset_valid() {
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

#[test]
fn recursive_step_output_chain_flow_asset_valid() {
    let setup = build_asset_generation_setup();
    let mock_prover_setup = build_recursive_mock_prover_setup(&setup);
    let recursive_chain_state =
        load_recursive_chain_state_asset(&recursive_chain_state_asset_path())
            .expect("recursive chain state asset should load");
    let recursive_step_output =
        load_recursive_step_output_asset(&recursive_step_output_asset_path())
            .expect("recursive step output asset should load");
    let (expected_message, _) =
        next_message_and_preimage_for_step(&setup, &recursive_chain_state.state);
    let expected_next_state = next_state_for_step(&recursive_chain_state.state, expected_message);

    let expected_next_accumulator = compute_exact_next_accumulator_from_assets(
        &mock_prover_setup,
        &recursive_chain_state,
        &expected_next_state,
        &recursive_step_output.certificate_proof,
    );

    assert_eq!(
        expected_next_state.as_public_input(),
        recursive_step_output.next_state.as_public_input(),
        "stored recursive step output should match the expected chained-flow next state"
    );
    assert_eq!(
        AssignedAccumulator::as_public_input(&expected_next_accumulator),
        AssignedAccumulator::as_public_input(&recursive_step_output.next_accumulator),
        "replayed next accumulator should match the stored recursive step output"
    );
}
