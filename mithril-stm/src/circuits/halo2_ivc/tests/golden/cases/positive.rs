//! Positive golden tests for the recursive Halo2 IVC flow.
//!
//! This suite intentionally mixes two styles:
//! - `MockProver` checks for circuit satisfiability when we want coverage of a
//!   transition without paying for a fresh final recursive proof.
//! - stored assets check proof-bearing checkpoints and replayed transitions
//!   against committed artifacts.

use midnight_circuits::types::Instantiable;

use crate::circuits::halo2_ivc::tests::golden::{
    asset_readers::{
        load_embedded_recursive_chain_state_asset, load_embedded_recursive_step_output_asset,
        load_embedded_verification_context_asset,
    },
    generators::{
        build_asset_generation_setup, build_genesis_base_case_next_state,
        build_genesis_base_case_witness, build_next_certificate_asset_data,
        build_same_epoch_certificate_asset_data, next_message_and_preimage_for_step,
        next_state_for_step,
    },
    helpers::{
        assert_recursive_mock_prover_accepts, build_recursive_mock_prover_setup,
        compute_exact_next_accumulator_from_assets, compute_expected_next_accumulator,
        verify_and_prepare_blake2b_recursive_proof, verify_and_prepare_poseidon_recursive_proof,
    },
};
use crate::circuits::halo2_ivc::{AssignedAccumulator, circuit::IvcCircuit, state::State};

// TODO: Move this slow golden test into a dedicated slow/extended CI mode once
// the recursive test suite is split into fast and slow lanes.
#[test]
#[ignore]
fn slow_genesis_base_case_accepts_valid_public_inputs() {
    // MockProver check for the explicit genesis/base-case branch where no
    // previous recursive proof exists yet and the circuit must accept the
    // first valid transition.
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

// TODO: Move this slow golden test into a dedicated slow/extended CI mode once
// the recursive test suite is split into fast and slow lanes.
#[test]
#[ignore]
fn slow_recursive_step_next_epoch_accepts_valid_public_inputs() {
    // MockProver check for one non-genesis next-epoch recursive step using
    // stored previous recursive artifacts plus fresh certificate-side data
    // generated in-test.
    let setup = build_asset_generation_setup();
    let mock_prover_setup = build_recursive_mock_prover_setup(&setup);
    let recursive_chain_state = load_embedded_recursive_chain_state_asset()
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

// TODO: Move this slow golden test into a dedicated slow/extended CI mode once
// the recursive test suite is split into fast and slow lanes.
#[test]
#[ignore]
fn slow_recursive_step_same_epoch_accepts_valid_public_inputs() {
    // MockProver check for one non-genesis same-epoch recursive step using
    // stored previous recursive artifacts plus fresh certificate-side data
    // generated in-test.
    let setup = build_asset_generation_setup();
    let mock_prover_setup = build_recursive_mock_prover_setup(&setup);
    let recursive_chain_state = load_embedded_recursive_chain_state_asset()
        .expect("recursive chain state asset should load");

    let (certificate_proof, certificate_accumulator, next_state, recursive_witness) =
        build_same_epoch_certificate_asset_data(
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
fn recursive_chain_state_asset_proof_and_accumulator_are_valid() {
    // Asset-based check that the stored previous recursive checkpoint remains a
    // coherent proof/state/accumulator bundle on its own.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let recursive_chain_state = load_embedded_recursive_chain_state_asset()
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
fn recursive_step_output_asset_proof_and_accumulator_are_valid() {
    // Asset-based check that the stored next recursive checkpoint remains
    // valid in isolation, including both the final proof and the folded
    // accumulator.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let recursive_step_output = load_embedded_recursive_step_output_asset()
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

// TODO: Move this slow golden test into a dedicated slow/extended CI mode once
// the recursive test suite is split into fast and slow lanes.
#[test]
#[ignore]
fn slow_recursive_step_output_asset_matches_replayed_chain_flow() {
    // Asset-based replay check that recomputes the expected next step from the
    // stored previous checkpoint and verifies that the stored next-step
    // artifact is truly its continuation.
    let setup = build_asset_generation_setup();
    let mock_prover_setup = build_recursive_mock_prover_setup(&setup);
    let recursive_chain_state = load_embedded_recursive_chain_state_asset()
        .expect("recursive chain state asset should load");
    let recursive_step_output = load_embedded_recursive_step_output_asset()
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
