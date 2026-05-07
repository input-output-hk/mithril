//! Positive golden tests for the recursive Halo2 IVC flow.
//!
//! This suite intentionally mixes two styles:
//! - `MockProver` checks for circuit satisfiability when we want coverage of a
//!   transition without paying for a fresh final recursive proof.
//! - stored assets check proof-bearing checkpoints and replayed transitions
//!   against committed artifacts.

use midnight_circuits::types::Instantiable;

use crate::circuits::halo2_ivc::tests::common::{
    asset_readers::{
        load_embedded_recursive_chain_state_asset, load_embedded_recursive_step_output_asset,
        load_embedded_verification_context_asset,
    },
    generators::{
        GENESIS_EPOCH, build_asset_generation_setup, build_genesis_base_case_next_state,
        build_genesis_base_case_witness, build_next_certificate_asset_data,
        build_same_epoch_certificate_asset_data, next_message_and_preimage_for_step,
        next_state_for_step,
    },
    helpers::{
        assert_recursive_mock_prover_accepts_with_label, build_recursive_mock_prover_setup,
        compute_exact_next_accumulator_from_assets, compute_expected_next_accumulator,
        verify_prepare_blake2b_recursive_proof, verify_prepare_poseidon_recursive_proof,
    },
};
use crate::circuits::halo2_ivc::{AssignedAccumulator, circuit::IvcCircuit, state::State};

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

    let dual_msm = verify_prepare_poseidon_recursive_proof(
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

    let dual_msm = verify_prepare_blake2b_recursive_proof(
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

mod slow {
    use super::*;

    #[test]
    fn all_positive_circuit_scenarios_are_accepted() {
        // One setup call shared across genesis, same-epoch, and next-epoch MockProver
        // checks, plus the stored asset replay assertion. The label in each MockProver
        // assertion identifies which scenario caused a spurious failure.
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_recursive_mock_prover_setup(&setup);
        let recursive_chain_state = load_embedded_recursive_chain_state_asset()
            .expect("recursive chain state asset should load");

        // Genesis base case: no previous proof, trivial accumulator, gated contributions.
        let genesis_circuit = IvcCircuit::new(
            mock_prover_setup.global.clone(),
            State::genesis(),
            build_genesis_base_case_witness(&setup),
            vec![],
            vec![],
            mock_prover_setup.trivial_accumulator.clone(),
            mock_prover_setup.certificate_verifying_key.vk(),
            &mock_prover_setup.recursive_verifying_key,
        );
        let genesis_public_inputs = [
            mock_prover_setup.global.as_public_input(),
            build_genesis_base_case_next_state(&setup, GENESIS_EPOCH).as_public_input(),
            AssignedAccumulator::as_public_input(&mock_prover_setup.trivial_accumulator),
        ]
        .concat();
        assert_recursive_mock_prover_accepts_with_label(
            genesis_circuit,
            genesis_public_inputs,
            "genesis base case",
        );

        // Same-epoch recursive step: fresh certificate data, same merkle root and epoch.
        let (se_cert_proof, se_cert_acc, se_next_state, se_witness) =
            build_same_epoch_certificate_asset_data(
                &setup,
                &mock_prover_setup.certificate_commitment_parameters,
                &setup.certificate_relation,
                &mock_prover_setup.certificate_verifying_key,
                &recursive_chain_state.state,
                &mut rand_core::OsRng,
            );
        let se_next_accumulator = compute_expected_next_accumulator(
            &mock_prover_setup,
            &recursive_chain_state,
            se_cert_acc,
        );
        let se_circuit = IvcCircuit::new(
            mock_prover_setup.global.clone(),
            recursive_chain_state.state.clone(),
            se_witness,
            se_cert_proof,
            recursive_chain_state.proof.clone(),
            recursive_chain_state.accumulator.clone(),
            mock_prover_setup.certificate_verifying_key.vk(),
            &mock_prover_setup.recursive_verifying_key,
        );
        let se_public_inputs = [
            mock_prover_setup.global.as_public_input(),
            se_next_state.as_public_input(),
            AssignedAccumulator::as_public_input(&se_next_accumulator),
        ]
        .concat();
        assert_recursive_mock_prover_accepts_with_label(
            se_circuit,
            se_public_inputs,
            "same-epoch recursive step",
        );

        // Next-epoch recursive step: fresh certificate data, advancing epoch.
        let (ne_cert_proof, ne_cert_acc, ne_next_state, ne_witness) =
            build_next_certificate_asset_data(
                &setup,
                &mock_prover_setup.certificate_commitment_parameters,
                &setup.certificate_relation,
                &mock_prover_setup.certificate_verifying_key,
                &recursive_chain_state.state,
                &mut rand_core::OsRng,
            );
        let ne_next_accumulator = compute_expected_next_accumulator(
            &mock_prover_setup,
            &recursive_chain_state,
            ne_cert_acc,
        );
        let ne_circuit = IvcCircuit::new(
            mock_prover_setup.global.clone(),
            recursive_chain_state.state.clone(),
            ne_witness,
            ne_cert_proof,
            recursive_chain_state.proof.clone(),
            recursive_chain_state.accumulator.clone(),
            mock_prover_setup.certificate_verifying_key.vk(),
            &mock_prover_setup.recursive_verifying_key,
        );
        let ne_public_inputs = [
            mock_prover_setup.global.as_public_input(),
            ne_next_state.as_public_input(),
            AssignedAccumulator::as_public_input(&ne_next_accumulator),
        ]
        .concat();
        assert_recursive_mock_prover_accepts_with_label(
            ne_circuit,
            ne_public_inputs,
            "next-epoch recursive step",
        );

        // Replay integrity: stored next-step artifact must match off-circuit replay.
        let recursive_step_output = load_embedded_recursive_step_output_asset()
            .expect("recursive step output asset should load");
        let (expected_message, _) =
            next_message_and_preimage_for_step(&setup, &recursive_chain_state.state);
        let expected_next_state =
            next_state_for_step(&recursive_chain_state.state, expected_message);
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
}
