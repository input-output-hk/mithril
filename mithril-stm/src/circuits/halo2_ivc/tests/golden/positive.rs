//! Positive golden tests for the recursive Halo2 IVC flow.
//!
//! Fast tests verify stored proof artifacts against the full verifier. The slow
//! `MockProver` check covers the genesis base case in-circuit (the unique code path
//! that has no stored proof to verify against). Same-epoch and next-epoch positive
//! constraint coverage is provided by the stored-asset verification tests above,
//! which use the full prover output: a valid proof implies all constraints hold.

use midnight_circuits::types::Instantiable;

use crate::circuits::halo2_ivc::tests::common::{
    asset_readers::{
        load_embedded_recursive_chain_state_asset, load_embedded_recursive_step_output_asset,
        load_embedded_verification_context_asset,
    },
    generators::{
        GENESIS_EPOCH, build_asset_generation_setup, build_genesis_base_case_next_state,
        build_genesis_base_case_witness, next_message_and_preimage_for_step, next_state_for_step,
    },
    helpers::{
        assert_recursive_mock_prover_accepts_with_label, build_mock_prover_public_inputs,
        build_mock_prover_setup_from_assets, build_recursive_mock_prover_setup,
        build_trivial_mock_prover_circuit, compute_exact_next_accumulator_from_assets,
        verify_prepare_blake2b_recursive_proof, verify_prepare_poseidon_recursive_proof,
    },
};
use crate::circuits::halo2_ivc::{AssignedAccumulator, state::State};

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
        recursive_chain_state.proof.as_bytes(),
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
    // 1 (verifier). Load the verification context and the stored recursive
    // step output.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let recursive_step_output = load_embedded_recursive_step_output_asset()
        .expect("recursive step output asset should load");

    // 2 (verifier). Rebuild the public inputs committed by the stored proof:
    // global values, next state, and next accumulator.
    let public_inputs = [
        verification_context.global_field_elements.clone(),
        recursive_step_output.next_state.as_public_input(),
        AssignedAccumulator::as_public_input(&recursive_step_output.next_accumulator),
    ]
    .concat();

    // 3 (verifier). Verify/prepare the stored recursive proof with the
    // recursive verifying key.
    let dual_msm = verify_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        recursive_step_output.proof.as_bytes(),
        &public_inputs,
    );

    // 4 (verifier). Check that the proof verification result is valid.
    assert!(
        dual_msm.clone().check(&verification_context.verifier_params),
        "stored recursive step output proof should verify"
    );
    // 5 (verifier). Check the folded accumulator stored with the same step
    // output.
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
    fn genesis_base_case_circuit_is_accepted() {
        // MockProver constraint check for the genesis base case: no previous proof,
        // trivial accumulator, all accumulator contributions gated to the group identity.
        // Same-epoch and next-epoch positive constraint coverage is provided by
        // `recursive_chain_state_asset_proof_and_accumulator_are_valid` and
        // `recursive_step_output_asset_proof_and_accumulator_are_valid` above — a valid
        // full proof implies all constraints held when the proof was generated.
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_mock_prover_setup_from_assets(&setup);
        let next_state = build_genesis_base_case_next_state(&setup, GENESIS_EPOCH);
        let circuit = build_trivial_mock_prover_circuit(
            &mock_prover_setup,
            State::genesis(),
            build_genesis_base_case_witness(&setup),
        );
        let public_inputs = build_mock_prover_public_inputs(&mock_prover_setup, &next_state);
        assert_recursive_mock_prover_accepts_with_label(
            circuit,
            public_inputs,
            "genesis base case",
        );
    }

    #[test]
    fn replay_integrity_matches_stored_next_step_output() {
        // Off-circuit replay of the stored next-epoch transition: verifies that the
        // committed recursive_step_output asset matches an independent recomputation
        // of the next state and folded accumulator, confirming the generator is
        // deterministic and self-consistent.
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_recursive_mock_prover_setup(&setup);
        let recursive_chain_state = load_embedded_recursive_chain_state_asset()
            .expect("recursive chain state asset should load");
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
            recursive_step_output.certificate_proof.as_bytes(),
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
