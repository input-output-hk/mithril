//! Tests that the circuit correctly enforces previous IVC proof validity in non-genesis steps.
//!
//! Fast tests confirm that the verifier accepts stored proofs with a valid
//! previous IVC proof for both same-epoch and next-epoch transitions. Slow tests
//! use the real prover to confirm that a tampered previous IVC proof is rejected
//! at the KZG opening check.

use midnight_circuits::types::Instantiable;

use crate::StmResult;
use crate::circuits::halo2_ivc::{
    Accumulator, AssignedAccumulator, S,
    circuit::IvcCircuit,
    state::{State, Witness},
    tests::common::{
        asset_readers::{
            RecursiveChainStateAsset, RecursiveStepOutputAsset,
            load_embedded_recursive_chain_state_asset, load_embedded_recursive_step_output_asset,
            load_embedded_same_epoch_step_output_asset, load_embedded_verification_context_asset,
        },
        generators::{
            AssetGenerationSetup, build_asset_generation_setup, build_next_certificate_asset_data,
            build_recursive_proving_key_from_vks, build_same_epoch_certificate_asset_data,
            prove_blake2b_ivc,
        },
        helpers::{
            RecursiveTestSetup, build_recursive_test_setup, compute_expected_next_accumulator,
            verify_prepare_blake2b_recursive_proof,
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

mod slow_real_prover {
    use super::*;

    /// Builds a fresh non-genesis circuit with `build_cert_data`, flips one byte
    /// in the stored previous IVC proof (`recursive_chain_state.proof`), then
    /// asserts the verifier rejects the resulting proof.
    ///
    /// `build_cert_data` receives the deterministic setup, the shared recursive
    /// setup, and the stored chain-state asset, and returns the certificate proof,
    /// accumulator, next state, and IVC witness for that step, allowing each test
    /// to choose the transition variant it covers.
    ///
    /// Public inputs are built from the **correct** accumulators (certificate and
    /// previous IVC) so the circuit's in-circuit accumulator (derived from the
    /// tampered previous IVC proof bytes) diverges from the committed value. The
    /// halo2 prover does not check constraint satisfaction, so it succeeds; the
    /// KZG opening check in the verifier then returns false.
    fn assert_tampered_previous_ivc_proof_is_rejected(
        build_cert_data: impl FnOnce(
            &AssetGenerationSetup,
            &RecursiveTestSetup,
            &RecursiveChainStateAsset,
        ) -> (Vec<u8>, Accumulator<S>, State, Witness),
        rejection_message: &str,
    ) {
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_recursive_test_setup(&setup);
        let proving_key = build_recursive_proving_key_from_vks(
            &mock_prover_setup.certificate_verifying_key,
            mock_prover_setup.recursive_verifying_key.clone(),
        );

        let recursive_chain_state = load_embedded_recursive_chain_state_asset()
            .expect("recursive chain state asset should load");

        let (cert_proof, cert_accumulator, next_state, ivc_witness) =
            build_cert_data(&setup, &mock_prover_setup, &recursive_chain_state);

        let mut self_proof = recursive_chain_state.proof.clone();
        self_proof[0] ^= 0xFF;

        let next_accumulator = compute_expected_next_accumulator(
            &mock_prover_setup,
            &recursive_chain_state,
            cert_accumulator,
        );

        let circuit = IvcCircuit::new(
            mock_prover_setup.global.clone(),
            recursive_chain_state.state.clone(),
            ivc_witness,
            cert_proof,
            self_proof,
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

        let proof = prove_blake2b_ivc(
            &mock_prover_setup.recursive_commitment_parameters,
            &proving_key,
            &circuit,
            &public_inputs,
            &mut rand_core::OsRng,
        );

        let dual_msm = verify_prepare_blake2b_recursive_proof(
            &mock_prover_setup.recursive_verifying_key,
            &proof,
            &public_inputs,
        );

        assert!(
            !dual_msm.check(&mock_prover_setup.universal_verifier_params),
            "{rejection_message}"
        );
    }

    #[test]
    fn same_epoch_step_rejects_tampered_previous_ivc_proof() {
        // Real prover: confirms that the circuit enforces previous IVC proof validity
        // in a same-epoch step; a single flipped byte in the previous IVC proof
        // bytes causes the KZG opening check in the verifier to fail.
        assert_tampered_previous_ivc_proof_is_rejected(
            |setup, mock, chain_state| {
                build_same_epoch_certificate_asset_data(
                    setup,
                    &mock.certificate_commitment_parameters,
                    &setup.certificate_relation,
                    &mock.certificate_verifying_key,
                    &chain_state.state,
                    &mut rand_core::OsRng,
                )
            },
            "same-epoch step with a tampered previous IVC proof should be rejected by the verifier",
        );
    }

    #[test]
    fn next_epoch_step_rejects_tampered_previous_ivc_proof() {
        // Real prover: confirms that the circuit enforces previous IVC proof validity
        // in a next-epoch step; a single flipped byte in the previous IVC proof
        // bytes causes the KZG opening check in the verifier to fail.
        assert_tampered_previous_ivc_proof_is_rejected(
            |setup, mock, chain_state| {
                build_next_certificate_asset_data(
                    setup,
                    &mock.certificate_commitment_parameters,
                    &setup.certificate_relation,
                    &mock.certificate_verifying_key,
                    &chain_state.state,
                    &mut rand_core::OsRng,
                )
            },
            "next-epoch step with a tampered previous IVC proof should be rejected by the verifier",
        );
    }
}
