//! Tests that the circuit correctly enforces certificate proof validity in non-genesis steps.
//!
//! Fast tests confirm that the verifier accepts stored proofs with a valid certificate
//! proof for both same-epoch and next-epoch transitions. Slow tests confirm the
//! realistic adversary scenario: tampered cert proof bytes, with `next_accumulator`
//! computed consistently from those tampered bytes, produce an accumulator that fails
//! the off-circuit pairing check — confirming the KZG system catches the forgery.

use midnight_circuits::types::Instantiable;

use crate::StmResult;
use crate::circuits::halo2_ivc::{
    Accumulator, AssignedAccumulator, S,
    state::{State, Witness},
    tests::common::{
        asset_readers::{
            RecursiveStepOutputAsset, load_embedded_recursive_chain_state_asset,
            load_embedded_recursive_step_output_asset, load_embedded_same_epoch_step_output_asset,
            load_embedded_verification_context_asset,
        },
        generators::{
            AssetGenerationSetup, build_asset_generation_setup, build_next_certificate_asset_data,
            build_same_epoch_certificate_asset_data, certificate_public_inputs_for_step,
        },
        helpers::{
            RecursiveMockProverSetup, build_recursive_mock_prover_setup,
            prepare_previous_recursive_proof_accumulator,
            try_verify_prepare_poseidon_recursive_proof, verify_prepare_blake2b_recursive_proof,
        },
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

mod slow {
    use super::*;

    /// Generates a fresh certificate proof using `build_cert_data`, flips one byte,
    /// then computes `next_accumulator` **consistently from the tampered bytes** and
    /// asserts the off-circuit pairing check fails.
    ///
    /// This is the realistic adversary scenario: an adversary who supplies tampered
    /// cert proof bytes would also derive `next_accumulator` from those tampered bytes.
    /// The KZG accumulator check catches the forgery — no in-circuit prover run needed.
    fn assert_tampered_cert_proof_produces_invalid_accumulator(
        build_cert_data: impl FnOnce(
            &AssetGenerationSetup,
            &RecursiveMockProverSetup,
        ) -> (Vec<u8>, Accumulator<S>, State, Witness),
    ) {
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_recursive_mock_prover_setup(&setup);
        let recursive_chain_state = load_embedded_recursive_chain_state_asset()
            .expect("recursive chain state asset should load");

        let (mut cert_proof, _, next_state, _) = build_cert_data(&setup, &mock_prover_setup);
        cert_proof[0] ^= 0xFF;

        let cert_public_inputs =
            certificate_public_inputs_for_step(&recursive_chain_state.state, &next_state);

        // Try to parse the tampered cert proof off-circuit. Returns None if the bytes
        // are too malformed to deserialize (forgery caught immediately). Returns
        // Some(dual_msm) if they parse but carry invalid KZG opening terms.
        let tampered_cert_acc = match try_verify_prepare_poseidon_recursive_proof(
            mock_prover_setup.certificate_verifying_key.vk(),
            &cert_proof,
            &cert_public_inputs,
        ) {
            None => return, // bytes rejected at deserialization — forgery caught
            Some(dual_msm) => {
                let mut acc: Accumulator<S> = dual_msm.into();
                acc.extract_fixed_bases(&mock_prover_setup.certificate_fixed_bases);
                acc.collapse();
                acc
            }
        };

        let prev_ivc_acc = prepare_previous_recursive_proof_accumulator(
            &mock_prover_setup,
            &recursive_chain_state,
        );

        let mut tampered_next_acc = Accumulator::accumulate(&[
            recursive_chain_state.accumulator.clone(),
            tampered_cert_acc,
            prev_ivc_acc,
        ]);
        tampered_next_acc.collapse();

        assert!(
            !tampered_next_acc.check(
                &mock_prover_setup.verifier_tau_in_g2,
                &mock_prover_setup.combined_fixed_bases,
            ),
            "next_accumulator derived consistently from tampered cert proof bytes should fail the pairing check"
        );
    }

    #[test]
    fn same_epoch_step_tampered_cert_proof_produces_invalid_accumulator() {
        // Off-circuit check: tampered same-epoch cert proof bytes, with next_accumulator
        // derived consistently from those bytes, cause the KZG pairing check to fail.
        assert_tampered_cert_proof_produces_invalid_accumulator(|setup, mock| {
            build_same_epoch_certificate_asset_data(
                setup,
                &mock.certificate_commitment_parameters,
                &setup.certificate_relation,
                &mock.certificate_verifying_key,
                &load_embedded_recursive_chain_state_asset()
                    .expect("recursive chain state asset should load")
                    .state,
                &mut rand_core::OsRng,
            )
        });
    }

    #[test]
    fn next_epoch_step_tampered_cert_proof_produces_invalid_accumulator() {
        // Off-circuit check: tampered next-epoch cert proof bytes, with next_accumulator
        // derived consistently from those bytes, cause the KZG pairing check to fail.
        assert_tampered_cert_proof_produces_invalid_accumulator(|setup, mock| {
            build_next_certificate_asset_data(
                setup,
                &mock.certificate_commitment_parameters,
                &setup.certificate_relation,
                &mock.certificate_verifying_key,
                &load_embedded_recursive_chain_state_asset()
                    .expect("recursive chain state asset should load")
                    .state,
                &mut rand_core::OsRng,
            )
        });
    }
}
