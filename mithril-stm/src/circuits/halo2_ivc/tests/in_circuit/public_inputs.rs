//! Negative public-input tests: tampered global fields and accumulator (fast CI)
//! and MockProver constraint checks (in `mod slow`).

use ff::Field;
use midnight_circuits::types::Instantiable;

use crate::circuits::halo2_ivc::{
    AssignedAccumulator, F,
    state::State,
    tests::common::{
        asset_readers::{
            load_embedded_genesis_step_output_asset, load_embedded_verification_context_asset,
        },
        generators::{
            GENESIS_EPOCH, build_asset_generation_setup, build_genesis_base_case_next_state,
            build_genesis_base_case_witness,
        },
        helpers::{
            assert_recursive_mock_prover_rejects_with_label, build_mock_prover_setup_from_assets,
            build_trivial_mock_prover_circuit, verify_prepare_blake2b_recursive_proof,
        },
    },
};

/// Loads the genesis assets, applies `tamper` to the three public-input sections
/// (`global`, `state`, `accumulator`), then asserts the verifier rejects the result.
///
/// `global` layout: `[genesis_msg, genesis_verification_key.x, genesis_verification_key.y,
/// certificate_verifying_key_repr, ivc_verifying_key_repr]`.
fn assert_genesis_step_output_rejects_tampered_public_inputs(
    tamper: impl FnOnce(&mut Vec<F>, &mut Vec<F>, &mut Vec<F>),
    rejection_message: &str,
) {
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let genesis_step_output =
        load_embedded_genesis_step_output_asset().expect("genesis step output asset should load");

    let mut global = verification_context.global_field_elements.clone();
    let mut state = genesis_step_output.next_state.as_public_input();
    let mut accumulator_encoding =
        AssignedAccumulator::as_public_input(&genesis_step_output.next_accumulator);

    tamper(&mut global, &mut state, &mut accumulator_encoding);

    let public_inputs = [global, state, accumulator_encoding].concat();

    let dual_msm = verify_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &genesis_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "{rejection_message}"
    );
}

#[test]
fn genesis_msg_tampered_public_input_is_rejected() {
    // Asset-based check that the verifier rejects a stored proof when genesis_msg
    // is replaced in the global public inputs, confirming it is committed at index 0.
    assert_genesis_step_output_rejects_tampered_public_inputs(
        |global, _, _| global[0] = F::ONE,
        "proof with tampered genesis_msg should be rejected by the verifier",
    );
}

#[test]
fn genesis_verification_key_x_tampered_public_input_is_rejected() {
    // Asset-based check that the verifier rejects a stored proof when the x coordinate of
    // genesis_verification_key is replaced in the global public inputs, confirming index 1 is committed.
    assert_genesis_step_output_rejects_tampered_public_inputs(
        |global, _, _| global[1] = F::ONE,
        "proof with tampered genesis_verification_key x coordinate should be rejected by the verifier",
    );
}

#[test]
fn genesis_verification_key_y_tampered_public_input_is_rejected() {
    // Asset-based check that the verifier rejects a stored proof when the y coordinate of
    // genesis_verification_key is replaced in the global public inputs, confirming index 2 is committed.
    assert_genesis_step_output_rejects_tampered_public_inputs(
        |global, _, _| global[2] = F::ONE,
        "proof with tampered genesis_verification_key y coordinate should be rejected by the verifier",
    );
}

#[test]
fn certificate_verifying_key_repr_tampered_public_input_is_rejected() {
    // Asset-based check that the verifier rejects a stored proof when
    // certificate_verifying_key_repr is replaced in the global public inputs,
    // confirming it is committed at index 3.
    assert_genesis_step_output_rejects_tampered_public_inputs(
        |global, _, _| global[3] = F::ONE,
        "proof with tampered certificate_verifying_key_repr should be rejected by the verifier",
    );
}

#[test]
fn ivc_verifying_key_repr_tampered_public_input_is_rejected() {
    // Asset-based check that the verifier rejects a stored proof when
    // ivc_verifying_key_repr is replaced in the global public inputs,
    // confirming it is committed at index 4.
    assert_genesis_step_output_rejects_tampered_public_inputs(
        |global, _, _| global[4] = F::ONE,
        "proof with tampered ivc_verifying_key_repr should be rejected by the verifier",
    );
}

#[test]
fn next_accumulator_tampered_public_input_is_rejected() {
    // Asset-based check that the verifier rejects a stored proof when next_accumulator
    // is replaced in the public inputs, confirming the accumulator output is committed.
    assert_genesis_step_output_rejects_tampered_public_inputs(
        |_, _, accumulator_encoding| accumulator_encoding[0] = F::ONE,
        "proof with tampered next_accumulator should be rejected by the verifier",
    );
}

mod slow {
    use super::*;

    #[test]
    fn circuit_rejects_wrong_genesis_msg_global_field() {
        // MockProver constraint check: global[0] (genesis_msg) set to ONE must violate
        // the in-circuit constraint that pins the genesis message to the global public input.
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_mock_prover_setup_from_assets(&setup);
        let next_state = build_genesis_base_case_next_state(&setup, GENESIS_EPOCH);
        let witness = build_genesis_base_case_witness(&setup);
        let circuit =
            build_trivial_mock_prover_circuit(&mock_prover_setup, State::genesis(), witness);
        let mut global = mock_prover_setup.global.as_public_input();
        let state = next_state.as_public_input();
        let accumulator_encoding =
            AssignedAccumulator::as_public_input(&mock_prover_setup.trivial_accumulator);
        global[0] = F::ONE;
        assert_recursive_mock_prover_rejects_with_label(
            circuit,
            [global, state, accumulator_encoding].concat(),
            "global[0] (genesis_msg) set to ONE",
        );
    }

    #[test]
    fn circuit_rejects_wrong_certificate_vk_repr_global_field() {
        // MockProver constraint check: global[3] (certificate_verifying_key_repr) set to ONE
        // must violate the in-circuit constraint that pins the certificate VK hash.
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_mock_prover_setup_from_assets(&setup);
        let next_state = build_genesis_base_case_next_state(&setup, GENESIS_EPOCH);
        let witness = build_genesis_base_case_witness(&setup);
        let circuit =
            build_trivial_mock_prover_circuit(&mock_prover_setup, State::genesis(), witness);
        let mut global = mock_prover_setup.global.as_public_input();
        let state = next_state.as_public_input();
        let accumulator_encoding =
            AssignedAccumulator::as_public_input(&mock_prover_setup.trivial_accumulator);
        global[3] = F::ONE;
        assert_recursive_mock_prover_rejects_with_label(
            circuit,
            [global, state, accumulator_encoding].concat(),
            "global[3] (certificate_verifying_key_repr) set to ONE",
        );
    }

    #[test]
    fn circuit_rejects_wrong_ivc_vk_repr_global_field() {
        // MockProver constraint check: global[4] (ivc_verifying_key_repr) set to ONE
        // must violate the in-circuit constraint that pins the IVC VK hash.
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_mock_prover_setup_from_assets(&setup);
        let next_state = build_genesis_base_case_next_state(&setup, GENESIS_EPOCH);
        let witness = build_genesis_base_case_witness(&setup);
        let circuit =
            build_trivial_mock_prover_circuit(&mock_prover_setup, State::genesis(), witness);
        let mut global = mock_prover_setup.global.as_public_input();
        let state = next_state.as_public_input();
        let accumulator_encoding =
            AssignedAccumulator::as_public_input(&mock_prover_setup.trivial_accumulator);
        global[4] = F::ONE;
        assert_recursive_mock_prover_rejects_with_label(
            circuit,
            [global, state, accumulator_encoding].concat(),
            "global[4] (ivc_verifying_key_repr) set to ONE",
        );
    }
}
