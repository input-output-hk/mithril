//! Negative encoding tests: tampered public inputs (fast CI) and MockProver
//! constraint checks (in `mod slow`).

use ff::Field;
use midnight_circuits::types::Instantiable;

use crate::circuits::halo2_ivc::{
    AssignedAccumulator, F, PREIMAGE_CURRENT_EPOCH_BYTES, PREIMAGE_NEXT_MERKLE_ROOT_BYTES,
    PREIMAGE_NEXT_PROTOCOL_PARAMS_BYTES,
    circuit::IvcCircuit,
    state::{State, Witness},
    tests::common::{
        asset_readers::{
            load_embedded_recursive_step_output_asset, load_embedded_verification_context_asset,
        },
        generators::{
            GENESIS_EPOCH, build_asset_generation_setup, build_genesis_base_case_next_state,
            build_genesis_base_case_witness,
        },
        helpers::{
            assert_recursive_mock_prover_rejects, build_recursive_mock_prover_setup,
            verify_and_prepare_blake2b_recursive_proof,
        },
    },
};

#[test]
fn next_merkle_root_tampered_public_input_is_rejected() {
    // Asset-based check that the verifier rejects a stored proof when
    // next_merkle_root is replaced in the public inputs, confirming the field
    // extracted from PREIMAGE_NEXT_MERKLE_ROOT_BYTES is enforced as a public input.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let recursive_step_output = load_embedded_recursive_step_output_asset()
        .expect("recursive step output asset should load");

    let mut tampered_state = recursive_step_output.next_state.clone();
    tampered_state.next_merkle_root = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&recursive_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &recursive_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered next_merkle_root should be rejected by the verifier"
    );
}

#[test]
fn next_protocol_params_tampered_public_input_is_rejected() {
    // Asset-based check that the verifier rejects a stored proof when
    // next_protocol_params is replaced in the public inputs, confirming the
    // field extracted from PREIMAGE_NEXT_PROTOCOL_PARAMS_BYTES is enforced as a public input.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let recursive_step_output = load_embedded_recursive_step_output_asset()
        .expect("recursive step output asset should load");

    let mut tampered_state = recursive_step_output.next_state.clone();
    tampered_state.next_protocol_params = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&recursive_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &recursive_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered next_protocol_params should be rejected by the verifier"
    );
}

#[test]
fn current_epoch_tampered_public_input_is_rejected() {
    // Asset-based check that the verifier rejects a stored proof when
    // current_epoch is replaced in the public inputs, confirming the field
    // extracted from PREIMAGE_CURRENT_EPOCH_BYTES is enforced as a public input.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let recursive_step_output = load_embedded_recursive_step_output_asset()
        .expect("recursive step output asset should load");

    let mut tampered_state = recursive_step_output.next_state.clone();
    tampered_state.current_epoch = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&recursive_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &recursive_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered current_epoch should be rejected by the verifier"
    );
}

mod slow {
    use super::*;

    /// Builds a genesis circuit with a tampered witness and asserts the MockProver rejects it.
    ///
    /// `tamper` receives the genesis witness before it is passed to the circuit,
    /// allowing each test to corrupt exactly the byte range it wants to verify.
    fn assert_genesis_circuit_rejects_tampered_witness(tamper: impl FnOnce(&mut Witness)) {
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_recursive_mock_prover_setup(&setup);

        let mut witness = build_genesis_base_case_witness(&setup);
        tamper(&mut witness);

        let circuit = IvcCircuit::new(
            mock_prover_setup.global.clone(),
            State::genesis(),
            witness,
            vec![],
            vec![],
            mock_prover_setup.trivial_accumulator.clone(),
            mock_prover_setup.certificate_verifying_key.vk(),
            &mock_prover_setup.recursive_verifying_key,
        );

        let public_inputs = [
            mock_prover_setup.global.as_public_input(),
            build_genesis_base_case_next_state(&setup, GENESIS_EPOCH).as_public_input(),
            AssignedAccumulator::as_public_input(&mock_prover_setup.trivial_accumulator),
        ]
        .concat();

        assert_recursive_mock_prover_rejects(circuit, public_inputs);
    }

    #[test]
    fn circuit_rejects_wrong_bytes_at_next_merkle_root_range() {
        // MockProver check that the circuit rejects a genesis witness where
        // msg_preimage[PREIMAGE_NEXT_MERKLE_ROOT_BYTES] contains wrong bytes,
        // confirming the byte extraction constraint for PREIMAGE_NEXT_MERKLE_ROOT_BYTES is enforced.
        assert_genesis_circuit_rejects_tampered_witness(|w| {
            w.msg_preimage[PREIMAGE_NEXT_MERKLE_ROOT_BYTES].fill(0xff)
        });
    }

    #[test]
    fn circuit_rejects_wrong_bytes_at_next_protocol_params_range() {
        // MockProver check that the circuit rejects a genesis witness where
        // msg_preimage[PREIMAGE_NEXT_PROTOCOL_PARAMS_BYTES] contains wrong bytes,
        // confirming the byte extraction constraint for PREIMAGE_NEXT_PROTOCOL_PARAMS_BYTES is enforced.
        assert_genesis_circuit_rejects_tampered_witness(|w| {
            w.msg_preimage[PREIMAGE_NEXT_PROTOCOL_PARAMS_BYTES].fill(0xff)
        });
    }

    #[test]
    fn circuit_rejects_wrong_bytes_at_current_epoch_range() {
        // MockProver check that the circuit rejects a genesis witness where
        // msg_preimage[PREIMAGE_CURRENT_EPOCH_BYTES] contains wrong bytes,
        // confirming the byte extraction constraint for PREIMAGE_CURRENT_EPOCH_BYTES is enforced.
        assert_genesis_circuit_rejects_tampered_witness(|w| {
            w.msg_preimage[PREIMAGE_CURRENT_EPOCH_BYTES].fill(0xff)
        });
    }
}
