//! Negative transition tests: tampered public inputs (fast CI) and MockProver
//! constraint checks (in `mod slow`).

use ff::Field;
use midnight_circuits::types::Instantiable;

use crate::StmResult;
use crate::circuits::halo2_ivc::{
    AssignedAccumulator, F,
    state::{State, Witness},
    tests::common::{
        asset_readers::{
            RecursiveStepOutputAsset, load_embedded_genesis_step_output_asset,
            load_embedded_recursive_chain_state_asset, load_embedded_recursive_step_output_asset,
            load_embedded_same_epoch_step_output_asset, load_embedded_verification_context_asset,
        },
        generators::{
            GENESIS_EPOCH, build_asset_generation_setup, build_genesis_base_case_next_state,
            build_genesis_base_case_witness, next_message_and_preimage_for_step,
            next_state_for_step, same_epoch_message_and_preimage_for_step,
            same_epoch_next_state_for_step,
        },
        helpers::{
            assert_recursive_mock_prover_rejects, build_mock_prover_public_inputs,
            build_recursive_mock_prover_setup, build_trivial_mock_prover_circuit,
            verify_and_prepare_blake2b_recursive_proof,
        },
    },
};

/// Loads `verification_context` and a step output via `load_step_output`, tampers
/// `next_state` via `tamper`, then asserts the verifier rejects the result.
fn assert_step_output_rejects_tampered_state(
    load_step_output: impl FnOnce() -> StmResult<RecursiveStepOutputAsset>,
    load_label: &str,
    tamper: impl FnOnce(&mut State),
    rejection_message: &str,
) {
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let step_output =
        load_step_output().unwrap_or_else(|_| panic!("{load_label} asset should load"));

    let mut tampered_state = step_output.next_state.clone();
    tamper(&mut tampered_state);

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "{rejection_message}"
    );
}

// ---------------------------------------------------------------------------
// Genesis fast tests
// ---------------------------------------------------------------------------

#[test]
fn genesis_merkle_root_tampered_is_rejected() {
    // Asset-based check: circuit enforces merkle_root = 0 at the genesis base case.
    assert_step_output_rejects_tampered_state(
        load_embedded_genesis_step_output_asset,
        "genesis step output",
        |s| s.merkle_root = F::ONE,
        "proof with tampered merkle_root should be rejected by the verifier",
    );
}

#[test]
fn genesis_protocol_params_tampered_is_rejected() {
    // Asset-based check: circuit enforces protocol_params = 0 at the genesis base case.
    assert_step_output_rejects_tampered_state(
        load_embedded_genesis_step_output_asset,
        "genesis step output",
        |s| s.protocol_params = F::ONE,
        "proof with tampered protocol_params should be rejected by the verifier",
    );
}

#[test]
fn genesis_next_merkle_root_tampered_is_rejected() {
    // Asset-based check: circuit enforces next_merkle_root is extracted from the genesis message preimage.
    assert_step_output_rejects_tampered_state(
        load_embedded_genesis_step_output_asset,
        "genesis step output",
        |s| s.next_merkle_root = F::ONE,
        "proof with tampered next_merkle_root should be rejected by the verifier",
    );
}

#[test]
fn genesis_next_protocol_params_tampered_is_rejected() {
    // Asset-based check: circuit enforces next_protocol_params is extracted from the genesis message preimage.
    assert_step_output_rejects_tampered_state(
        load_embedded_genesis_step_output_asset,
        "genesis step output",
        |s| s.next_protocol_params = F::ONE,
        "proof with tampered next_protocol_params should be rejected by the verifier",
    );
}

#[test]
fn genesis_counter_tampered_is_rejected() {
    // Asset-based check: circuit enforces counter transitions 0 → 1 at the genesis base case.
    assert_step_output_rejects_tampered_state(
        load_embedded_genesis_step_output_asset,
        "genesis step output",
        |s| s.counter = F::from(2u64),
        "proof with tampered counter should be rejected by the verifier",
    );
}

#[test]
fn genesis_current_epoch_tampered_is_rejected() {
    // Asset-based check: circuit enforces current_epoch is extracted from the genesis message preimage.
    assert_step_output_rejects_tampered_state(
        load_embedded_genesis_step_output_asset,
        "genesis step output",
        |s| s.current_epoch = F::ONE,
        "proof with tampered current_epoch should be rejected by the verifier",
    );
}

#[test]
fn genesis_msg_tampered_is_rejected() {
    // Asset-based check: circuit enforces msg equals the genesis message committed in the Global public inputs.
    assert_step_output_rejects_tampered_state(
        load_embedded_genesis_step_output_asset,
        "genesis step output",
        |s| s.msg = F::ONE,
        "proof with tampered msg should be rejected by the verifier",
    );
}

// ---------------------------------------------------------------------------
// Same-epoch fast tests
// ---------------------------------------------------------------------------

#[test]
fn same_epoch_merkle_root_tampered_is_rejected() {
    // Asset-based check: circuit enforces merkle_root = prev.merkle_root in a same-epoch transition.
    assert_step_output_rejects_tampered_state(
        load_embedded_same_epoch_step_output_asset,
        "same-epoch step output",
        |s| s.merkle_root = F::ONE,
        "proof with tampered merkle_root should be rejected by the verifier",
    );
}

#[test]
fn same_epoch_next_merkle_root_tampered_is_rejected() {
    // Asset-based check: circuit enforces next_merkle_root is extracted from the certificate message preimage.
    assert_step_output_rejects_tampered_state(
        load_embedded_same_epoch_step_output_asset,
        "same-epoch step output",
        |s| s.next_merkle_root = F::ONE,
        "proof with tampered next_merkle_root should be rejected by the verifier",
    );
}

#[test]
fn same_epoch_protocol_params_tampered_is_rejected() {
    // Asset-based check: circuit enforces protocol_params = prev.protocol_params in a same-epoch transition.
    assert_step_output_rejects_tampered_state(
        load_embedded_same_epoch_step_output_asset,
        "same-epoch step output",
        |s| s.protocol_params = F::ONE,
        "proof with tampered protocol_params should be rejected by the verifier",
    );
}

#[test]
fn same_epoch_next_protocol_params_tampered_is_rejected() {
    // Asset-based check: circuit enforces next_protocol_params is extracted from the certificate message preimage.
    assert_step_output_rejects_tampered_state(
        load_embedded_same_epoch_step_output_asset,
        "same-epoch step output",
        |s| s.next_protocol_params = F::ONE,
        "proof with tampered next_protocol_params should be rejected by the verifier",
    );
}

#[test]
fn same_epoch_current_epoch_tampered_is_rejected() {
    // Asset-based check: circuit enforces current_epoch is unchanged in a same-epoch transition.
    assert_step_output_rejects_tampered_state(
        load_embedded_same_epoch_step_output_asset,
        "same-epoch step output",
        |s| s.current_epoch = F::ONE,
        "proof with tampered current_epoch should be rejected by the verifier",
    );
}

#[test]
fn same_epoch_counter_tampered_is_rejected() {
    // Asset-based check: circuit enforces counter increments by 1 at every recursive step.
    assert_step_output_rejects_tampered_state(
        load_embedded_same_epoch_step_output_asset,
        "same-epoch step output",
        |s| s.counter = F::ONE,
        "proof with tampered counter should be rejected by the verifier",
    );
}

#[test]
fn same_epoch_msg_tampered_is_rejected() {
    // Asset-based check: circuit enforces msg equals the certificate message hash verified in-circuit.
    assert_step_output_rejects_tampered_state(
        load_embedded_same_epoch_step_output_asset,
        "same-epoch step output",
        |s| s.msg = F::ONE,
        "proof with tampered msg should be rejected by the verifier",
    );
}

// ---------------------------------------------------------------------------
// Next-epoch fast tests
// ---------------------------------------------------------------------------

#[test]
fn next_epoch_merkle_root_tampered_is_rejected() {
    // Asset-based check: circuit enforces merkle_root = prev.next_merkle_root in a next-epoch transition.
    assert_step_output_rejects_tampered_state(
        load_embedded_recursive_step_output_asset,
        "recursive step output",
        |s| s.merkle_root = F::ONE,
        "proof with tampered merkle_root should be rejected by the verifier",
    );
}

#[test]
fn next_epoch_protocol_params_tampered_is_rejected() {
    // Asset-based check: circuit enforces protocol_params = prev.next_protocol_params in a next-epoch transition.
    assert_step_output_rejects_tampered_state(
        load_embedded_recursive_step_output_asset,
        "recursive step output",
        |s| s.protocol_params = F::ONE,
        "proof with tampered protocol_params should be rejected by the verifier",
    );
}

#[test]
fn next_epoch_next_merkle_root_tampered_is_rejected() {
    // Asset-based check: circuit enforces next_merkle_root is extracted from the certificate message preimage.
    assert_step_output_rejects_tampered_state(
        load_embedded_recursive_step_output_asset,
        "recursive step output",
        |s| s.next_merkle_root = F::ONE,
        "proof with tampered next_merkle_root should be rejected by the verifier",
    );
}

#[test]
fn next_epoch_next_protocol_params_tampered_is_rejected() {
    // Asset-based check: circuit enforces next_protocol_params is extracted from the certificate message preimage.
    assert_step_output_rejects_tampered_state(
        load_embedded_recursive_step_output_asset,
        "recursive step output",
        |s| s.next_protocol_params = F::ONE,
        "proof with tampered next_protocol_params should be rejected by the verifier",
    );
}

#[test]
fn next_epoch_counter_tampered_is_rejected() {
    // Asset-based check: circuit enforces counter = prev.counter + 1 in a next-epoch transition.
    assert_step_output_rejects_tampered_state(
        load_embedded_recursive_step_output_asset,
        "recursive step output",
        |s| s.counter = F::ONE,
        "proof with tampered counter should be rejected by the verifier",
    );
}

#[test]
fn next_epoch_current_epoch_tampered_is_rejected() {
    // Asset-based check: circuit enforces current_epoch = prev.current_epoch + 1 in a next-epoch transition.
    assert_step_output_rejects_tampered_state(
        load_embedded_recursive_step_output_asset,
        "recursive step output",
        |s| s.current_epoch = F::ZERO,
        "proof with tampered current_epoch should be rejected by the verifier",
    );
}

#[test]
fn next_epoch_msg_tampered_is_rejected() {
    // Asset-based check: circuit enforces msg equals the certificate message hash verified in-circuit.
    assert_step_output_rejects_tampered_state(
        load_embedded_recursive_step_output_asset,
        "recursive step output",
        |s| s.msg = F::ONE,
        "proof with tampered msg should be rejected by the verifier",
    );
}

mod slow {
    use super::*;

    /// Builds a genesis circuit with a tampered next state and asserts the MockProver rejects it.
    ///
    /// `tamper` receives the correct genesis next state before it is passed to the
    /// public inputs, allowing each test to corrupt exactly the field it wants to verify.
    fn assert_genesis_circuit_rejects_tampered_next_state(tamper: impl FnOnce(&mut State)) {
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_recursive_mock_prover_setup(&setup);

        let witness = build_genesis_base_case_witness(&setup);

        let mut tampered_state = build_genesis_base_case_next_state(&setup, GENESIS_EPOCH);
        tamper(&mut tampered_state);

        let circuit =
            build_trivial_mock_prover_circuit(&mock_prover_setup, State::genesis(), witness);
        let public_inputs = build_mock_prover_public_inputs(&mock_prover_setup, &tampered_state);

        assert_recursive_mock_prover_rejects(circuit, public_inputs);
    }

    /// Builds a same-epoch circuit with a tampered next state and asserts the MockProver rejects it.
    ///
    /// `tamper` receives the correct same-epoch next state before it is passed to the
    /// public inputs, allowing each test to corrupt exactly the field it wants to verify.
    fn assert_same_epoch_circuit_rejects_tampered_state(tamper: impl FnOnce(&mut State)) {
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_recursive_mock_prover_setup(&setup);

        let prev_state = load_embedded_recursive_chain_state_asset()
            .expect("recursive chain state asset should load")
            .state;

        let (message, preimage_bytes) =
            same_epoch_message_and_preimage_for_step(&setup, &prev_state);
        let witness = Witness::new(
            setup.genesis_signature.clone(),
            prev_state.merkle_root,
            message,
            preimage_bytes
                .try_into()
                .expect("same-epoch preimage should be PREIMAGE_SIZE bytes"),
        );

        let mut tampered_state = same_epoch_next_state_for_step(&prev_state, message);
        tamper(&mut tampered_state);

        let circuit = build_trivial_mock_prover_circuit(&mock_prover_setup, prev_state, witness);
        let public_inputs = build_mock_prover_public_inputs(&mock_prover_setup, &tampered_state);

        assert_recursive_mock_prover_rejects(circuit, public_inputs);
    }

    /// Builds a next-epoch circuit with a tampered next state and asserts the MockProver rejects it.
    ///
    /// `tamper` receives the correct next-epoch next state before it is passed to the
    /// public inputs, allowing each test to corrupt exactly the field it wants to verify.
    fn assert_next_epoch_circuit_rejects_tampered_state(tamper: impl FnOnce(&mut State)) {
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_recursive_mock_prover_setup(&setup);

        let prev_state = load_embedded_recursive_chain_state_asset()
            .expect("recursive chain state asset should load")
            .state;

        let (message, preimage_bytes) = next_message_and_preimage_for_step(&setup, &prev_state);
        let witness = Witness::new(
            setup.genesis_signature.clone(),
            prev_state.next_merkle_root,
            message,
            preimage_bytes
                .try_into()
                .expect("next-epoch preimage should be PREIMAGE_SIZE bytes"),
        );

        let mut tampered_state = next_state_for_step(&prev_state, message);
        tamper(&mut tampered_state);

        let circuit = build_trivial_mock_prover_circuit(&mock_prover_setup, prev_state, witness);
        let public_inputs = build_mock_prover_public_inputs(&mock_prover_setup, &tampered_state);

        assert_recursive_mock_prover_rejects(circuit, public_inputs);
    }

    #[test]
    fn circuit_rejects_same_epoch_with_wrong_merkle_root_linkage() {
        // MockProver check that the in-circuit constraint carrying merkle_root unchanged
        // across a same-epoch transition is wired correctly.
        assert_same_epoch_circuit_rejects_tampered_state(|s| s.merkle_root = F::ONE);
    }

    #[test]
    fn circuit_rejects_same_epoch_with_wrong_protocol_params_linkage() {
        // MockProver check that the in-circuit constraint carrying protocol_params unchanged
        // across a same-epoch transition is wired correctly.
        assert_same_epoch_circuit_rejects_tampered_state(|s| s.protocol_params = F::ONE);
    }

    #[test]
    fn circuit_rejects_same_epoch_with_wrong_current_epoch_linkage() {
        // MockProver check that the in-circuit constraint keeping current_epoch unchanged
        // across a same-epoch transition is wired correctly.
        assert_same_epoch_circuit_rejects_tampered_state(|s| s.current_epoch += F::ONE);
    }

    #[test]
    fn circuit_rejects_next_epoch_with_wrong_protocol_params_linkage() {
        // MockProver check that the in-circuit constraint advancing protocol_params to
        // prev.next_protocol_params across a next-epoch transition is wired correctly.
        assert_next_epoch_circuit_rejects_tampered_state(|s| s.protocol_params = F::ONE);
    }

    #[test]
    fn circuit_rejects_next_epoch_with_wrong_merkle_root_linkage() {
        // MockProver check that the in-circuit constraint advancing merkle_root to
        // prev.next_merkle_root across a next-epoch transition is wired correctly.
        assert_next_epoch_circuit_rejects_tampered_state(|s| s.merkle_root = F::ONE);
    }

    #[test]
    fn circuit_rejects_next_epoch_with_wrong_current_epoch_increment() {
        // MockProver check that the in-circuit constraint advancing current_epoch by exactly
        // one across a next-epoch transition is wired correctly.
        assert_next_epoch_circuit_rejects_tampered_state(|s| s.current_epoch -= F::ONE);
    }

    #[test]
    fn circuit_rejects_msg_inconsistent_with_preimage() {
        // MockProver check that the in-circuit Blake2b hash constraint between
        // msg_preimage bytes and the resulting msg field is wired correctly.
        assert_genesis_circuit_rejects_tampered_next_state(|s| s.msg = F::ONE);
    }
}
