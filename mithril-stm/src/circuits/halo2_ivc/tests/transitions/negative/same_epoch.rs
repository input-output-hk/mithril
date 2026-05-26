use super::*;

use crate::circuits::halo2_ivc::{
    state::Witness,
    tests::common::{
        asset_readers::{
            load_embedded_recursive_chain_state_asset, load_embedded_same_epoch_step_output_asset,
        },
        generators::{
            build_asset_generation_setup, same_epoch_message_and_preimage_for_step,
            same_epoch_next_state_for_step,
        },
        helpers::{
            assert_recursive_mock_prover_rejects_with_label, build_mock_prover_public_inputs,
            build_mock_prover_setup_from_assets, build_trivial_mock_prover_circuit,
        },
    },
};

#[test]
fn merkle_root_tampered_is_rejected() {
    // Asset-based check: circuit enforces merkle_root = prev.merkle_root in a same-epoch transition.
    assert_step_output_rejects_tampered_state(
        load_embedded_same_epoch_step_output_asset,
        "same-epoch step output",
        |s| s.merkle_root = F::ONE,
        "proof with tampered merkle_root should be rejected by the verifier",
    );
}

#[test]
fn next_merkle_root_tampered_is_rejected() {
    // Asset-based check: circuit enforces next_merkle_root is extracted from the certificate message preimage.
    assert_step_output_rejects_tampered_state(
        load_embedded_same_epoch_step_output_asset,
        "same-epoch step output",
        |s| s.next_merkle_root = F::ONE,
        "proof with tampered next_merkle_root should be rejected by the verifier",
    );
}

#[test]
fn protocol_params_tampered_is_rejected() {
    // Asset-based check: circuit enforces protocol_params = prev.protocol_params in a same-epoch transition.
    assert_step_output_rejects_tampered_state(
        load_embedded_same_epoch_step_output_asset,
        "same-epoch step output",
        |s| s.protocol_params = F::ONE,
        "proof with tampered protocol_params should be rejected by the verifier",
    );
}

#[test]
fn next_protocol_params_tampered_is_rejected() {
    // Asset-based check: circuit enforces next_protocol_params is extracted from the certificate message preimage.
    assert_step_output_rejects_tampered_state(
        load_embedded_same_epoch_step_output_asset,
        "same-epoch step output",
        |s| s.next_protocol_params = F::ONE,
        "proof with tampered next_protocol_params should be rejected by the verifier",
    );
}

#[test]
fn current_epoch_tampered_is_rejected() {
    // Asset-based check: circuit enforces current_epoch is unchanged in a same-epoch transition.
    assert_step_output_rejects_tampered_state(
        load_embedded_same_epoch_step_output_asset,
        "same-epoch step output",
        |s| s.current_epoch = F::ONE,
        "proof with tampered current_epoch should be rejected by the verifier",
    );
}

#[test]
fn counter_tampered_is_rejected() {
    // Asset-based check: circuit enforces counter increments by 1 at every recursive step.
    assert_step_output_rejects_tampered_state(
        load_embedded_same_epoch_step_output_asset,
        "same-epoch step output",
        |s| s.counter = F::ONE,
        "proof with tampered counter should be rejected by the verifier",
    );
}

#[test]
fn msg_tampered_is_rejected() {
    // Asset-based check: circuit enforces msg equals the certificate message hash verified in-circuit.
    assert_step_output_rejects_tampered_state(
        load_embedded_same_epoch_step_output_asset,
        "same-epoch step output",
        |s| s.msg = F::ONE,
        "proof with tampered msg should be rejected by the verifier",
    );
}

mod slow {
    use super::*;

    #[test]
    fn circuit_rejects_merkle_root_carry_violation_in_same_epoch_step() {
        // MockProver constraint check: in a same-epoch transition the circuit must carry
        // merkle_root unchanged from prev_state. Setting it to ONE violates that constraint.
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_mock_prover_setup_from_assets(&setup);
        let prev_state = load_embedded_recursive_chain_state_asset()
            .expect("recursive chain state asset should load")
            .state;
        let (message, preimage_bytes) =
            same_epoch_message_and_preimage_for_step(&setup, &prev_state);
        let witness = Witness::new(
            setup.genesis_signature,
            prev_state.merkle_root,
            message,
            preimage_bytes
                .try_into()
                .expect("same-epoch preimage should be PREIMAGE_SIZE bytes"),
        );
        let mut tampered_state = same_epoch_next_state_for_step(&prev_state, message);
        tampered_state.merkle_root = F::ONE;
        let circuit = build_trivial_mock_prover_circuit(&mock_prover_setup, prev_state, witness);
        let public_inputs = build_mock_prover_public_inputs(&mock_prover_setup, &tampered_state);
        assert_recursive_mock_prover_rejects_with_label(
            circuit,
            public_inputs,
            "merkle_root set to ONE (same-epoch: must carry prev.merkle_root unchanged)",
        );
    }

    #[test]
    fn circuit_rejects_protocol_params_carry_violation_in_same_epoch_step() {
        // MockProver constraint check: in a same-epoch transition the circuit must carry
        // protocol_params unchanged from prev_state. Setting it to ONE violates that constraint.
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_mock_prover_setup_from_assets(&setup);
        let prev_state = load_embedded_recursive_chain_state_asset()
            .expect("recursive chain state asset should load")
            .state;
        let (message, preimage_bytes) =
            same_epoch_message_and_preimage_for_step(&setup, &prev_state);
        let witness = Witness::new(
            setup.genesis_signature,
            prev_state.merkle_root,
            message,
            preimage_bytes
                .try_into()
                .expect("same-epoch preimage should be PREIMAGE_SIZE bytes"),
        );
        let mut tampered_state = same_epoch_next_state_for_step(&prev_state, message);
        tampered_state.protocol_params = F::ONE;
        let circuit = build_trivial_mock_prover_circuit(&mock_prover_setup, prev_state, witness);
        let public_inputs = build_mock_prover_public_inputs(&mock_prover_setup, &tampered_state);
        assert_recursive_mock_prover_rejects_with_label(
            circuit,
            public_inputs,
            "protocol_params set to ONE (same-epoch: must carry prev.protocol_params unchanged)",
        );
    }

    #[test]
    fn circuit_rejects_epoch_advance_in_same_epoch_step() {
        // MockProver constraint check: in a same-epoch transition the circuit must keep
        // current_epoch equal to prev_state.current_epoch. Incrementing it violates that constraint.
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_mock_prover_setup_from_assets(&setup);
        let prev_state = load_embedded_recursive_chain_state_asset()
            .expect("recursive chain state asset should load")
            .state;
        let (message, preimage_bytes) =
            same_epoch_message_and_preimage_for_step(&setup, &prev_state);
        let witness = Witness::new(
            setup.genesis_signature,
            prev_state.merkle_root,
            message,
            preimage_bytes
                .try_into()
                .expect("same-epoch preimage should be PREIMAGE_SIZE bytes"),
        );
        let mut tampered_state = same_epoch_next_state_for_step(&prev_state, message);
        tampered_state.current_epoch += F::ONE;
        let circuit = build_trivial_mock_prover_circuit(&mock_prover_setup, prev_state, witness);
        let public_inputs = build_mock_prover_public_inputs(&mock_prover_setup, &tampered_state);
        assert_recursive_mock_prover_rejects_with_label(
            circuit,
            public_inputs,
            "current_epoch incremented (same-epoch: must equal prev.current_epoch)",
        );
    }
}
