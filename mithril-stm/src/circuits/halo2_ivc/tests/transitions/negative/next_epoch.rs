use super::*;

use crate::circuits::halo2_ivc::{
    state::Witness,
    tests::common::{
        asset_readers::{
            load_embedded_recursive_chain_state_asset, load_embedded_recursive_step_output_asset,
        },
        generators::{
            build_asset_generation_setup, next_message_and_preimage_for_step, next_state_for_step,
        },
        helpers::{
            assert_recursive_mock_prover_rejects_with_label, build_mock_prover_public_inputs,
            build_mock_prover_setup_from_assets, build_trivial_mock_prover_circuit,
        },
    },
    types::{
        EpochNumber, MerkleTreeCommitment, MessageHash, ProtocolMessagePreimage,
        ProtocolParametersHash, StepCounter,
    },
};

#[test]
fn merkle_root_tampered_is_rejected() {
    // Asset-based check: circuit enforces merkle_root = prev.next_merkle_root in a next-epoch transition.
    assert_step_output_rejects_tampered_state(
        load_embedded_recursive_step_output_asset,
        "recursive step output",
        |s| s.merkle_root = MerkleTreeCommitment::from_field(F::ONE),
        "proof with tampered merkle_root should be rejected by the verifier",
    );
}

#[test]
fn protocol_params_tampered_is_rejected() {
    // Asset-based check: circuit enforces protocol_params = prev.next_protocol_params in a next-epoch transition.
    assert_step_output_rejects_tampered_state(
        load_embedded_recursive_step_output_asset,
        "recursive step output",
        |s| s.protocol_params = ProtocolParametersHash::from_field(F::ONE),
        "proof with tampered protocol_params should be rejected by the verifier",
    );
}

#[test]
fn next_merkle_root_tampered_is_rejected() {
    // Asset-based check: circuit enforces next_merkle_root is extracted from the certificate message preimage.
    assert_step_output_rejects_tampered_state(
        load_embedded_recursive_step_output_asset,
        "recursive step output",
        |s| s.next_merkle_root = MerkleTreeCommitment::from_field(F::ONE),
        "proof with tampered next_merkle_root should be rejected by the verifier",
    );
}

#[test]
fn next_protocol_params_tampered_is_rejected() {
    // Asset-based check: circuit enforces next_protocol_params is extracted from the certificate message preimage.
    assert_step_output_rejects_tampered_state(
        load_embedded_recursive_step_output_asset,
        "recursive step output",
        |s| s.next_protocol_params = ProtocolParametersHash::from_field(F::ONE),
        "proof with tampered next_protocol_params should be rejected by the verifier",
    );
}

#[test]
fn counter_tampered_is_rejected() {
    // Asset-based check: circuit enforces counter = prev.counter + 1 in a next-epoch transition.
    assert_step_output_rejects_tampered_state(
        load_embedded_recursive_step_output_asset,
        "recursive step output",
        |s| s.counter = StepCounter::from_field(F::ONE),
        "proof with tampered counter should be rejected by the verifier",
    );
}

#[test]
fn current_epoch_tampered_is_rejected() {
    // Asset-based check: circuit enforces current_epoch = prev.current_epoch + 1 in a next-epoch transition.
    assert_step_output_rejects_tampered_state(
        load_embedded_recursive_step_output_asset,
        "recursive step output",
        |s| s.current_epoch = EpochNumber::ZERO,
        "proof with tampered current_epoch should be rejected by the verifier",
    );
}

#[test]
fn msg_tampered_is_rejected() {
    // Asset-based check: circuit enforces msg equals the certificate message hash verified in-circuit.
    assert_step_output_rejects_tampered_state(
        load_embedded_recursive_step_output_asset,
        "recursive step output",
        |s| s.msg = MessageHash::from_field(F::ONE),
        "proof with tampered msg should be rejected by the verifier",
    );
}

mod slow {
    use super::*;

    #[test]
    fn circuit_rejects_protocol_params_non_advance_in_next_epoch_step() {
        // MockProver constraint check: in a next-epoch transition the circuit must advance
        // protocol_params to prev_state.next_protocol_params. Setting it to ONE violates that.
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_mock_prover_setup_from_assets(&setup);
        let prev_state = load_embedded_recursive_chain_state_asset()
            .expect("recursive chain state asset should load")
            .state;
        let (message, preimage_bytes) = next_message_and_preimage_for_step(&setup, &prev_state);
        let witness = Witness::new(
            setup.genesis_signature,
            prev_state.next_merkle_root,
            MessageHash::from_field(message),
            ProtocolMessagePreimage::new(
                preimage_bytes
                    .try_into()
                    .expect("next-epoch preimage should be PREIMAGE_SIZE bytes"),
            ),
        );
        let mut tampered_state = next_state_for_step(&prev_state, message);
        tampered_state.protocol_params = ProtocolParametersHash::from_field(F::ONE);
        let circuit = build_trivial_mock_prover_circuit(&mock_prover_setup, prev_state, witness);
        let public_inputs = build_mock_prover_public_inputs(&mock_prover_setup, &tampered_state);
        assert_recursive_mock_prover_rejects_with_label(
            circuit,
            public_inputs,
            "protocol_params set to ONE (next-epoch: must advance to prev.next_protocol_params)",
        );
    }

    #[test]
    fn circuit_rejects_merkle_root_non_advance_in_next_epoch_step() {
        // MockProver constraint check: in a next-epoch transition the circuit must advance
        // merkle_root to prev_state.next_merkle_root. Setting it to ONE violates that.
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_mock_prover_setup_from_assets(&setup);
        let prev_state = load_embedded_recursive_chain_state_asset()
            .expect("recursive chain state asset should load")
            .state;
        let (message, preimage_bytes) = next_message_and_preimage_for_step(&setup, &prev_state);
        let witness = Witness::new(
            setup.genesis_signature,
            prev_state.next_merkle_root,
            MessageHash::from_field(message),
            ProtocolMessagePreimage::new(
                preimage_bytes
                    .try_into()
                    .expect("next-epoch preimage should be PREIMAGE_SIZE bytes"),
            ),
        );
        let mut tampered_state = next_state_for_step(&prev_state, message);
        tampered_state.merkle_root = MerkleTreeCommitment::from_field(F::ONE);
        let circuit = build_trivial_mock_prover_circuit(&mock_prover_setup, prev_state, witness);
        let public_inputs = build_mock_prover_public_inputs(&mock_prover_setup, &tampered_state);
        assert_recursive_mock_prover_rejects_with_label(
            circuit,
            public_inputs,
            "merkle_root set to ONE (next-epoch: must advance to prev.next_merkle_root)",
        );
    }

    #[test]
    fn circuit_rejects_epoch_non_increment_in_next_epoch_step() {
        // MockProver constraint check: in a next-epoch transition the circuit must increment
        // current_epoch by exactly one. Decrementing it violates that constraint.
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_mock_prover_setup_from_assets(&setup);
        let prev_state = load_embedded_recursive_chain_state_asset()
            .expect("recursive chain state asset should load")
            .state;
        let (message, preimage_bytes) = next_message_and_preimage_for_step(&setup, &prev_state);
        let witness = Witness::new(
            setup.genesis_signature,
            prev_state.next_merkle_root,
            MessageHash::from_field(message),
            ProtocolMessagePreimage::new(
                preimage_bytes
                    .try_into()
                    .expect("next-epoch preimage should be PREIMAGE_SIZE bytes"),
            ),
        );
        let mut tampered_state = next_state_for_step(&prev_state, message);
        tampered_state.current_epoch = EpochNumber::new(tampered_state.current_epoch.as_u64() - 1);
        let circuit = build_trivial_mock_prover_circuit(&mock_prover_setup, prev_state, witness);
        let public_inputs = build_mock_prover_public_inputs(&mock_prover_setup, &tampered_state);
        assert_recursive_mock_prover_rejects_with_label(
            circuit,
            public_inputs,
            "current_epoch decremented (next-epoch: must increment by exactly one)",
        );
    }
}
