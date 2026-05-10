//! Tests that the circuit correctly enforces constraints linking the witness,
//! current state, and next state for same-epoch transitions.
//!
//! All tests are slow MockProver checks confirming the arithmetic constraint
//! wiring for the following in-circuit invariants:
//!
//! `next_merkle_root` consistency    — must equal `prev_state.next_merkle_root`.
//! `next_protocol_params` consistency — must equal `prev_state.next_protocol_params`.
//! `msg = Blake2b(preimage)`          — must equal the Blake2b hash of the message preimage.
//!
//! The `msg = Blake2b(preimage)` constraint is the same gate for same-epoch and
//! next-epoch paths, so only the same-epoch witness is exercised here.

mod slow {
    use ff::Field;

    use crate::circuits::halo2_ivc::{
        F,
        state::Witness,
        tests::common::{
            asset_readers::load_embedded_recursive_chain_state_asset,
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
    fn circuit_rejects_wrong_same_epoch_next_merkle_root() {
        // MockProver constraint check: next_merkle_root set to ONE must violate the
        // in-circuit constraint that pins it to prev_state.next_merkle_root.
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_mock_prover_setup_from_assets(&setup);
        let prev_state = load_embedded_recursive_chain_state_asset()
            .expect("recursive chain state asset should load")
            .state;
        let (same_epoch_message, same_epoch_preimage_bytes) =
            same_epoch_message_and_preimage_for_step(&setup, &prev_state);
        let witness = Witness::new(
            setup.genesis_signature.clone(),
            prev_state.merkle_root,
            same_epoch_message,
            same_epoch_preimage_bytes
                .try_into()
                .expect("same-epoch preimage should be PREIMAGE_SIZE bytes"),
        );
        let mut tampered_state = same_epoch_next_state_for_step(&prev_state, same_epoch_message);
        tampered_state.next_merkle_root = F::ONE;
        let circuit = build_trivial_mock_prover_circuit(&mock_prover_setup, prev_state, witness);
        assert_recursive_mock_prover_rejects_with_label(
            circuit,
            build_mock_prover_public_inputs(&mock_prover_setup, &tampered_state),
            "next_merkle_root set to ONE (must equal prev_state.next_merkle_root)",
        );
    }

    #[test]
    fn circuit_rejects_wrong_same_epoch_next_protocol_params() {
        // MockProver constraint check: next_protocol_params set to ONE must violate the
        // in-circuit constraint that pins it to prev_state.next_protocol_params.
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_mock_prover_setup_from_assets(&setup);
        let prev_state = load_embedded_recursive_chain_state_asset()
            .expect("recursive chain state asset should load")
            .state;
        let (same_epoch_message, same_epoch_preimage_bytes) =
            same_epoch_message_and_preimage_for_step(&setup, &prev_state);
        let witness = Witness::new(
            setup.genesis_signature.clone(),
            prev_state.merkle_root,
            same_epoch_message,
            same_epoch_preimage_bytes
                .try_into()
                .expect("same-epoch preimage should be PREIMAGE_SIZE bytes"),
        );
        let mut tampered_state = same_epoch_next_state_for_step(&prev_state, same_epoch_message);
        tampered_state.next_protocol_params = F::ONE;
        let circuit = build_trivial_mock_prover_circuit(&mock_prover_setup, prev_state, witness);
        assert_recursive_mock_prover_rejects_with_label(
            circuit,
            build_mock_prover_public_inputs(&mock_prover_setup, &tampered_state),
            "next_protocol_params set to ONE (must equal prev_state.next_protocol_params)",
        );
    }

    #[test]
    fn circuit_rejects_wrong_same_epoch_msg_blake2b_constraint() {
        // MockProver constraint check: msg set to ONE must violate the in-circuit
        // Blake2b constraint enforcing msg = Blake2b(msg_preimage). The same gate
        // is exercised by both same-epoch and next-epoch paths, so testing it once suffices.
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_mock_prover_setup_from_assets(&setup);
        let prev_state = load_embedded_recursive_chain_state_asset()
            .expect("recursive chain state asset should load")
            .state;
        let (same_epoch_message, same_epoch_preimage_bytes) =
            same_epoch_message_and_preimage_for_step(&setup, &prev_state);
        let witness = Witness::new(
            setup.genesis_signature.clone(),
            prev_state.merkle_root,
            same_epoch_message,
            same_epoch_preimage_bytes
                .try_into()
                .expect("same-epoch preimage should be PREIMAGE_SIZE bytes"),
        );
        let mut tampered_state = same_epoch_next_state_for_step(&prev_state, same_epoch_message);
        tampered_state.msg = F::ONE;
        let circuit = build_trivial_mock_prover_circuit(&mock_prover_setup, prev_state, witness);
        assert_recursive_mock_prover_rejects_with_label(
            circuit,
            build_mock_prover_public_inputs(&mock_prover_setup, &tampered_state),
            "msg set to ONE (must equal Blake2b(msg_preimage))",
        );
    }
}
