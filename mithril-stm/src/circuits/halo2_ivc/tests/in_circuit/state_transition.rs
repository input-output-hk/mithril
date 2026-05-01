//! Tests that the circuit correctly enforces constraints linking the witness,
//! current state, and next state for same-epoch and next-epoch transitions.
//!
//! These MockProver checks confirm that in-circuit arithmetic constraints not
//! yet covered by Layer B slow tests are wired correctly. Layer B already
//! covers `merkle_root`, `protocol_params`, and `current_epoch` linkage via
//! MockProver; the tests here fill the remaining gaps:
//!
//! `next_merkle_root` consistency  — same-epoch only; must equal `prev_state.next_merkle_root`.
//! `next_protocol_params` consistency — same-epoch only; must equal `prev_state.next_protocol_params`.
//! `msg = SHA256(preimage)` — same-epoch and next-epoch (genesis is covered by Layer B).

mod slow {
    use ff::Field;

    use crate::circuits::halo2_ivc::{
        F,
        state::{State, Witness},
        tests::common::{
            asset_readers::load_embedded_recursive_chain_state_asset,
            generators::{
                build_asset_generation_setup, next_message_and_preimage_for_step,
                next_state_for_step, same_epoch_message_and_preimage_for_step,
                same_epoch_next_state_for_step,
            },
            helpers::{
                assert_recursive_mock_prover_rejects, build_mock_prover_public_inputs,
                build_recursive_mock_prover_setup, build_trivial_mock_prover_circuit,
            },
        },
    };

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
    fn circuit_rejects_with_wrong_next_merkle_root_consistency() {
        // MockProver check that the in-circuit constraint keeping next_merkle_root
        // unchanged across a same-epoch transition is wired correctly.
        assert_same_epoch_circuit_rejects_tampered_state(|s| s.next_merkle_root = F::ONE);
    }

    #[test]
    fn circuit_rejects_with_wrong_next_protocol_params_consistency() {
        // MockProver check that the in-circuit constraint keeping next_protocol_params
        // unchanged across a same-epoch transition is wired correctly.
        assert_same_epoch_circuit_rejects_tampered_state(|s| s.next_protocol_params = F::ONE);
    }

    #[test]
    fn circuit_rejects_msg_inconsistent_with_preimage_in_same_epoch_step() {
        // MockProver check that the in-circuit Blake2b hash constraint between
        // msg_preimage bytes and the resulting msg field is wired correctly for
        // a same-epoch transition.
        assert_same_epoch_circuit_rejects_tampered_state(|s| s.msg = F::ONE);
    }

    #[test]
    fn circuit_rejects_msg_inconsistent_with_preimage_in_next_epoch_step() {
        // MockProver check that the in-circuit Blake2b hash constraint between
        // msg_preimage bytes and the resulting msg field is wired correctly for
        // a next-epoch transition.
        assert_next_epoch_circuit_rejects_tampered_state(|s| s.msg = F::ONE);
    }
}
