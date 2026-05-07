//! Tests that the circuit correctly enforces constraints linking the witness,
//! current state, and next state for same-epoch and next-epoch transitions.
//!
//! All tests are slow MockProver checks confirming the arithmetic constraint
//! wiring for the following in-circuit invariants:
//!
//! `next_merkle_root` consistency    — same-epoch; must equal `prev_state.next_merkle_root`.
//! `next_protocol_params` consistency — same-epoch; must equal `prev_state.next_protocol_params`.
//! `msg = Blake2b(preimage)`          — same-epoch and next-epoch.

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
                assert_recursive_mock_prover_rejects_with_label, build_mock_prover_public_inputs,
                build_recursive_mock_prover_setup, build_trivial_mock_prover_circuit,
            },
        },
    };

    #[test]
    fn circuit_rejects_all_wrong_state_transition_fields() {
        // MockProver constraint check: one setup call shared across three same-epoch tampers
        // and one next-epoch tamper. The label in each assertion identifies the failing case.
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_recursive_mock_prover_setup(&setup);
        let prev_state = load_embedded_recursive_chain_state_asset()
            .expect("recursive chain state asset should load")
            .state;

        // Same-epoch tampers: next_merkle_root, next_protocol_params, msg (3 cases).
        let (se_message, se_preimage_bytes) =
            same_epoch_message_and_preimage_for_step(&setup, &prev_state);
        for (label, tamper) in [
            (
                "next_merkle_root set to ONE (same-epoch: must equal prev.next_merkle_root)",
                (|s: &mut State| s.next_merkle_root = F::ONE) as fn(&mut State),
            ),
            (
                "next_protocol_params set to ONE (same-epoch: must equal prev.next_protocol_params)",
                |s: &mut State| s.next_protocol_params = F::ONE,
            ),
            (
                "msg set to ONE (same-epoch: must equal Blake2b(msg_preimage))",
                |s: &mut State| s.msg = F::ONE,
            ),
        ] {
            let witness = Witness::new(
                setup.genesis_signature.clone(),
                prev_state.merkle_root,
                se_message,
                se_preimage_bytes
                    .clone()
                    .try_into()
                    .expect("same-epoch preimage should be PREIMAGE_SIZE bytes"),
            );
            let mut tampered_state = same_epoch_next_state_for_step(&prev_state, se_message);
            tamper(&mut tampered_state);
            let circuit =
                build_trivial_mock_prover_circuit(&mock_prover_setup, prev_state.clone(), witness);
            assert_recursive_mock_prover_rejects_with_label(
                circuit,
                build_mock_prover_public_inputs(&mock_prover_setup, &tampered_state),
                label,
            );
        }

        // Next-epoch tamper: msg (1 case).
        let (ne_message, ne_preimage_bytes) =
            next_message_and_preimage_for_step(&setup, &prev_state);
        let ne_witness = Witness::new(
            setup.genesis_signature.clone(),
            prev_state.next_merkle_root,
            ne_message,
            ne_preimage_bytes
                .try_into()
                .expect("next-epoch preimage should be PREIMAGE_SIZE bytes"),
        );
        let mut ne_tampered_state = next_state_for_step(&prev_state, ne_message);
        ne_tampered_state.msg = F::ONE;
        let ne_circuit =
            build_trivial_mock_prover_circuit(&mock_prover_setup, prev_state, ne_witness);
        assert_recursive_mock_prover_rejects_with_label(
            ne_circuit,
            build_mock_prover_public_inputs(&mock_prover_setup, &ne_tampered_state),
            "msg set to ONE (next-epoch: must equal Blake2b(msg_preimage))",
        );
    }
}
