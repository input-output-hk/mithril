//! Tests that the circuit bypasses proof verification at genesis (step 0).
//!
//! At genesis the circuit must accept arbitrary certificate and IVC proof bytes
//! because there is no prior certificate or IVC proof to verify. These slow
//! tests use the MockProver to confirm that the gating condition nullifies any
//! accumulator contribution from garbage proof bytes at the constraint level.

use midnight_circuits::types::Instantiable;

use crate::circuits::halo2_ivc::{
    AssignedAccumulator,
    circuit::IvcCircuit,
    state::State,
    tests::common::{
        generators::{
            GENESIS_EPOCH, build_asset_generation_setup, build_genesis_base_case_next_state,
            build_genesis_base_case_witness,
        },
        helpers::{
            assert_recursive_mock_prover_accepts_with_label, build_recursive_mock_prover_setup,
        },
    },
};

mod slow {
    use super::*;

    #[test]
    fn genesis_step_accepts_garbage_proof_bytes_for_both_slots() {
        // MockProver constraint check: at genesis (counter = 0) the circuit gates both
        // proof accumulator contributions to the group identity via scale_by_bit(0, acc),
        // so garbage bytes in either slot must not violate any constraint.
        // One setup call shared across both cases. The label identifies which slot failed.
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_recursive_mock_prover_setup(&setup);
        let public_inputs = [
            mock_prover_setup.global.as_public_input(),
            build_genesis_base_case_next_state(&setup, GENESIS_EPOCH).as_public_input(),
            AssignedAccumulator::as_public_input(&mock_prover_setup.trivial_accumulator),
        ]
        .concat();

        for (label, cert_proof, self_proof) in [
            (
                "garbage certificate proof bytes (64 × 0x00)",
                vec![0u8; 64],
                vec![],
            ),
            ("garbage IVC proof bytes (64 × 0x00)", vec![], vec![0u8; 64]),
        ] {
            let circuit = IvcCircuit::new(
                mock_prover_setup.global.clone(),
                State::genesis(),
                build_genesis_base_case_witness(&setup),
                cert_proof,
                self_proof,
                mock_prover_setup.trivial_accumulator.clone(),
                mock_prover_setup.certificate_verifying_key.vk(),
                &mock_prover_setup.recursive_verifying_key,
            );
            assert_recursive_mock_prover_accepts_with_label(circuit, public_inputs.clone(), label);
        }
    }
}
