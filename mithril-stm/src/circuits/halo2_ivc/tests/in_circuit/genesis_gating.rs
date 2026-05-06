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
        helpers::{assert_recursive_mock_prover_accepts, build_recursive_mock_prover_setup},
    },
};

mod slow {
    use super::*;

    /// Builds a genesis circuit with the given `cert_proof` and `self_proof` bytes and
    /// asserts that the MockProver accepts all constraints.
    ///
    /// At genesis (`state.counter = 0`) the circuit gates both proof contributions via
    /// `scale_by_bit(is_not_genesis = 0, acc)`, forcing each accumulator to the group
    /// identity regardless of the supplied bytes. The trivial accumulator is therefore
    /// the correct public input even when garbage bytes are supplied.
    fn assert_genesis_step_accepts_garbage_proof_bytes(cert_proof: Vec<u8>, self_proof: Vec<u8>) {
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_recursive_mock_prover_setup(&setup);

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

        let public_inputs = [
            mock_prover_setup.global.as_public_input(),
            build_genesis_base_case_next_state(&setup, GENESIS_EPOCH).as_public_input(),
            AssignedAccumulator::as_public_input(&mock_prover_setup.trivial_accumulator),
        ]
        .concat();

        assert_recursive_mock_prover_accepts(circuit, public_inputs);
    }

    #[test]
    fn genesis_step_accepts_garbage_certificate_proof_bytes() {
        // MockProver: confirms that the genesis gating zeroes the certificate proof
        // accumulator contribution when the step counter is zero, so garbage
        // certificate proof bytes satisfy all circuit constraints.
        assert_genesis_step_accepts_garbage_proof_bytes(vec![0u8; 64], vec![]);
    }

    #[test]
    fn genesis_step_accepts_garbage_ivc_proof_bytes() {
        // MockProver: confirms that the genesis gating zeroes the IVC proof
        // accumulator contribution when the step counter is zero, so garbage
        // IVC proof bytes satisfy all circuit constraints.
        assert_genesis_step_accepts_garbage_proof_bytes(vec![], vec![0u8; 64]);
    }
}
