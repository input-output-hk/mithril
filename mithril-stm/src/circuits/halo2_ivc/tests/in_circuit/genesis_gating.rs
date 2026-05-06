//! Tests that the circuit bypasses proof verification at genesis (step 0).
//!
//! At genesis the circuit must accept arbitrary certificate and IVC proof bytes
//! because there is no prior certificate or IVC proof to verify. These slow
//! tests use the real prover to confirm that the gating condition nullifies
//! any accumulator contribution from invalid proof bytes end-to-end.

use midnight_circuits::types::Instantiable;

use crate::circuits::halo2_ivc::{
    AssignedAccumulator,
    circuit::IvcCircuit,
    state::State,
    tests::common::{
        generators::{
            GENESIS_EPOCH, build_asset_generation_setup, build_genesis_base_case_next_state,
            build_genesis_base_case_witness, build_recursive_proving_key_from_vks,
            prove_poseidon_ivc,
        },
        helpers::{build_recursive_test_setup, verify_prepare_poseidon_recursive_proof},
    },
};

mod slow_real_prover {
    use super::*;

    /// Builds a fresh genesis circuit with the given `cert_proof` and `self_proof` bytes,
    /// runs the real prover, and asserts the resulting proof verifies successfully.
    ///
    /// At genesis (`state.counter = 0`) the circuit gates both proof contributions via
    /// `scale_by_bit(is_not_genesis = 0, acc)`, forcing each accumulator to the group
    /// identity regardless of the supplied bytes.
    fn assert_genesis_step_accepts_garbage_proof_bytes(
        cert_proof: Vec<u8>,
        self_proof: Vec<u8>,
        acceptance_message: &str,
    ) {
        let setup = build_asset_generation_setup();
        let mock_prover_setup = build_recursive_test_setup(&setup);
        let proving_key = build_recursive_proving_key_from_vks(
            &mock_prover_setup.certificate_verifying_key,
            mock_prover_setup.recursive_verifying_key.clone(),
        );

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

        let proof = prove_poseidon_ivc(
            &mock_prover_setup.recursive_commitment_parameters,
            &proving_key,
            &circuit,
            &public_inputs,
            &mut rand_core::OsRng,
        );

        let dual_msm = verify_prepare_poseidon_recursive_proof(
            &mock_prover_setup.recursive_verifying_key,
            &proof,
            &public_inputs,
        );

        assert!(
            dual_msm.check(&mock_prover_setup.universal_verifier_params),
            "{acceptance_message}",
        );
    }

    #[test]
    fn genesis_step_accepts_garbage_certificate_proof_bytes() {
        // Real prover: confirms that the genesis gating zeroes the certificate proof
        // accumulator contribution when the step counter is zero, so all-zero
        // certificate proof bytes do not prevent proof generation or verification.
        assert_genesis_step_accepts_garbage_proof_bytes(
            vec![0u8; 64],
            vec![],
            "genesis step with garbage certificate proof bytes should be accepted by the verifier",
        );
    }

    #[test]
    fn genesis_step_accepts_garbage_ivc_proof_bytes() {
        // Real prover: confirms that the genesis gating zeroes the IVC proof
        // accumulator contribution when the step counter is zero, so all-zero
        // IVC proof bytes do not prevent proof generation or verification.
        assert_genesis_step_accepts_garbage_proof_bytes(
            vec![],
            vec![0u8; 64],
            "genesis step with garbage IVC proof bytes should be accepted by the verifier",
        );
    }
}
