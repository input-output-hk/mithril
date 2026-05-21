//! Negative tests for verify_and_prepare_accumulator.
//!
//! Confirms that CertificateProofRejected is returned for:
//! - garbage proof bytes (transcript parse failure)
//! - a valid proof paired with wrong public inputs (pairing check failure)

use ff::Field;

use crate::circuits::halo2::types::CircuitBase;
use crate::circuits::halo2_ivc::{
    certificate_proof::verify_and_prepare_accumulator,
    errors::IvcCircuitError,
    tests::common::asset_readers::{
        load_embedded_same_epoch_step_output_asset,
        load_embedded_verification_context_asset,
    },
};

#[test]
fn verify_and_prepare_accumulator_rejects_garbage_proof_bytes() {
    // Garbage bytes cannot be parsed as a valid Poseidon transcript, so
    // `prepare` returns an error and the function returns CertificateProofRejected.
    let ctx = load_embedded_verification_context_asset()
        .expect("verification context asset should load");

    let result = verify_and_prepare_accumulator(
        &vec![0u8; 64],
        &[],
        &ctx.certificate_verifying_key,
        &ctx.verifier_params,
    );

    let err = result
        .unwrap_err()
        .downcast::<IvcCircuitError>()
        .expect("error should downcast to IvcCircuitError");
    assert_eq!(err, IvcCircuitError::CertificateProofRejected);
}

#[test]
fn verify_and_prepare_accumulator_rejects_valid_proof_with_wrong_public_inputs() {
    // A valid cert proof paired with wrong public inputs produces a DualMSM
    // that fails the pairing check, so the function returns CertificateProofRejected.
    let ctx = load_embedded_verification_context_asset()
        .expect("verification context asset should load");
    let step_output = load_embedded_same_epoch_step_output_asset()
        .expect("same-epoch step output asset should load");

    let wrong_inputs = vec![CircuitBase::ZERO, CircuitBase::ZERO];

    let result = verify_and_prepare_accumulator(
        &step_output.certificate_proof,
        &wrong_inputs,
        &ctx.certificate_verifying_key,
        &ctx.verifier_params,
    );

    let err = result
        .unwrap_err()
        .downcast::<IvcCircuitError>()
        .expect("error should downcast to IvcCircuitError");
    assert_eq!(err, IvcCircuitError::CertificateProofRejected);
}
