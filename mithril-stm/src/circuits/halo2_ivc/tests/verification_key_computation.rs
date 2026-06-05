use anyhow::Context;
use midnight_circuits::verifier::{BlstrsEmulation, SelfEmulation};
use midnight_proofs::{
    plonk::{VerifyingKey, keygen_vk_with_k},
    poly::kzg::KZGCommitmentScheme,
    utils::SerdeFormat,
};
use midnight_zk_stdlib::MidnightVK;

use crate::{
    StmResult,
    circuits::{
        halo2::NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
        halo2_ivc::{
            K, RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION, circuit::IvcCircuitData,
        },
        trusted_setup::TrustedSetupProvider,
    },
};

/// Derive the recursive circuit verification key from the production SRS and the hardcoded
/// non-recursive VK. Used by [`write_recursive_circuit_verification_key_for_production_to_file`]
/// to regenerate `recursive_circuit_verification_key_for_production.bin` when the circuit changes.
fn compute_recursive_circuit_verification_key() -> StmResult<Vec<u8>> {
    let shared_srs_degree = K;
    let srs = TrustedSetupProvider::default().get_trusted_setup_parameters()?;
    // The recursive circuit uses a fixed SRS degree K, smaller than the full production SRS.
    let mut recursive_commitment_parameters = srs;
    recursive_commitment_parameters.downsize(shared_srs_degree);

    let mut non_recursive_verification_key = NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION;
    let certificate_verifying_key =
        MidnightVK::read(&mut non_recursive_verification_key, SerdeFormat::RawBytes)
            .with_context(|| "Failed to deserialize the circuit verification key.")?;

    let default_ivc_circuit = IvcCircuitData::unknown(certificate_verifying_key.vk())
        .expect("valid IvcCircuitData unknown");
    let recursive_verification_key: VerifyingKey<
        <BlstrsEmulation as SelfEmulation>::F,
        KZGCommitmentScheme<<BlstrsEmulation as SelfEmulation>::Engine>,
    > = keygen_vk_with_k(
        &recursive_commitment_parameters,
        &default_ivc_circuit,
        shared_srs_degree,
    )?;

    let mut buffer_for_recursive_circuit_verification_key = vec![];
    recursive_verification_key
        .write(
            &mut buffer_for_recursive_circuit_verification_key,
            SerdeFormat::RawBytes,
        )
        .with_context(|| "Failed to write the recursive circuit verification key to a buffer.")?;
    Ok(buffer_for_recursive_circuit_verification_key)
}

/// Function that recomputes the recursive production key and updates
/// the binary file storing its value
#[test]
#[ignore]
pub fn write_recursive_circuit_verification_key_for_production_to_file() {
    let verification_key = compute_recursive_circuit_verification_key().unwrap();

    std::fs::write(
        "src/circuits/halo2_ivc/recursive_circuit_verification_key_for_production.bin",
        verification_key,
    )
    .unwrap();
}

/// Function that recomputes the recursive production key and checks its value
/// against the value used to compile the library
#[test]
#[ignore]
fn integrity_test_for_recursive_production_key() {
    let production_recursive_key = RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION;
    let recomputed_recursive_key = compute_recursive_circuit_verification_key().unwrap();
    assert_eq!(production_recursive_key, recomputed_recursive_key);
}
