use std::{fs::File, io::BufReader, path::Path};

use anyhow::Context;
use midnight_circuits::verifier::{BlstrsEmulation, SelfEmulation};
use midnight_curves::Bls12;
use midnight_proofs::{
    plonk::{VerifyingKey, keygen_vk_with_k},
    poly::kzg::{KZGCommitmentScheme, params::ParamsKZG},
    utils::SerdeFormat,
};
use midnight_zk_stdlib::MidnightVK;

use crate::{
    StmResult,
    circuits::{
        halo2::NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
        halo2_ivc::{K, RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION, circuit::IvcCircuit},
    },
};

/// Function used to compute a recursive circuit verification key.
/// It loads a SRS and the non-recursive verification key for production
/// to generate a verification key for the recursive circuit.
/// It can generate the verification key for the recursive circuit if given
/// Midnight SRS.
///
// TODO: this function is temporary and needs to be replaced with the function used to compute the verification key for the circuit cache.
fn compute_recursive_circuit_verification_key(srs_path: &Path) -> StmResult<Vec<u8>> {
    let shared_srs_degree = K;

    // Change the path to the settled path for the SRS
    let srs_file =
        File::open(srs_path).with_context(|| "Failed to find the SRS file at the given path.")?;

    let mut reader = BufReader::new(srs_file);
    let srs: ParamsKZG<Bls12> = ParamsKZG::read_custom(&mut reader, SerdeFormat::RawBytes)
        .with_context(|| "Failed to deserialize the SRS.")?;

    // Computes the correct size SRS for the recursive circuit
    let mut recursive_commitment_parameters = srs;
    recursive_commitment_parameters.downsize(shared_srs_degree);

    let mut non_recursive_verification_key = NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION;

    // Loads the non recursive CVK
    let certificate_verifying_key =
        MidnightVK::read(&mut non_recursive_verification_key, SerdeFormat::RawBytes)
            .with_context(|| "Failed to deserialize the circuit verification key.")?;

    // Create the IVC circuit, it is initiliazed with empty public inputs and the verification key
    // of the non-recursive circuit
    let default_ivc_circuit = IvcCircuit::unknown(certificate_verifying_key.vk());
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
    // This path might need to be updated depending on where the Midnight SRS is stored locally
    // TODO: update this path once the SRS location is fixed
    let srs_path = Path::new("../../midnight-srs-2p22");

    let verification_key = compute_recursive_circuit_verification_key(srs_path).unwrap();

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
    // This path might need to be updated depending on where the Midnight SRS is stored locally
    // TODO: update this path once the SRS location is fixed
    let srs_path = Path::new("../../midnight-srs-2p22");
    let production_recursive_key = RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION;

    let recomputed_recursive_key = compute_recursive_circuit_verification_key(srs_path).unwrap();

    assert_eq!(production_recursive_key, recomputed_recursive_key);
}
