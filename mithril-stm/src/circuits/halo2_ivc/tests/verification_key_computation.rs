use std::{fs::File, io::BufReader, path::PathBuf};

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
        halo2_ivc::{K, circuit::IvcCircuit},
    },
};

/// Function used to compute a recursive circuit verification key.
/// It loads a SRS and the non-recursive verification key for production
/// to generate a verification key for the recursive circuit.
/// It can generate the verification key for the recursive circuit if given
/// Midnight SRS.
///
/// TODO: remove #[allow(dead_code)] when used
#[allow(dead_code)]
fn compute_recursive_circuit_verification_key(srs_path: &PathBuf) -> StmResult<Vec<u8>> {
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
