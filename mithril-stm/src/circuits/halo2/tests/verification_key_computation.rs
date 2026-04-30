use std::{fs::File, io::BufReader, path::Path};

use anyhow::Context;
use midnight_proofs::{poly::kzg::params::ParamsKZG, utils::SerdeFormat};

use crate::{
    MERKLE_TREE_DEPTH_FOR_SNARK, Parameters, StmResult,
    circuits::halo2::{NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION, circuit::StmCircuit},
};

/// Constant representing the current STM parameters used for production,
/// i.e. parameters that guarantee the security of the protocol
const STM_PARAMETERS_FOR_PRODUCTION: Parameters = Parameters {
    m: 16948,
    k: 1944,
    phi_f: 0.2,
};

/// Function used to compute a non-recursive circuit verification key.
/// It uses the given parameters and a SRS to generate the circuit verification key.
/// It can generate the production verification key for the non-recursive circuit if
/// given the production parameters and Midnight SRS.
///
// TODO: this function is temporary and needs to be replaced with the function used to compute the verification key for the circuit cache.
fn compute_non_recursive_circuit_verification_key(
    params: &Parameters,
    merkle_tree_depth: u32,
    srs_path: &Path,
) -> StmResult<Vec<u8>> {
    let circuit = StmCircuit::try_new(params, merkle_tree_depth)?;

    // Change the path to the settled path for the SRS
    let srs_file =
        File::open(srs_path).with_context(|| "Failed to find the SRS file at the given path.")?;

    let mut reader = BufReader::new(srs_file);
    let srs = ParamsKZG::read_custom(&mut reader, SerdeFormat::RawBytes)
        .with_context(|| "Failed to deserialize the SRS.")?;

    let non_recursive_circuit_verification_key = midnight_zk_stdlib::setup_vk(&srs, &circuit);
    let mut buffer_for_non_recursive_circuit_verification_key = vec![];
    non_recursive_circuit_verification_key
        .write(
            &mut buffer_for_non_recursive_circuit_verification_key,
            SerdeFormat::RawBytes,
        )
        .with_context(
            || "Failed to write the non recursive circuit verification key to a buffer.",
        )?;
    Ok(buffer_for_non_recursive_circuit_verification_key)
}

/// Function that recomputes the non recursive production key and updates
/// the binary file storing its value
#[test]
#[ignore]
pub fn write_non_recursive_circuit_verification_key_for_production_to_file() {
    // This path might need to be updated depending on where the Midnight SRS is stored locally
    // TODO: update this path once the SRS location is fixed
    let srs_path = Path::new("../../midnight-srs-2p22");

    let verification_key = compute_non_recursive_circuit_verification_key(
        &STM_PARAMETERS_FOR_PRODUCTION,
        MERKLE_TREE_DEPTH_FOR_SNARK,
        srs_path,
    )
    .unwrap();

    std::fs::write(
        "src/circuits/halo2/non_recursive_circuit_verification_key_for_production.bin",
        verification_key,
    )
    .unwrap();
}

/// Function that recomputes the non recursive production key and checks its value
/// against the value used to compile the library
#[test]
#[ignore]
fn integrity_test_for_non_recursive_production_key() {
    // This path might need to be updated depending on where the Midnight SRS is stored locally
    // TODO: update this path once the SRS location is fixed
    let srs_path = Path::new("../../midnight-srs-2p22");

    let production_non_recursive_key = NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION;

    let recomputed_non_recursive_key = compute_non_recursive_circuit_verification_key(
        &STM_PARAMETERS_FOR_PRODUCTION,
        MERKLE_TREE_DEPTH_FOR_SNARK,
        srs_path,
    )
    .unwrap();

    assert_eq!(production_non_recursive_key, recomputed_non_recursive_key);
}
