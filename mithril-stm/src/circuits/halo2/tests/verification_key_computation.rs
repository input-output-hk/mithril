use std::{fs::File, io::BufReader, path::PathBuf};

use anyhow::Context;
use midnight_proofs::{poly::kzg::params::ParamsKZG, utils::SerdeFormat};

use crate::{
    MERKLE_TREE_DEPTH_FOR_SNARK, Parameters, StmResult, circuits::halo2::circuit::StmCircuit,
};

/// Function used to compute a non-recursive circuit verification key.
/// It uses the given parameters and a SRS to generate the circuit verification key.
/// It can generate the production verification key for the non-recursive circuit if
/// given the production parameters and Midnight SRS.
fn compute_non_recursive_circuit_verification_key(
    params: &Parameters,
    merkle_tree_depth: u32,
    srs_path: &PathBuf,
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

#[test]
#[ignore]
pub fn write_non_recursive_circuit_verification_key_for_production_to_file() {
    let srs_path = PathBuf::from("../../midnight-srs-2p22");

    let production_parameters = Parameters {
        m: 16948,
        k: 1944,
        phi_f: 0.2,
    };
    let merkle_tree_depth = MERKLE_TREE_DEPTH_FOR_SNARK;

    let verification_key = compute_non_recursive_circuit_verification_key(
        &production_parameters,
        merkle_tree_depth,
        &srs_path,
    )
    .unwrap();

    std::fs::write(
        "src/circuits/halo2/assets/non_recursive_circuit_verification_key_for_production.bin",
        verification_key,
    )
    .unwrap();
}
