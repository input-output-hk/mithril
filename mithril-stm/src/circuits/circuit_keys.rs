//! Module that handles the caching of circuit keys

use std::{
    fs::File,
    io::{BufReader, Read},
    path::PathBuf,
};

use midnight_proofs::utils::SerdeFormat;
use midnight_zk_stdlib::{self as zk, MidnightCircuit, MidnightPK, MidnightVK};

use crate::{
    MERKLE_TREE_DEPTH_FOR_SNARK, Parameters, StmResult,
    circuits::{
        MITHRIL_CIRCUIT_CACHE_FOLDER,
        halo2::{NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION, circuit::StmCircuit},
        trusted_setup::TrustedSetupProvider,
    },
};

pub const MITHRIL_CIRCUIT_PROVING_KEY_FILENAME: &str = "proving-key";
pub const MITHRIL_CIRCUIT_VERIFICATION_KEY_FILENAME: &str = "verification-key";

/// Structure that holds information on where to find and/or store the
/// circuit keys (proving and verification keys).
///
/// It contains a path to the folder containing the keys and expected hashes
/// of the keys to check for their correctness.
pub struct CircuitKeysProvider {
    local_keys_folder_path: PathBuf,
    verification_key_expected_hash: String,
    proving_key_expected_hash: String,
}

impl CircuitKeysProvider {
    fn new(
        local_keys_folder_path: PathBuf,
        verification_key_expected_hash: String,
        proving_key_expected_hash: String,
    ) -> Self {
        Self {
            local_keys_folder_path,
            verification_key_expected_hash,
            proving_key_expected_hash,
        }
    }

    /// Recompute the keys using the stored SRS and the current circuit
    fn recompute_keys(&self) -> StmResult<MidnightVK> {
        let srs = TrustedSetupProvider::default().get_trusted_setup_parameters()?;

        let params = Parameters {
            m: 16948,
            k: 1944,
            phi_f: 0.2,
        };

        let circuit = StmCircuit::try_new(&params, MERKLE_TREE_DEPTH_FOR_SNARK)?;

        let vk = zk::setup_vk(&srs, &circuit);
        Ok(vk)
    }

    fn compute_keys_if_not_cached(&self) -> StmResult<()> {
        if !self
            .local_keys_folder_path
            .join(MITHRIL_CIRCUIT_VERIFICATION_KEY_FILENAME)
            .exists()
        {
            let vk = self.recompute_keys();

            // let mut vk_buffer = vec![];
            // vk.write(&mut vk_buffer, SerdeFormat::RawBytes);

            // if vk_buffer == NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION {}
            // let pk = zk::setup_pk(&circuit, &vk);
        } else {
            let vk_file = File::open(
                self.local_keys_folder_path
                    .join(MITHRIL_CIRCUIT_VERIFICATION_KEY_FILENAME),
            )?;
            let mut vk_buffer = vec![];
            BufReader::new(vk_file).read_to_end(&mut vk_buffer)?;
            if vk_buffer != NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION {
                // Cache is stale, recompute and store again the vk
                let srs = TrustedSetupProvider::default().get_trusted_setup_parameters()?;

                let params = Parameters {
                    m: 16948,
                    k: 1944,
                    phi_f: 0.2,
                };

                let circuit = StmCircuit::try_new(&params, MERKLE_TREE_DEPTH_FOR_SNARK)?;

                let vk = zk::setup_vk(&srs, &circuit);
            }

            // vk.write(&mut vk_buffer, SerdeFormat::RawBytes);
        }

        Ok(())
    }
}
