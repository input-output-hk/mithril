//! Module that handles the caching of circuit keys

use std::{
    fs::File,
    io::{BufReader, Read, Write},
    path::PathBuf,
};

use anyhow::Context;
use midnight_proofs::utils::SerdeFormat;
use midnight_zk_stdlib::{self as zk, MidnightCircuit, MidnightPK, MidnightVK, Relation};
use sha2::{Digest, Sha256};

use crate::{
    MERKLE_TREE_DEPTH_FOR_SNARK, Parameters, StmResult,
    circuits::{
        MITHRIL_CIRCUIT_CACHE_FOLDER,
        halo2::{NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION, circuit::StmCircuit},
        halo2_ivc::circuit::IvcCircuit,
        trusted_setup::TrustedSetupProvider,
    },
};

pub const MITHRIL_CIRCUIT_KEYS_FOLDER: &str = "circuit_keys";

pub const MITHRIL_CIRCUIT_PROVING_KEY_FILENAME: &str = "proving-key";
pub const MITHRIL_CIRCUIT_VERIFICATION_KEY_FILENAME: &str = "verification-key";

pub const NON_RECURSIVE_CIRCUIT_PROVING_KEY_FOR_PRODUCTION_HASH: &str = "";

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

pub struct CircuitVerificationKeyProvider {
    local_keys_folder_path: PathBuf,
}

impl CircuitVerificationKeyProvider {
    pub fn new(local_keys_folder_path: PathBuf) -> Self {
        Self {
            local_keys_folder_path: local_keys_folder_path.join(MITHRIL_CIRCUIT_KEYS_FOLDER),
        }
    }

    /// Recompute the keys using the stored SRS and the current circuit
    fn recompute_verification_key(&self) -> StmResult<MidnightVK> {
        let srs = TrustedSetupProvider::default().get_trusted_setup_parameters()?;

        let params = Parameters {
            m: 16948,
            k: 1944,
            phi_f: 0.2,
        };

        let circuit = StmCircuit::try_new(&params, MERKLE_TREE_DEPTH_FOR_SNARK)?;

        Ok(zk::setup_vk(&srs, &circuit))
    }

    // For now just vk but should also be pk
    fn remove_stale_cache_values(&self) -> StmResult<()> {
        if self
            .local_keys_folder_path
            .join(MITHRIL_CIRCUIT_PROVING_KEY_FILENAME)
            .exists()
        {
            std::fs::remove_file(
                self.local_keys_folder_path.join(MITHRIL_CIRCUIT_PROVING_KEY_FILENAME),
            )
            .with_context(|| "Failed to remove the proving key file.")?;
        }

        std::fs::remove_file(
            self.local_keys_folder_path
                .join(MITHRIL_CIRCUIT_VERIFICATION_KEY_FILENAME),
        )
        .with_context(|| "Failed to remove the verification key file.")
    }

    // Is the order of operation correct, when should the deserialization be?
    fn store_verification_key_to_file(&self, vk: MidnightVK) -> StmResult<()> {
        let mut vk_bytes = vec![];
        vk.write(&mut vk_bytes, SerdeFormat::RawBytes)
            .with_context(|| "Writing the bytes to the buffer should have succeeded.")?;

        std::fs::create_dir_all(&self.local_keys_folder_path)
            .with_context(|| "Subdirectory creation should have succeeded.")?;

        let final_path = self
            .local_keys_folder_path
            .join(MITHRIL_CIRCUIT_VERIFICATION_KEY_FILENAME);

        store_using_temp_file(&vk_bytes, final_path)?;

        File::open(&self.local_keys_folder_path)
            .and_then(|dir| dir.sync_all())
            .with_context(|| "Failed to fsync directory after rename.")?;
        Ok(())
    }

    // Should probably have a flag to trigger recomputation and storing
    //
    fn compute_verification_key_if_not_cached(&self) -> StmResult<()> {
        let mut empty_cache = false;
        if !self
            .local_keys_folder_path
            .join(MITHRIL_CIRCUIT_VERIFICATION_KEY_FILENAME)
            .exists()
        {
            empty_cache = true;
        } else {
            let vk_file = File::open(
                self.local_keys_folder_path
                    .join(MITHRIL_CIRCUIT_VERIFICATION_KEY_FILENAME),
            )?;
            let mut vk_buffer = vec![];
            BufReader::new(vk_file).read_to_end(&mut vk_buffer)?;

            if vk_buffer != NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION {
                empty_cache = true;
                self.remove_stale_cache_values()?;
            }
        }

        if empty_cache {
            // Cache is stale, recompute and store again the vk
            let vk = self.recompute_verification_key()?;
            // ... and storing recomputed version
            self.store_verification_key_to_file(vk)?;
        }

        Ok(())
    }

    pub fn get_circuit_verification_key(&self) -> StmResult<MidnightVK> {
        self.compute_verification_key_if_not_cached()?;

        let file = File::open(
            self.local_keys_folder_path
                .join(MITHRIL_CIRCUIT_VERIFICATION_KEY_FILENAME),
        )
        .with_context(|| {
            format!(
                "Failed to open verification key file at {:?}.",
                self.local_keys_folder_path
                    .join(MITHRIL_CIRCUIT_VERIFICATION_KEY_FILENAME)
            )
        })?;
        let mut reader = BufReader::new(file);

        MidnightVK::read(&mut reader, SerdeFormat::RawBytes)
            .with_context(|| "Failed to deserialize verification key from file.")
    }
}

impl Default for CircuitVerificationKeyProvider {
    fn default() -> Self {
        Self::new(std::env::temp_dir().join(MITHRIL_CIRCUIT_CACHE_FOLDER))
    }
}

pub struct CircuitProvingKeyProvider {
    verification_key_provider: CircuitVerificationKeyProvider,
    local_keys_folder_path: PathBuf,
    proving_key_expected_hash: String,
}

impl CircuitProvingKeyProvider {
    pub fn new(
        verification_key_provider: CircuitVerificationKeyProvider,
        local_keys_folder_path: PathBuf,
        proving_key_expected_hash: String,
    ) -> Self {
        Self {
            verification_key_provider,
            local_keys_folder_path: local_keys_folder_path.join(MITHRIL_CIRCUIT_KEYS_FOLDER),
            proving_key_expected_hash,
        }
    }

    /// Recompute the keys using the stored SRS and the current circuit
    fn recompute_proving_key(&self) -> StmResult<MidnightPK<StmCircuit>> {
        let params = Parameters {
            m: 16948,
            k: 1944,
            phi_f: 0.2,
        };

        let circuit = StmCircuit::try_new(&params, MERKLE_TREE_DEPTH_FOR_SNARK)?;
        let verification_key = self.verification_key_provider.get_circuit_verification_key()?;

        Ok(zk::setup_pk(&circuit, &verification_key))
    }

    // For now just vk but should also be pk
    fn remove_stale_cache_values(&self) -> StmResult<()> {
        std::fs::remove_file(
            self.local_keys_folder_path
                .join(MITHRIL_CIRCUIT_VERIFICATION_KEY_FILENAME),
        )
        .with_context(|| "Failed to remove the verification key file.")?;
        std::fs::remove_file(self.local_keys_folder_path.join(MITHRIL_CIRCUIT_PROVING_KEY_FILENAME))
            .with_context(|| "Failed to remove the proving key file.")
    }

    // Is the order of operation correct, when should the deserialization be?
    fn store_proving_key_to_file(&self, proving_key: MidnightPK<StmCircuit>) -> StmResult<()> {
        let mut proving_key_bytes = vec![];
        proving_key
            .write(&mut proving_key_bytes, SerdeFormat::RawBytes)
            .with_context(|| "Writing the bytes to the buffer should have succeeded.")?;

        std::fs::create_dir_all(&self.local_keys_folder_path)
            .with_context(|| "Subdirectory creation should have succeeded.")?;

        let final_path = self.local_keys_folder_path.join(MITHRIL_CIRCUIT_PROVING_KEY_FILENAME);

        store_using_temp_file(&proving_key_bytes, final_path)?;

        File::open(&self.local_keys_folder_path)
            .and_then(|dir| dir.sync_all())
            .with_context(|| "Failed to fsync directory after rename.")?;
        Ok(())
    }

    // Should probably have a flag to trigger recomputation and storing
    //
    fn compute_proving_key_if_not_cached(&self) -> StmResult<()> {
        let mut empty_cache = false;
        if !self
            .local_keys_folder_path
            .join(MITHRIL_CIRCUIT_PROVING_KEY_FILENAME)
            .exists()
        {
            empty_cache = true;
        } else {
            let proving_key_file =
                File::open(self.local_keys_folder_path.join(MITHRIL_CIRCUIT_PROVING_KEY_FILENAME))?;
            let mut proving_key_buffer = vec![];
            BufReader::new(proving_key_file).read_to_end(&mut proving_key_buffer)?;

            let mut hasher = Sha256::new();
            hasher.update(proving_key_buffer);

            let proving_key_hash_value = hex::encode(hasher.finalize());

            if proving_key_hash_value != NON_RECURSIVE_CIRCUIT_PROVING_KEY_FOR_PRODUCTION_HASH {
                empty_cache = true;
                self.remove_stale_cache_values()?;
            }
        }

        if empty_cache {
            // Cache is stale, recompute and store again the proving key
            let proving_key = self.recompute_proving_key()?;
            // ... and storing recomputed version
            self.store_proving_key_to_file(proving_key)?;
        }

        Ok(())
    }

    pub fn get_circuit_proving_key(&self) -> StmResult<MidnightPK<StmCircuit>> {
        self.compute_proving_key_if_not_cached()?;

        let file =
            File::open(self.local_keys_folder_path.join(MITHRIL_CIRCUIT_PROVING_KEY_FILENAME))
                .with_context(|| {
                    format!(
                        "Failed to open proving key file at {:?}.",
                        self.local_keys_folder_path.join(MITHRIL_CIRCUIT_PROVING_KEY_FILENAME)
                    )
                })?;
        let mut reader = BufReader::new(file);

        MidnightPK::read(&mut reader, SerdeFormat::RawBytes)
            .with_context(|| "Failed to deserialize proving key from file.")
    }
}

fn store_using_temp_file(bytes: &[u8], file_path: PathBuf) -> StmResult<()> {
    let temp_path = file_path.with_extension("temp");
    let mut temporary_file = File::create(&temp_path)
        .with_context(|| format!("Failed to create temporary file at {temp_path:?}."))?;
    temporary_file.write_all(&bytes)?;
    temporary_file
        .sync_all()
        .with_context(|| "Failed to fsync temporary file before rename.")?;
    drop(temporary_file);

    std::fs::rename(temp_path, file_path).with_context(|| "Failed to rename the file.")
}
