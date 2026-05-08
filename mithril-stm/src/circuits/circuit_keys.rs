//! Module that handles the caching of circuit keys

use std::{
    fs::File,
    io::{BufReader, Read, Write},
    path::PathBuf,
};

use anyhow::Context;
use midnight_proofs::utils::SerdeFormat;
use midnight_zk_stdlib::{self as zk, MidnightPK, MidnightVK};
use sha2::{Digest, Sha256};

use crate::{
    MERKLE_TREE_DEPTH_FOR_SNARK, Parameters, StmResult,
    circuits::{
        MITHRIL_CIRCUIT_CACHE_FOLDER,
        halo2::{NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION, circuit::StmCircuit},
        trusted_setup::TrustedSetupProvider,
    },
};

/// Constant holding the default folder of the circuit keys
pub const MITHRIL_CIRCUIT_KEYS_FOLDER: &str = "circuit_keys";
/// Constant holding the default filename of the circuit proving key
pub const MITHRIL_CIRCUIT_PROVING_KEY_FILENAME: &str = "proving-key";
/// Constant holding the default filename of the circuit verification key
pub const MITHRIL_CIRCUIT_VERIFICATION_KEY_FILENAME: &str = "verification-key";
/// Constant holding the hash value of the current circuit proving key
// TODO: complete this value
pub const NON_RECURSIVE_CIRCUIT_PROVING_KEY_FOR_PRODUCTION_HASH: &str = "";

/// Errors which can be outputted by the circuit keys verification.
#[derive(Debug, Clone, thiserror::Error, PartialEq, Eq)]
pub enum CircuitKeysVerificationError {
    /// The hash verification of the proving key bytes failed
    #[error(
        "The hash of the proving key file does not match the hard-coded value. Expected: {expected}, Computed hash: {computed}"
    )]
    VerifyHashFail { expected: String, computed: String },
    /// The recomputation of the verification key failed to match the expected bytes
    #[error("The recomputation of the verification key does not match the expected bytes.")]
    VerificationKeyRecomputationFailed,
}

/// Structure that holds information on where to find and/or store the
/// circuit verification key.
///
/// It contains the path to the folder containing the keys
pub struct CircuitVerificationKeyProvider {
    local_keys_folder_path: PathBuf,
}

impl CircuitVerificationKeyProvider {
    /// Create a new structure based on the given path
    pub fn new(local_keys_folder_path: PathBuf) -> Self {
        Self {
            local_keys_folder_path: local_keys_folder_path.join(MITHRIL_CIRCUIT_KEYS_FOLDER),
        }
    }

    /// Loads the bytes of the verification key stored in the current file
    fn load_verification_key_bytes_from_file(&self) -> StmResult<Vec<u8>> {
        let verification_key_file = File::open(
            self.local_keys_folder_path
                .join(MITHRIL_CIRCUIT_VERIFICATION_KEY_FILENAME),
        )?;
        let mut verification_key_buffer = vec![];
        BufReader::new(verification_key_file).read_to_end(&mut verification_key_buffer)?;
        Ok(verification_key_buffer)
    }

    /// Recompute the circuit verification key using the default SRS and the current circuit
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

    /// Removes the proving key file if it exists and the verification key file as the cache
    /// was identified as stale
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

    /// Stores the given circuit verification key in stored path
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

    /// Checks if the circuit verification key is present or stale and
    /// recomputes and stores the circuit verification key if necessary
    fn compute_verification_key_if_not_cached(&self) -> StmResult<()> {
        let mut empty_cache = false;
        if !self
            .local_keys_folder_path
            .join(MITHRIL_CIRCUIT_VERIFICATION_KEY_FILENAME)
            .exists()
        {
            empty_cache = true;
        } else {
            let vk_bytes = self.load_verification_key_bytes_from_file()?;
            if vk_bytes != NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION {
                empty_cache = true;
                self.remove_stale_cache_values()?;
            }
        }

        if empty_cache {
            // Cache is stale, recompute and store again the vk
            let vk = self.recompute_verification_key()?;

            let vk_bytes = self.load_verification_key_bytes_from_file()?;
            if vk_bytes != NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION {
                self.store_verification_key_to_file(vk)?;
            } else {
                return Err(
                    CircuitKeysVerificationError::VerificationKeyRecomputationFailed.into(),
                );
            }
        }

        Ok(())
    }

    /// Verifies the circuit verification key file is present and creates and returns
    /// the circuit verification key
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

/// Structure that holds information on where to find and/or store the
/// circuit proving key.
///
/// It contains the path to the folder containing the keys, the expected hash of
/// the proving key and a provider of the circuit verification key
pub struct CircuitProvingKeyProvider {
    local_keys_folder_path: PathBuf,
    proving_key_expected_hash: String,
    verification_key_provider: CircuitVerificationKeyProvider,
}

impl CircuitProvingKeyProvider {
    /// Creates a new structure based on the given path, the expected hash and
    /// a circuit verification key provider
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

    /// Computes the proving key hash of the given bytes and returns its hex encoding.
    fn compute_hash(bytes: &[u8]) -> String {
        let mut hasher = Sha256::new();
        hasher.update(bytes);

        hex::encode(hasher.finalize())
    }

    /// Checks SHA256 hash of the given bytes against the stored expected value.
    fn verify_proving_key_bytes_sha256_hash(&self, srs_bytes: &[u8]) -> StmResult<()> {
        let recomputed_hash = Self::compute_hash(srs_bytes);

        if self.proving_key_expected_hash != recomputed_hash {
            return Err(CircuitKeysVerificationError::VerifyHashFail {
                expected: self.proving_key_expected_hash.clone(),
                computed: recomputed_hash,
            }
            .into());
        }
        Ok(())
    }

    /// Loads the bytes of the proving key stored in the current file
    fn load_proving_key_bytes_from_file(&self) -> StmResult<Vec<u8>> {
        let proving_key_file =
            File::open(self.local_keys_folder_path.join(MITHRIL_CIRCUIT_PROVING_KEY_FILENAME))?;
        let mut proving_key_buffer = vec![];
        BufReader::new(proving_key_file).read_to_end(&mut proving_key_buffer)?;
        Ok(proving_key_buffer)
    }

    /// Recomputes the circuit proving key using the current circuit and the stored
    /// circuit verification key
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

    /// Removes the proving key file and the verification key file as the cache
    /// was identified as stale
    fn remove_stale_cache_values(&self) -> StmResult<()> {
        std::fs::remove_file(
            self.local_keys_folder_path
                .join(MITHRIL_CIRCUIT_VERIFICATION_KEY_FILENAME),
        )
        .with_context(|| "Failed to remove the verification key file.")?;
        std::fs::remove_file(self.local_keys_folder_path.join(MITHRIL_CIRCUIT_PROVING_KEY_FILENAME))
            .with_context(|| "Failed to remove the proving key file.")
    }

    /// Stores the given circuit proving key in stored path
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

    /// Checks if the circuit proving key is present or stale and
    /// recomputes and stores the circuit proving key if necessary
    // TODO: decide if this function should also recompute the verification
    // key when the cache is stale
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

            // TODO: maybe move this to an independant function
            let mut hasher = Sha256::new();
            hasher.update(proving_key_buffer);
            let proving_key_hash_value = hex::encode(hasher.finalize());

            if proving_key_hash_value != NON_RECURSIVE_CIRCUIT_PROVING_KEY_FOR_PRODUCTION_HASH {
                empty_cache = true;
                self.remove_stale_cache_values()?;
            }
        }

        if empty_cache {
            let proving_key = self.recompute_proving_key()?;

            let proving_key_file =
                File::open(self.local_keys_folder_path.join(MITHRIL_CIRCUIT_PROVING_KEY_FILENAME))?;
            let mut proving_key_buffer = vec![];
            BufReader::new(proving_key_file).read_to_end(&mut proving_key_buffer)?;

            self.verify_proving_key_bytes_sha256_hash(&proving_key_buffer)?;
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
