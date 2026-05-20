//! `IvcRollingState`: caller-owned bridge between consecutive IVC proving steps.

use std::{
    fs::File,
    io::{BufReader, BufWriter, Read, Write},
    path::Path,
};

use anyhow::Context;
use midnight_circuits::verifier::{Accumulator, BlstrsEmulation};
use midnight_proofs::utils::SerdeFormat;

use crate::{
    StmResult,
    circuits::halo2_ivc::{
        io::{Read as IvcRead, Write as IvcWrite},
        state::{State, trivial_acc},
    },
    signature_scheme::StandardSchnorrSignature,
};

/// Caller-owned bridge between consecutive IVC proving steps.
// TODO: remove this allow dead_code directive when the IVC prover consumes this rolling state
#[allow(dead_code)]
#[derive(Debug)]
pub(crate) struct IvcRollingState {
    /// Last committed chain state.
    state: State,
    /// Bytes of the last IVC proof under the Poseidon transcript
    ivc_proof: Vec<u8>,
    /// Folded accumulator the new step will build on top of
    accumulator: Accumulator<BlstrsEmulation>,
    /// Chain-specific Schnorr signature over the genesis state
    genesis_signature: StandardSchnorrSignature,
}

#[allow(dead_code)]
impl IvcRollingState {
    /// Builds a rolling state from the four fields produced by an IVC proving step.
    pub(crate) fn new(
        state: State,
        ivc_proof: Vec<u8>,
        accumulator: Accumulator<BlstrsEmulation>,
        genesis_signature: StandardSchnorrSignature,
    ) -> Self {
        Self {
            state,
            ivc_proof,
            accumulator,
            genesis_signature,
        }
    }

    /// Returns the last committed chain state.
    pub(crate) fn state(&self) -> &State {
        &self.state
    }

    /// Returns the bytes of the last IVC proof.
    pub(crate) fn ivc_proof(&self) -> &[u8] {
        &self.ivc_proof
    }

    /// Returns the folded accumulator the new step will build on top of.
    pub(crate) fn accumulator(&self) -> &Accumulator<BlstrsEmulation> {
        &self.accumulator
    }

    /// Returns the chain-specific Schnorr signature over the genesis state.
    pub(crate) fn genesis_signature(&self) -> StandardSchnorrSignature {
        self.genesis_signature
    }

    /// Loads the rolling state from `path` if it exists, otherwise bootstraps a
    /// genesis rolling state from the caller-supplied genesis signature and the
    /// fixed-base names of the combined accumulator map.
    pub(crate) fn load_or_genesis(
        path: &Path,
        genesis_signature: StandardSchnorrSignature,
        fixed_base_names: &[String],
    ) -> StmResult<Self> {
        if path.exists() {
            Self::read_from_disk(path)
        } else {
            let rolling_state = Self::genesis(genesis_signature, fixed_base_names);
            rolling_state.save(path)?;
            Ok(rolling_state)
        }
    }

    /// Persists the rolling state to `path`, overwriting any existing file.
    pub(crate) fn save(&self, path: &Path) -> StmResult<()> {
        let file = File::create(path)
            .with_context(|| format!("Failed to create rolling state file at {path:?}"))?;
        let mut writer = BufWriter::new(file);

        IvcWrite::write(&self.state, &mut writer, SerdeFormat::RawBytes)
            .with_context(|| "Failed to write state to rolling state file")?;

        writer
            .write_all(&(self.ivc_proof.len() as u32).to_le_bytes())
            .with_context(|| "Failed to write IVC proof length to rolling state file")?;
        writer
            .write_all(&self.ivc_proof)
            .with_context(|| "Failed to write IVC proof bytes to rolling state file")?;

        IvcWrite::write(&self.accumulator, &mut writer, SerdeFormat::RawBytes)
            .with_context(|| "Failed to write accumulator to rolling state file")?;

        IvcWrite::write(&self.genesis_signature, &mut writer, SerdeFormat::RawBytes)
            .with_context(|| "Failed to write genesis signature to rolling state file")?;

        writer.flush().with_context(|| "Failed to flush rolling state file")?;
        Ok(())
    }

    /// Builds the genesis rolling state: zeroed chain state, empty IVC proof,
    /// trivial accumulator over the supplied fixed-base names, and the
    /// caller-supplied genesis signature.
    fn genesis(genesis_signature: StandardSchnorrSignature, fixed_base_names: &[String]) -> Self {
        Self {
            state: State::genesis(),
            ivc_proof: Vec::new(),
            accumulator: trivial_acc(fixed_base_names),
            genesis_signature,
        }
    }

    /// Reads a rolling state from `path`, in the same field order that `save`
    /// writes: chain state, IVC proof length and bytes, accumulator, genesis
    /// signature.
    fn read_from_disk(path: &Path) -> StmResult<Self> {
        let file = File::open(path)
            .with_context(|| format!("Failed to open rolling state file at {path:?}"))?;
        let mut reader = BufReader::new(file);

        let state = <State as IvcRead>::read(&mut reader, SerdeFormat::RawBytes)
            .with_context(|| "Failed to read state from rolling state file")?;

        let mut len_bytes = [0u8; 4];
        reader
            .read_exact(&mut len_bytes)
            .with_context(|| "Failed to read IVC proof length from rolling state file")?;
        let ivc_proof_len = u32::from_le_bytes(len_bytes) as usize;
        let mut ivc_proof = vec![0u8; ivc_proof_len];
        reader
            .read_exact(&mut ivc_proof)
            .with_context(|| "Failed to read IVC proof bytes from rolling state file")?;

        let accumulator =
            <Accumulator<BlstrsEmulation> as IvcRead>::read(&mut reader, SerdeFormat::RawBytes)
                .with_context(|| "Failed to read accumulator from rolling state file")?;

        let genesis_signature =
            <StandardSchnorrSignature as IvcRead>::read(&mut reader, SerdeFormat::RawBytes)
                .with_context(|| "Failed to read genesis signature from rolling state file")?;

        Ok(Self {
            state,
            ivc_proof,
            accumulator,
            genesis_signature,
        })
    }
}

#[cfg(test)]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;
    use tempfile::tempdir;

    use crate::signature_scheme::{BaseFieldElement, SchnorrSigningKey};

    use super::*;

    fn build_genesis_signature() -> StandardSchnorrSignature {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let signing_key = SchnorrSigningKey::generate(&mut rng);
        let message = vec![BaseFieldElement::from(1u64)];
        signing_key.sign_standard(&message, &mut rng).unwrap()
    }

    #[test]
    fn save_load_roundtrip() {
        let temp_dir = tempdir().unwrap();
        let path = temp_dir.path().join("rolling_state");

        let genesis_signature = build_genesis_signature();
        let fixed_base_names = vec!["base_one".to_string(), "base_two".to_string()];

        let bootstrapped =
            IvcRollingState::load_or_genesis(&path, genesis_signature, &fixed_base_names).unwrap();

        assert!(path.exists(), "Genesis bootstrap should persist the file");
        assert!(bootstrapped.ivc_proof.is_empty());
        assert_eq!(bootstrapped.genesis_signature, genesis_signature);

        let loaded =
            IvcRollingState::load_or_genesis(&path, genesis_signature, &fixed_base_names).unwrap();

        assert_eq!(loaded.genesis_signature, genesis_signature);
        assert!(loaded.ivc_proof.is_empty());

        let resave_path = temp_dir.path().join("rolling_state_resaved");
        loaded.save(&resave_path).unwrap();

        let original_bytes = std::fs::read(&path).unwrap();
        let resaved_bytes = std::fs::read(&resave_path).unwrap();
        assert_eq!(
            original_bytes, resaved_bytes,
            "Save/load roundtrip should produce byte-identical files"
        );
    }

    #[test]
    fn load_corrupted_file_returns_read_error() {
        let temp_dir = tempdir().unwrap();
        let path = temp_dir.path().join("rolling_state");

        std::fs::write(&path, b"not a valid rolling state").unwrap();

        let genesis_signature = build_genesis_signature();
        let fixed_base_names = vec!["base_one".to_string()];

        IvcRollingState::load_or_genesis(&path, genesis_signature, &fixed_base_names)
            .expect_err("Loading a corrupted file should fail while reading the rolling state");
    }

    #[test]
    fn save_load_roundtrip_with_non_empty_ivc_proof() {
        let temp_dir = tempdir().unwrap();
        let path = temp_dir.path().join("rolling_state");

        let genesis_signature = build_genesis_signature();
        let fixed_base_names = vec!["base_one".to_string()];

        let ivc_proof_bytes = vec![0xDE, 0xAD, 0xBE, 0xEF, 0x01, 0x02, 0x03];
        let rolling_state = IvcRollingState::new(
            State::genesis(),
            ivc_proof_bytes.clone(),
            trivial_acc(&fixed_base_names),
            genesis_signature,
        );
        rolling_state.save(&path).unwrap();

        let loaded =
            IvcRollingState::load_or_genesis(&path, genesis_signature, &fixed_base_names).unwrap();

        assert_eq!(loaded.ivc_proof(), ivc_proof_bytes.as_slice());
        assert_eq!(loaded.genesis_signature(), genesis_signature);
    }

    #[test]
    fn load_truncated_file_returns_read_error() {
        let temp_dir = tempdir().unwrap();
        let path = temp_dir.path().join("rolling_state");

        let genesis_signature = build_genesis_signature();
        let fixed_base_names = vec!["base_one".to_string()];

        IvcRollingState::load_or_genesis(&path, genesis_signature, &fixed_base_names).unwrap();

        std::fs::OpenOptions::new()
            .write(true)
            .open(&path)
            .unwrap()
            .set_len(10)
            .unwrap();

        IvcRollingState::load_or_genesis(&path, genesis_signature, &fixed_base_names)
            .expect_err("Loading a truncated file should fail while reading the rolling state");
    }

    #[test]
    fn save_to_nonexistent_directory_returns_create_error() {
        let temp_dir = tempdir().unwrap();
        let path = temp_dir.path().join("missing_dir").join("rolling_state");

        let genesis_signature = build_genesis_signature();
        let fixed_base_names = vec!["base_one".to_string()];

        let rolling_state = IvcRollingState::new(
            State::genesis(),
            Vec::new(),
            trivial_acc(&fixed_base_names),
            genesis_signature,
        );

        rolling_state
            .save(&path)
            .expect_err("Saving to a missing parent dir should fail while creating the file");
    }
}
