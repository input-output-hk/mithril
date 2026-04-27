use std::{
    collections::BTreeMap,
    fs::{self, File},
    io::{BufReader, BufWriter, Cursor, Read, Write},
    path::Path,
};

use anyhow::{Context, anyhow};
use midnight_curves::pairing::Engine;
use midnight_proofs::{
    poly::kzg::params::ParamsVerifierKZG,
    utils::{SerdeFormat, helpers::ProcessedSerdeObject},
};

use crate::StmResult;
use crate::circuits::halo2_ivc::{
    Accumulator, C, E, F, KZGCommitmentScheme, S, VerifyingKey,
    circuit::IvcCircuit,
    helpers::utils::jubjub_base_from_le_bytes,
    io::{Read as IvcRead, Write as IvcWrite},
    state::State,
};

/// Stored recursive chain checkpoint used by the golden tests.
#[derive(Debug)]
pub(crate) struct RecursiveChainStateAsset {
    /// Stored global public inputs for the recursive flow.
    pub(crate) global_field_elements: Vec<F>,
    /// Stored recursive state checkpoint.
    pub(crate) state: State,
    /// Stored previous recursive proof bytes.
    pub(crate) proof: Vec<u8>,
    /// Stored folded accumulator for the checkpoint.
    pub(crate) accumulator: Accumulator<S>,
}

/// Stored verifier-side context shared by the golden assets.
#[derive(Debug)]
pub(crate) struct VerificationContextAsset {
    /// Shared global public inputs used by the committed assets.
    pub(crate) global_field_elements: Vec<F>,
    /// Stored recursive verifying key.
    pub(crate) recursive_verifying_key: VerifyingKey<F, KZGCommitmentScheme<E>>,
    /// Combined fixed bases needed to check recursive accumulators.
    pub(crate) combined_fixed_bases: BTreeMap<String, C>,
    /// Shared verifier-side KZG parameters.
    pub(crate) verifier_params: ParamsVerifierKZG<E>,
    /// The verifier-side `s_g2` element extracted from the stored params.
    pub(crate) verifier_tau_in_g2: <E as Engine>::G2Affine,
}

/// Stored output of extending the recursive chain by one more step.
#[derive(Debug)]
pub(crate) struct RecursiveStepOutputAsset {
    /// Stored final recursive proof bytes for the next step.
    pub(crate) proof: Vec<u8>,
    /// Stored folded accumulator after extending the chain by one step.
    pub(crate) next_accumulator: Accumulator<S>,
    /// Stored next recursive state.
    pub(crate) next_state: State,
    /// Stored certificate proof consumed by the recursive step.
    pub(crate) certificate_proof: Vec<u8>,
}

const RECURSIVE_CHAIN_STATE_ASSET_BYTES: &[u8] =
    include_bytes!("../assets/recursive_chain_state.bin");
const VERIFICATION_CONTEXT_ASSET_BYTES: &[u8] =
    include_bytes!("../assets/verification_context.bin");
const RECURSIVE_STEP_OUTPUT_ASSET_BYTES: &[u8] =
    include_bytes!("../assets/recursive_step_output.bin");
const GENESIS_STEP_OUTPUT_ASSET_BYTES: &[u8] = include_bytes!("../assets/genesis_step_output.bin");
const SAME_EPOCH_STEP_OUTPUT_ASSET_BYTES: &[u8] =
    include_bytes!("../assets/same_epoch_step_output.bin");

/// Opens a committed golden asset for buffered reading.
fn open_asset_file(path: &Path) -> StmResult<BufReader<File>> {
    Ok(BufReader::new(File::open(path)?))
}

/// Creates a golden asset file and its parent directory if needed.
fn create_asset_file(path: &Path) -> StmResult<BufWriter<File>> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    Ok(BufWriter::new(File::create(path)?))
}

/// Reads one field element encoded as 32 little-endian bytes.
fn read_field_element<R: Read>(reader: &mut R) -> StmResult<F> {
    let mut bytes = [0u8; 32];
    reader.read_exact(&mut bytes)?;
    Ok(jubjub_base_from_le_bytes(&bytes))
}

/// Writes one field element as 32 little-endian bytes.
fn write_field_element<W: Write>(writer: &mut W, value: &F) -> StmResult<()> {
    writer.write_all(&value.to_bytes_le())?;
    Ok(())
}

/// Reads the seven public-input field elements that define a recursive state.
fn read_state_public_input<R: Read>(reader: &mut R) -> StmResult<State> {
    Ok(State::new(
        read_field_element(reader)?,
        read_field_element(reader)?,
        read_field_element(reader)?,
        read_field_element(reader)?,
        read_field_element(reader)?,
        read_field_element(reader)?,
        read_field_element(reader)?,
    ))
}

/// Writes the seven public-input field elements of a recursive state.
fn write_state_public_input<W: Write>(writer: &mut W, state: &State) -> StmResult<()> {
    for value in state.as_public_input() {
        write_field_element(writer, &value)?;
    }
    Ok(())
}

/// Reads proof bytes stored behind a 32-bit little-endian length prefix.
fn read_length_prefixed_proof<R: Read>(reader: &mut R) -> StmResult<Vec<u8>> {
    let mut len = [0u8; 4];
    reader.read_exact(&mut len)?;
    let proof_len = u32::from_le_bytes(len) as usize;

    let mut proof = vec![0u8; proof_len];
    reader.read_exact(&mut proof)?;

    Ok(proof)
}

/// Writes proof bytes with a 32-bit little-endian length prefix.
fn write_length_prefixed_proof<W: Write>(writer: &mut W, proof: &[u8]) -> StmResult<()> {
    writer.write_all(&(proof.len() as u32).to_le_bytes())?;
    writer.write_all(proof)?;
    Ok(())
}

/// Reads the named fixed-base map stored in the verification-context asset.
fn read_named_fixed_bases<R: Read>(reader: &mut R) -> StmResult<BTreeMap<String, C>> {
    let mut count = [0u8; 4];
    reader.read_exact(&mut count)?;
    let count = u32::from_le_bytes(count) as usize;

    let mut map = BTreeMap::new();
    for _ in 0..count {
        let mut name_len = [0u8; 4];
        reader.read_exact(&mut name_len)?;
        let name_len = u32::from_le_bytes(name_len) as usize;

        let mut name_bytes = vec![0u8; name_len];
        reader.read_exact(&mut name_bytes)?;
        let name = String::from_utf8(name_bytes)
            .map_err(|_| anyhow!("invalid UTF-8 key in verification-context fixed-base map"))?;

        let point = C::read(reader, SerdeFormat::RawBytesUnchecked)?;
        map.insert(name, point);
    }

    Ok(map)
}

/// Writes the named fixed-base map stored in the verification-context asset.
fn write_named_fixed_bases<W: Write>(
    writer: &mut W,
    fixed_bases: &BTreeMap<String, C>,
) -> StmResult<()> {
    writer.write_all(&(fixed_bases.len() as u32).to_le_bytes())?;
    for (name, point) in fixed_bases {
        let name_bytes = name.as_bytes();
        writer.write_all(&(name_bytes.len() as u32).to_le_bytes())?;
        writer.write_all(name_bytes)?;
        point.write(writer, SerdeFormat::RawBytesUnchecked)?;
    }
    Ok(())
}

/// Loads a recursive chain snapshot from the committed binary asset layout.
fn load_recursive_chain_state_asset_from_reader<R: Read>(
    reader: &mut R,
) -> StmResult<RecursiveChainStateAsset> {
    let global_field_elements = (0..5)
        .map(|_| read_field_element(reader))
        .collect::<Result<Vec<_>, _>>()?;
    let state = read_state_public_input(reader)?;
    let proof = read_length_prefixed_proof(reader)?;
    let accumulator = Accumulator::<S>::read(reader, SerdeFormat::RawBytesUnchecked)?;

    Ok(RecursiveChainStateAsset {
        global_field_elements,
        state,
        proof,
        accumulator,
    })
}

/// Loads the stored recursive chain snapshot from a committed asset file.
pub(crate) fn load_recursive_chain_state_asset(path: &Path) -> StmResult<RecursiveChainStateAsset> {
    let mut reader = open_asset_file(path).with_context(|| {
        format!(
            "failed to open recursive chain state asset: {}",
            path.display()
        )
    })?;
    load_recursive_chain_state_asset_from_reader(&mut reader).with_context(|| {
        format!(
            "failed to decode recursive chain state asset: {}",
            path.display()
        )
    })
}

/// Loads the embedded recursive chain snapshot compiled into the test binary.
pub(crate) fn load_embedded_recursive_chain_state_asset() -> StmResult<RecursiveChainStateAsset> {
    let mut reader = Cursor::new(RECURSIVE_CHAIN_STATE_ASSET_BYTES);
    load_recursive_chain_state_asset_from_reader(&mut reader)
        .context("failed to decode embedded recursive chain state asset")
}

/// Writes the recursive chain state asset using the committed binary layout.
pub(crate) fn store_recursive_chain_state_asset(
    path: &Path,
    asset: &RecursiveChainStateAsset,
) -> StmResult<()> {
    let mut writer = create_asset_file(path).with_context(|| {
        format!(
            "failed to create recursive chain state asset file: {}",
            path.display()
        )
    })?;

    for value in &asset.global_field_elements {
        write_field_element(&mut writer, value)?;
    }
    write_state_public_input(&mut writer, &asset.state)?;
    write_length_prefixed_proof(&mut writer, &asset.proof)?;
    asset.accumulator.write(&mut writer, SerdeFormat::RawBytesUnchecked)?;
    writer.flush().with_context(|| {
        format!(
            "failed to flush recursive chain state asset: {}",
            path.display()
        )
    })?;
    Ok(())
}

/// Loads a verifier-side context from the committed binary asset layout.
fn load_verification_context_asset_from_reader<R: Read>(
    reader: &mut R,
) -> StmResult<VerificationContextAsset> {
    let global_field_elements = (0..5)
        .map(|_| read_field_element(reader))
        .collect::<Result<Vec<_>, _>>()?;
    let recursive_verifying_key = VerifyingKey::<F, KZGCommitmentScheme<E>>::read::<_, IvcCircuit>(
        reader,
        SerdeFormat::RawBytesUnchecked,
        (),
    )?;
    let combined_fixed_bases = read_named_fixed_bases(reader)?;
    let mut verifier_param_bytes = Vec::new();
    reader.read_to_end(&mut verifier_param_bytes)?;

    let mut verifier_params_reader = Cursor::new(&verifier_param_bytes);
    let verifier_params =
        ParamsVerifierKZG::<E>::read(&mut verifier_params_reader, SerdeFormat::RawBytesUnchecked)?;

    // `ParamsVerifierKZG` does not expose `s_g2()` in this path, so we re-read
    // the raw verifier-param bytes and rely on the current upstream raw
    // serialization layout where the first serialized G2 element is `s_g2`.
    let mut verifier_tau_reader = Cursor::new(&verifier_param_bytes);
    let verifier_tau_in_g2 =
        <E as Engine>::G2::read(&mut verifier_tau_reader, SerdeFormat::RawBytesUnchecked)?.into();

    Ok(VerificationContextAsset {
        global_field_elements,
        recursive_verifying_key,
        combined_fixed_bases,
        verifier_params,
        verifier_tau_in_g2,
    })
}

/// Loads the embedded verifier-side context compiled into the test binary.
pub(crate) fn load_embedded_verification_context_asset() -> StmResult<VerificationContextAsset> {
    let mut reader = Cursor::new(VERIFICATION_CONTEXT_ASSET_BYTES);
    load_verification_context_asset_from_reader(&mut reader)
        .context("failed to decode embedded verification context asset")
}

/// Writes the verification-context asset using the committed binary layout.
pub(crate) fn store_verification_context_asset(
    path: &Path,
    asset: &VerificationContextAsset,
) -> StmResult<()> {
    let mut writer = create_asset_file(path).with_context(|| {
        format!(
            "failed to create verification context asset file: {}",
            path.display()
        )
    })?;

    for value in &asset.global_field_elements {
        write_field_element(&mut writer, value)?;
    }
    asset
        .recursive_verifying_key
        .write(&mut writer, SerdeFormat::RawBytesUnchecked)?;
    write_named_fixed_bases(&mut writer, &asset.combined_fixed_bases)?;
    asset
        .verifier_params
        .write(&mut writer, SerdeFormat::RawBytesUnchecked)?;
    writer.flush().with_context(|| {
        format!(
            "failed to flush verification context asset: {}",
            path.display()
        )
    })?;
    Ok(())
}

/// Loads a recursive step output from the committed binary asset layout.
fn load_recursive_step_output_asset_from_reader<R: Read>(
    reader: &mut R,
) -> StmResult<RecursiveStepOutputAsset> {
    let proof = read_length_prefixed_proof(reader)?;
    let next_accumulator = Accumulator::<S>::read(reader, SerdeFormat::RawBytesUnchecked)?;
    let next_state = read_state_public_input(reader)?;
    let certificate_proof = read_length_prefixed_proof(reader)?;

    Ok(RecursiveStepOutputAsset {
        proof,
        next_accumulator,
        next_state,
        certificate_proof,
    })
}

/// Loads the embedded recursive step output compiled into the test binary.
pub(crate) fn load_embedded_recursive_step_output_asset() -> StmResult<RecursiveStepOutputAsset> {
    let mut reader = Cursor::new(RECURSIVE_STEP_OUTPUT_ASSET_BYTES);
    load_recursive_step_output_asset_from_reader(&mut reader)
        .context("failed to decode embedded recursive step output asset")
}

/// Loads the embedded genesis step output compiled into the test binary.
pub(crate) fn load_embedded_genesis_step_output_asset() -> StmResult<RecursiveStepOutputAsset> {
    let mut reader = Cursor::new(GENESIS_STEP_OUTPUT_ASSET_BYTES);
    load_recursive_step_output_asset_from_reader(&mut reader)
        .context("failed to decode embedded genesis step output asset")
}

/// Loads the embedded same-epoch step output compiled into the test binary.
pub(crate) fn load_embedded_same_epoch_step_output_asset() -> StmResult<RecursiveStepOutputAsset> {
    let mut reader = Cursor::new(SAME_EPOCH_STEP_OUTPUT_ASSET_BYTES);
    load_recursive_step_output_asset_from_reader(&mut reader)
        .context("failed to decode embedded same-epoch step output asset")
}

/// Writes the recursive step output asset using the committed binary layout.
pub(crate) fn store_recursive_step_output_asset(
    path: &Path,
    asset: &RecursiveStepOutputAsset,
) -> StmResult<()> {
    let mut writer = create_asset_file(path).with_context(|| {
        format!(
            "failed to create recursive step output asset file: {}",
            path.display()
        )
    })?;

    write_length_prefixed_proof(&mut writer, &asset.proof)?;
    asset
        .next_accumulator
        .write(&mut writer, SerdeFormat::RawBytesUnchecked)?;
    write_state_public_input(&mut writer, &asset.next_state)?;
    write_length_prefixed_proof(&mut writer, &asset.certificate_proof)?;
    writer.flush().with_context(|| {
        format!(
            "failed to flush recursive step output asset: {}",
            path.display()
        )
    })?;
    Ok(())
}
