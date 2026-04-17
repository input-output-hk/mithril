use std::{
    collections::BTreeMap,
    fs::{self, File},
    io::{self, BufReader, BufWriter, Cursor, Read, Write},
    path::{Path, PathBuf},
};

use midnight_curves::pairing::Engine;
use midnight_proofs::{
    poly::kzg::params::ParamsVerifierKZG,
    utils::{SerdeFormat, helpers::ProcessedSerdeObject},
};

use crate::circuits::halo2_ivc::{
    Accumulator, C, E, F, KZGCommitmentScheme, VerifyingKey,
    circuit::IvcCircuit,
    helpers::utils::jubjub_base_from_le_bytes,
    io::{Read as IvcRead, Write as IvcWrite},
    state::State,
};

/// Stored recursive chain state asset.
///
/// Layout:
/// - `global_field_elements`: 5 field elements
/// - `state`: 7 field elements
/// - `proof`: length-prefixed proof bytes
/// - `accumulator`: serialized accumulator
#[derive(Debug)]
pub(crate) struct RecursiveChainStateAsset {
    pub(crate) global_field_elements: Vec<F>,
    pub(crate) state: State,
    pub(crate) proof: Vec<u8>,
    pub(crate) accumulator: Accumulator<crate::circuits::halo2_ivc::S>,
}

/// Stored verification context asset.
///
/// Layout:
/// - `global_field_elements`: 5 field elements
/// - `recursive_verifying_key`: serialized IVC verifying key
/// - `combined_fixed_bases`: count + named points
/// - `verifier_params`: shared verifier-side SRS data
#[derive(Debug)]
pub(crate) struct VerificationContextAsset {
    pub(crate) global_field_elements: Vec<F>,
    pub(crate) recursive_verifying_key: VerifyingKey<F, KZGCommitmentScheme<E>>,
    pub(crate) combined_fixed_bases: BTreeMap<String, C>,
    pub(crate) verifier_params: ParamsVerifierKZG<E>,
    pub(crate) verifier_tau_in_g2: <E as Engine>::G2Affine,
}

/// Stored recursive step output asset.
///
/// Layout:
/// - `proof`: length-prefixed proof bytes
/// - `next_accumulator`: serialized accumulator
/// - `next_state`: 7 field elements
/// - `certificate_proof`: length-prefixed proof bytes
#[derive(Debug)]
pub(crate) struct RecursiveStepOutputAsset {
    pub(crate) proof: Vec<u8>,
    pub(crate) next_accumulator: Accumulator<crate::circuits::halo2_ivc::S>,
    pub(crate) next_state: State,
    pub(crate) certificate_proof: Vec<u8>,
}

fn stored_asset_directory() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/circuits/halo2_ivc/tests/golden/assets")
}

/// Returns the committed asset path for the stored recursive chain snapshot.
pub(super) fn recursive_chain_state_asset_path() -> PathBuf {
    stored_asset_directory().join("recursive_chain_state.bin")
}

/// Returns the committed asset path for the static verifier-side context.
pub(super) fn verification_context_asset_path() -> PathBuf {
    stored_asset_directory().join("verification_context.bin")
}

/// Returns the committed asset path for the final recursive step output.
pub(super) fn recursive_step_output_asset_path() -> PathBuf {
    stored_asset_directory().join("recursive_step_output.bin")
}

/// Opens a committed golden asset for buffered reading.
fn open_asset_file(path: &Path) -> io::Result<BufReader<File>> {
    File::open(path).map(BufReader::new)
}

/// Creates a golden asset file and its parent directory if needed.
fn create_asset_file(path: &Path) -> io::Result<BufWriter<File>> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    File::create(path).map(BufWriter::new)
}

/// Reads one field element encoded as 32 little-endian bytes.
fn read_field_element<R: Read>(reader: &mut R) -> io::Result<F> {
    let mut bytes = [0u8; 32];
    reader.read_exact(&mut bytes)?;
    Ok(jubjub_base_from_le_bytes(&bytes))
}

/// Writes one field element as 32 little-endian bytes.
fn write_field_element<W: Write>(writer: &mut W, value: &F) -> io::Result<()> {
    writer.write_all(&value.to_bytes_le())
}

/// Reads the seven public-input field elements that define a recursive state.
fn read_state_public_input<R: Read>(reader: &mut R) -> io::Result<State> {
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
fn write_state_public_input<W: Write>(writer: &mut W, state: &State) -> io::Result<()> {
    for value in state.as_public_input() {
        write_field_element(writer, &value)?;
    }
    Ok(())
}

/// Reads proof bytes stored behind a 32-bit little-endian length prefix.
fn read_length_prefixed_proof<R: Read>(reader: &mut R) -> io::Result<Vec<u8>> {
    let mut len = [0u8; 4];
    reader.read_exact(&mut len)?;
    let proof_len = u32::from_le_bytes(len) as usize;

    let mut proof = vec![0u8; proof_len];
    reader.read_exact(&mut proof)?;

    Ok(proof)
}

/// Writes proof bytes with a 32-bit little-endian length prefix.
fn write_length_prefixed_proof<W: Write>(writer: &mut W, proof: &[u8]) -> io::Result<()> {
    writer.write_all(&(proof.len() as u32).to_le_bytes())?;
    writer.write_all(proof)
}

/// Reads the named fixed-base map stored in the verification-context asset.
fn read_named_fixed_bases<R: Read>(reader: &mut R) -> io::Result<BTreeMap<String, C>> {
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
            .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "invalid UTF-8 key"))?;

        let point = C::read(reader, SerdeFormat::RawBytesUnchecked)?;
        map.insert(name, point);
    }

    Ok(map)
}

/// Writes the named fixed-base map stored in the verification-context asset.
fn write_named_fixed_bases<W: Write>(
    writer: &mut W,
    fixed_bases: &BTreeMap<String, C>,
) -> io::Result<()> {
    writer.write_all(&(fixed_bases.len() as u32).to_le_bytes())?;
    for (name, point) in fixed_bases {
        let name_bytes = name.as_bytes();
        writer.write_all(&(name_bytes.len() as u32).to_le_bytes())?;
        writer.write_all(name_bytes)?;
        point.write(writer, SerdeFormat::RawBytesUnchecked)?;
    }
    Ok(())
}

/// Loads the stored recursive chain snapshot used by the golden tests.
pub(crate) fn load_recursive_chain_state_asset(
    path: &Path,
) -> io::Result<RecursiveChainStateAsset> {
    let mut reader = open_asset_file(path)?;

    let global_field_elements = (0..5)
        .map(|_| read_field_element(&mut reader))
        .collect::<Result<Vec<_>, _>>()?;
    let state = read_state_public_input(&mut reader)?;
    let proof = read_length_prefixed_proof(&mut reader)?;
    let accumulator = Accumulator::<crate::circuits::halo2_ivc::S>::read(
        &mut reader,
        SerdeFormat::RawBytesUnchecked,
    )?;

    Ok(RecursiveChainStateAsset {
        global_field_elements,
        state,
        proof,
        accumulator,
    })
}

/// Writes the recursive chain state asset using the committed binary layout.
pub(crate) fn store_recursive_chain_state_asset(
    path: &Path,
    asset: &RecursiveChainStateAsset,
) -> io::Result<()> {
    let mut writer = create_asset_file(path)?;

    for value in &asset.global_field_elements {
        write_field_element(&mut writer, value)?;
    }
    write_state_public_input(&mut writer, &asset.state)?;
    write_length_prefixed_proof(&mut writer, &asset.proof)?;
    asset.accumulator.write(&mut writer, SerdeFormat::RawBytesUnchecked)?;
    writer.flush()
}

/// Loads the static verifier-side asset set used by the golden tests.
pub(crate) fn load_verification_context_asset(path: &Path) -> io::Result<VerificationContextAsset> {
    let mut reader = open_asset_file(path)?;

    let global_field_elements = (0..5)
        .map(|_| read_field_element(&mut reader))
        .collect::<Result<Vec<_>, _>>()?;
    let recursive_verifying_key = VerifyingKey::<F, KZGCommitmentScheme<E>>::read::<_, IvcCircuit>(
        &mut reader,
        SerdeFormat::RawBytesUnchecked,
        (),
    )?;
    let combined_fixed_bases = read_named_fixed_bases(&mut reader)?;
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

/// Writes the verification-context asset using the committed binary layout.
pub(crate) fn store_verification_context_asset(
    path: &Path,
    asset: &VerificationContextAsset,
) -> io::Result<()> {
    let mut writer = create_asset_file(path)?;

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
    writer.flush()
}

/// Loads the stored output of extending the recursive chain by one more step.
pub(crate) fn load_recursive_step_output_asset(
    path: &Path,
) -> io::Result<RecursiveStepOutputAsset> {
    let mut reader = open_asset_file(path)?;

    let proof = read_length_prefixed_proof(&mut reader)?;
    let next_accumulator = Accumulator::<crate::circuits::halo2_ivc::S>::read(
        &mut reader,
        SerdeFormat::RawBytesUnchecked,
    )?;
    let next_state = read_state_public_input(&mut reader)?;
    let certificate_proof = read_length_prefixed_proof(&mut reader)?;

    Ok(RecursiveStepOutputAsset {
        proof,
        next_accumulator,
        next_state,
        certificate_proof,
    })
}

/// Writes the recursive step output asset using the committed binary layout.
pub(crate) fn store_recursive_step_output_asset(
    path: &Path,
    asset: &RecursiveStepOutputAsset,
) -> io::Result<()> {
    let mut writer = create_asset_file(path)?;

    write_length_prefixed_proof(&mut writer, &asset.proof)?;
    asset
        .next_accumulator
        .write(&mut writer, SerdeFormat::RawBytesUnchecked)?;
    write_state_public_input(&mut writer, &asset.next_state)?;
    write_length_prefixed_proof(&mut writer, &asset.certificate_proof)?;
    writer.flush()
}
