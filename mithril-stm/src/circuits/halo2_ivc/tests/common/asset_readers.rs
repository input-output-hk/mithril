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
use midnight_zk_stdlib::MidnightVK;

use crate::StmResult;
use crate::circuits::halo2_ivc::{
    Accumulator, C, E, F, KZGCommitmentScheme, PREIMAGE_SIZE, S, VerifyingKey,
    circuit::IvcCircuitData,
    io::{Read as IvcRead, Write as IvcWrite},
    state::State,
    types::{
        CertificateProofBytes, EpochNumber, IvcProofBytes, MerkleTreeCommitment, MessageHash,
        ProtocolParametersHash, StepCounter,
    },
};
use crate::signature_scheme::StandardSchnorrSignature;

use super::field_encoding::jubjub_base_from_raw_le_bytes;

/// Stored recursive chain checkpoint used by the golden tests.
#[derive(Debug)]
pub(crate) struct RecursiveChainStateAsset {
    /// Stored global public inputs for the recursive flow.
    pub(crate) global_field_elements: Vec<F>,
    /// Stored recursive state checkpoint.
    pub(crate) state: State,
    /// Stored previous recursive proof bytes.
    pub(crate) ivc_proof: IvcProofBytes,
    /// Stored folded accumulator for the checkpoint.
    pub(crate) accumulator: Accumulator<S>,
    /// Stored chain-specific Schnorr signature over the genesis state.
    pub(crate) genesis_signature: StandardSchnorrSignature,
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
    /// Stored certificate verifying key, enabling MockProver tests to skip SRS generation.
    pub(crate) certificate_verifying_key: MidnightVK,
}

/// Stored data of the first certificate produced from `State::genesis()`. Used to test
/// `IvcProverInput::prepare` at the genesis transition (where the rolling state's
/// `step_counter` is zero and no previous IVC proof exists).
#[derive(Debug)]
pub(crate) struct FirstCertificateInEpochAsset {
    /// Certificate proof bytes (consumed by `prepare` via `SnarkProof`).
    pub(crate) certificate_proof: CertificateProofBytes,
    /// Expected next state after `prepare` advances by one step.
    pub(crate) next_state: State,
    /// SHA-256 hash that the certificate proof committed to.
    pub(crate) message: [u8; 32],
    /// Protocol-message preimage; `ProtocolMessagePreimage::current_epoch()` / `next_merkle_tree_commitment()` / `next_protocol_parameters()` decode the four
    /// epoch fields.
    pub(crate) message_preimage: [u8; PREIMAGE_SIZE],
    /// Canonical encoding of the aggregate verification key merkle root the certificate
    /// proof committed to.
    pub(crate) aggregate_verification_key_merkle_root: [u8; 32],
}

/// Stored output of the genesis base-case step. Carries no certificate; the message,
/// preimage, and aggregate-verification-key fields are zero-byte placeholders.
#[derive(Debug)]
pub(crate) struct GenesisStepOutputAsset {
    /// Stored final recursive proof bytes for the genesis step.
    pub(crate) ivc_proof: IvcProofBytes,
    /// Stored folded accumulator after the genesis step.
    pub(crate) next_accumulator: Accumulator<S>,
    /// Stored next recursive state.
    pub(crate) next_state: State,
    /// Empty: genesis carries no certificate proof.
    pub(crate) certificate_proof: CertificateProofBytes,
    /// Zero placeholder; no certificate exists at genesis.
    pub(crate) message: [u8; 32],
    /// Zero placeholder; no protocol-message preimage exists at genesis.
    pub(crate) message_preimage: [u8; PREIMAGE_SIZE],
    /// Zero placeholder; no aggregate verification key merkle root exists at genesis.
    pub(crate) aggregate_verification_key_merkle_root: [u8; 32],
}

/// Stored output of extending the recursive chain by one next-epoch step (the cert
/// is the first cert of a new epoch, transitioning the chain from epoch N to N+1).
#[derive(Debug)]
pub(crate) struct NextEpochStepOutputAsset {
    /// Stored final recursive proof bytes for the next-epoch step.
    pub(crate) ivc_proof: IvcProofBytes,
    /// Stored folded accumulator after the next-epoch step.
    pub(crate) next_accumulator: Accumulator<S>,
    /// Stored next recursive state.
    pub(crate) next_state: State,
    /// Stored certificate proof consumed by the next-epoch step.
    pub(crate) certificate_proof: CertificateProofBytes,
    /// SHA-256 hash that the certificate proof committed to.
    pub(crate) message: [u8; 32],
    /// Protocol-message preimage; `ProtocolMessagePreimage::current_epoch()` / `next_merkle_tree_commitment()` / `next_protocol_parameters()` decode the four
    /// epoch fields.
    pub(crate) message_preimage: [u8; PREIMAGE_SIZE],
    /// Canonical encoding of the aggregate verification key merkle root the certificate
    /// proof committed to.
    pub(crate) aggregate_verification_key_merkle_root: [u8; 32],
}

/// Stored output of extending the recursive chain by one same-epoch step (the cert
/// follows in the current epoch and does not change the epoch boundary).
#[derive(Debug)]
pub(crate) struct FollowingCertificateInEpochAsset {
    /// Stored final recursive proof bytes for the same-epoch step.
    pub(crate) ivc_proof: IvcProofBytes,
    /// Stored folded accumulator after the same-epoch step.
    pub(crate) next_accumulator: Accumulator<S>,
    /// Stored next recursive state.
    pub(crate) next_state: State,
    /// Stored certificate proof consumed by the same-epoch step.
    pub(crate) certificate_proof: CertificateProofBytes,
    /// SHA-256 hash that the certificate proof committed to.
    pub(crate) message: [u8; 32],
    /// Protocol-message preimage; `ProtocolMessagePreimage::current_epoch()` / `next_merkle_tree_commitment()` / `next_protocol_parameters()` decode the four
    /// epoch fields.
    pub(crate) message_preimage: [u8; PREIMAGE_SIZE],
    /// Canonical encoding of the aggregate verification key merkle root the certificate
    /// proof committed to.
    pub(crate) aggregate_verification_key_merkle_root: [u8; 32],
}

const RECURSIVE_CHAIN_STATE_ASSET_BYTES: &[u8] =
    include_bytes!("../assets/recursive_chain_state.bin");
const VERIFICATION_CONTEXT_ASSET_BYTES: &[u8] =
    include_bytes!("../assets/verification_context.bin");
const NEXT_EPOCH_STEP_OUTPUT_ASSET_BYTES: &[u8] =
    include_bytes!("../assets/recursive_step_output.bin");
const GENESIS_STEP_OUTPUT_ASSET_BYTES: &[u8] = include_bytes!("../assets/genesis_step_output.bin");
const FOLLOWING_CERTIFICATE_IN_EPOCH_ASSET_BYTES: &[u8] =
    include_bytes!("../assets/same_epoch_step_output.bin");
const FIRST_CERTIFICATE_IN_EPOCH_ASSET_BYTES: &[u8] =
    include_bytes!("../assets/first_step_cert.bin");

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
    Ok(jubjub_base_from_raw_le_bytes(&bytes))
}

/// Writes one field element as 32 little-endian bytes.
fn write_field_element<W: Write>(writer: &mut W, value: &F) -> StmResult<()> {
    writer.write_all(&value.to_bytes_le())?;
    Ok(())
}

/// Reads the seven public-input field elements that define a recursive state.
fn read_state_public_input<R: Read>(reader: &mut R) -> StmResult<State> {
    Ok(State::new(
        StepCounter::from_field(read_field_element(reader)?),
        MessageHash::from_field(read_field_element(reader)?),
        MerkleTreeCommitment::from_field(read_field_element(reader)?),
        MerkleTreeCommitment::from_field(read_field_element(reader)?),
        ProtocolParametersHash::from_field(read_field_element(reader)?),
        ProtocolParametersHash::from_field(read_field_element(reader)?),
        EpochNumber::from_field(read_field_element(reader)?),
    ))
}

/// Writes the seven public-input field elements of a recursive state.
fn write_state_public_input<W: Write>(writer: &mut W, state: &State) -> StmResult<()> {
    for value in state.as_public_input() {
        write_field_element(writer, &value)?;
    }
    Ok(())
}

/// Reads a 64-byte `StandardSchnorrSignature` (response | challenge).
fn read_schnorr_signature<R: Read>(reader: &mut R) -> StmResult<StandardSchnorrSignature> {
    let mut bytes = [0u8; 64];
    reader.read_exact(&mut bytes)?;
    StandardSchnorrSignature::from_bytes(&bytes)
}

/// Writes a 64-byte `StandardSchnorrSignature` (response | challenge).
fn write_schnorr_signature<W: Write>(
    writer: &mut W,
    signature: &StandardSchnorrSignature,
) -> StmResult<()> {
    writer.write_all(&signature.to_bytes())?;
    Ok(())
}

/// Reads exactly `N` bytes stored behind a 32-bit little-endian length prefix. Returns
/// an error if the prefix does not equal `N`.
fn read_fixed_length_prefixed<const N: usize, R: Read>(
    reader: &mut R,
    field_name: &str,
) -> StmResult<[u8; N]> {
    let bytes = read_length_prefixed_proof(reader)?;
    bytes
        .try_into()
        .map_err(|v: Vec<u8>| anyhow!("{field_name}: expected {N} bytes, got {} bytes", v.len()))
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
    let ivc_proof = IvcProofBytes::new(read_length_prefixed_proof(reader)?);
    let accumulator = Accumulator::<S>::read(reader, SerdeFormat::RawBytesUnchecked)?;
    let genesis_signature = read_schnorr_signature(reader)?;

    Ok(RecursiveChainStateAsset {
        global_field_elements,
        state,
        ivc_proof,
        accumulator,
        genesis_signature,
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
    write_length_prefixed_proof(&mut writer, asset.ivc_proof.as_bytes())?;
    asset.accumulator.write(&mut writer, SerdeFormat::RawBytesUnchecked)?;
    write_schnorr_signature(&mut writer, &asset.genesis_signature)?;
    writer.flush().with_context(|| {
        format!(
            "failed to flush recursive chain state asset: {}",
            path.display()
        )
    })?;
    Ok(())
}

/// Loads a verifier-side context from the committed binary asset layout.
///
/// Binary layout (v2):
/// `[global(5×32b) | recursive_vk(var) | combined_fixed_bases(var) |
///   verifier_params_len(4b LE) | verifier_params(var) |
///   certificate_vk_len(4b LE) | certificate_vk(var)]`
fn load_verification_context_asset_from_reader<R: Read>(
    reader: &mut R,
) -> StmResult<VerificationContextAsset> {
    let global_field_elements = (0..5)
        .map(|_| read_field_element(reader))
        .collect::<Result<Vec<_>, _>>()?;
    let recursive_verifying_key = VerifyingKey::<F, KZGCommitmentScheme<E>>::read::<
        _,
        IvcCircuitData,
    >(reader, SerdeFormat::RawBytesUnchecked, ())?;
    let combined_fixed_bases = read_named_fixed_bases(reader)?;

    // verifier_params is length-prefixed so the certificate verification key can follow it.
    let verifier_param_bytes = read_length_prefixed_proof(reader)?;

    let mut verifier_params_reader = Cursor::new(&verifier_param_bytes);
    let verifier_params =
        ParamsVerifierKZG::<E>::read(&mut verifier_params_reader, SerdeFormat::RawBytesUnchecked)?;

    // `ParamsVerifierKZG` does not expose `s_g2()` in this path, so we re-read
    // the raw verifier-param bytes and rely on the current upstream raw
    // serialization layout where the first serialized G2 element is `s_g2`.
    let mut verifier_tau_reader = Cursor::new(&verifier_param_bytes);
    let verifier_tau_in_g2 =
        <E as Engine>::G2::read(&mut verifier_tau_reader, SerdeFormat::RawBytesUnchecked)?.into();

    let certificate_verification_key_bytes = read_length_prefixed_proof(reader)?;
    let certificate_verifying_key = MidnightVK::read(
        &mut certificate_verification_key_bytes.as_slice(),
        SerdeFormat::RawBytes,
    )?;

    Ok(VerificationContextAsset {
        global_field_elements,
        recursive_verifying_key,
        combined_fixed_bases,
        verifier_params,
        verifier_tau_in_g2,
        certificate_verifying_key,
    })
}

/// Loads the embedded verifier-side context compiled into the test binary.
pub(crate) fn load_embedded_verification_context_asset() -> StmResult<VerificationContextAsset> {
    let mut reader = Cursor::new(VERIFICATION_CONTEXT_ASSET_BYTES);
    load_verification_context_asset_from_reader(&mut reader)
        .context("failed to decode embedded verification context asset")
}

/// Writes the verification-context asset using the committed binary layout.
///
/// Binary layout (v2): see `load_verification_context_asset_from_reader`.
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

    // Buffer verifier_params to write with a length prefix so the certificate verification key can follow.
    let mut verifier_params_buf = Vec::new();
    asset
        .verifier_params
        .write(&mut verifier_params_buf, SerdeFormat::RawBytesUnchecked)?;
    write_length_prefixed_proof(&mut writer, &verifier_params_buf)?;

    let mut certificate_verification_key_buf = Vec::new();
    asset
        .certificate_verifying_key
        .write(&mut certificate_verification_key_buf, SerdeFormat::RawBytes)?;
    write_length_prefixed_proof(&mut writer, &certificate_verification_key_buf)?;

    writer.flush().with_context(|| {
        format!(
            "failed to flush verification context asset: {}",
            path.display()
        )
    })?;
    Ok(())
}

/// Read-only view of the fields that generic test helpers need across the three
/// step output asset variants. Implemented by `GenesisStepOutputAsset`,
/// `NextEpochStepOutputAsset`, and `FollowingCertificateInEpochAsset`.
pub(crate) trait StepOutputAsset {
    fn next_state(&self) -> &State;
    fn next_accumulator(&self) -> &Accumulator<S>;
    fn ivc_proof(&self) -> &IvcProofBytes;
}

impl StepOutputAsset for GenesisStepOutputAsset {
    fn next_state(&self) -> &State {
        &self.next_state
    }
    fn next_accumulator(&self) -> &Accumulator<S> {
        &self.next_accumulator
    }
    fn ivc_proof(&self) -> &IvcProofBytes {
        &self.ivc_proof
    }
}

impl StepOutputAsset for NextEpochStepOutputAsset {
    fn next_state(&self) -> &State {
        &self.next_state
    }
    fn next_accumulator(&self) -> &Accumulator<S> {
        &self.next_accumulator
    }
    fn ivc_proof(&self) -> &IvcProofBytes {
        &self.ivc_proof
    }
}

impl StepOutputAsset for FollowingCertificateInEpochAsset {
    fn next_state(&self) -> &State {
        &self.next_state
    }
    fn next_accumulator(&self) -> &Accumulator<S> {
        &self.next_accumulator
    }
    fn ivc_proof(&self) -> &IvcProofBytes {
        &self.ivc_proof
    }
}

/// Shared on-the-wire shape for the three step output asset variants. Used as a
/// transport struct between the byte-level codec and the typed public assets.
struct StepOutputFields {
    ivc_proof: IvcProofBytes,
    next_accumulator: Accumulator<S>,
    next_state: State,
    certificate_proof: CertificateProofBytes,
    message: [u8; 32],
    message_preimage: [u8; PREIMAGE_SIZE],
    aggregate_verification_key_merkle_root: [u8; 32],
}

impl From<StepOutputFields> for GenesisStepOutputAsset {
    fn from(f: StepOutputFields) -> Self {
        Self {
            ivc_proof: f.ivc_proof,
            next_accumulator: f.next_accumulator,
            next_state: f.next_state,
            certificate_proof: f.certificate_proof,
            message: f.message,
            message_preimage: f.message_preimage,
            aggregate_verification_key_merkle_root: f.aggregate_verification_key_merkle_root,
        }
    }
}

impl From<StepOutputFields> for NextEpochStepOutputAsset {
    fn from(f: StepOutputFields) -> Self {
        Self {
            ivc_proof: f.ivc_proof,
            next_accumulator: f.next_accumulator,
            next_state: f.next_state,
            certificate_proof: f.certificate_proof,
            message: f.message,
            message_preimage: f.message_preimage,
            aggregate_verification_key_merkle_root: f.aggregate_verification_key_merkle_root,
        }
    }
}

impl From<StepOutputFields> for FollowingCertificateInEpochAsset {
    fn from(f: StepOutputFields) -> Self {
        Self {
            ivc_proof: f.ivc_proof,
            next_accumulator: f.next_accumulator,
            next_state: f.next_state,
            certificate_proof: f.certificate_proof,
            message: f.message,
            message_preimage: f.message_preimage,
            aggregate_verification_key_merkle_root: f.aggregate_verification_key_merkle_root,
        }
    }
}

/// Reads the step-output common shape from the committed binary asset layout.
fn read_step_output_fields_from_reader<R: Read>(reader: &mut R) -> StmResult<StepOutputFields> {
    let ivc_proof = IvcProofBytes::new(read_length_prefixed_proof(reader)?);
    let next_accumulator = Accumulator::<S>::read(reader, SerdeFormat::RawBytesUnchecked)?;
    let next_state = read_state_public_input(reader)?;
    let certificate_proof = CertificateProofBytes::from_certificate_circuit_proof_bytes(
        read_length_prefixed_proof(reader)?,
    );
    let message = read_fixed_length_prefixed::<32, _>(reader, "message")?;
    let message_preimage =
        read_fixed_length_prefixed::<PREIMAGE_SIZE, _>(reader, "message_preimage")?;
    let mut aggregate_verification_key_merkle_root = [0u8; 32];
    reader.read_exact(&mut aggregate_verification_key_merkle_root)?;
    Ok(StepOutputFields {
        ivc_proof,
        next_accumulator,
        next_state,
        certificate_proof,
        message,
        message_preimage,
        aggregate_verification_key_merkle_root,
    })
}

/// Writes the step-output common shape using the committed binary layout. Shared
/// across the three step output asset writers.
#[allow(clippy::too_many_arguments)]
fn write_step_output_fields<W: Write>(
    writer: &mut W,
    ivc_proof: &IvcProofBytes,
    next_accumulator: &Accumulator<S>,
    next_state: &State,
    certificate_proof: &CertificateProofBytes,
    message: &[u8; 32],
    message_preimage: &[u8; PREIMAGE_SIZE],
    aggregate_verification_key_merkle_root: &[u8; 32],
) -> StmResult<()> {
    write_length_prefixed_proof(writer, ivc_proof.as_bytes())?;
    next_accumulator.write(writer, SerdeFormat::RawBytesUnchecked)?;
    write_state_public_input(writer, next_state)?;
    write_length_prefixed_proof(writer, certificate_proof.as_bytes())?;
    write_length_prefixed_proof(writer, message)?;
    write_length_prefixed_proof(writer, message_preimage)?;
    writer.write_all(aggregate_verification_key_merkle_root)?;
    Ok(())
}

/// Loads the embedded next-epoch step output compiled into the test binary.
pub(crate) fn load_embedded_next_epoch_step_output_asset() -> StmResult<NextEpochStepOutputAsset> {
    let mut reader = Cursor::new(NEXT_EPOCH_STEP_OUTPUT_ASSET_BYTES);
    Ok(read_step_output_fields_from_reader(&mut reader)
        .context("failed to decode embedded next-epoch step output asset")?
        .into())
}

/// Loads the embedded genesis step output compiled into the test binary.
pub(crate) fn load_embedded_genesis_step_output_asset() -> StmResult<GenesisStepOutputAsset> {
    let mut reader = Cursor::new(GENESIS_STEP_OUTPUT_ASSET_BYTES);
    Ok(read_step_output_fields_from_reader(&mut reader)
        .context("failed to decode embedded genesis step output asset")?
        .into())
}

/// Loads the embedded following-certificate-in-epoch (same-epoch) step output compiled
/// into the test binary.
pub(crate) fn load_embedded_following_certificate_in_epoch_asset()
-> StmResult<FollowingCertificateInEpochAsset> {
    let mut reader = Cursor::new(FOLLOWING_CERTIFICATE_IN_EPOCH_ASSET_BYTES);
    Ok(read_step_output_fields_from_reader(&mut reader)
        .context("failed to decode embedded following-certificate-in-epoch asset")?
        .into())
}

/// Writes the next-epoch step output asset using the committed binary layout.
pub(crate) fn store_next_epoch_step_output_asset(
    path: &Path,
    asset: &NextEpochStepOutputAsset,
) -> StmResult<()> {
    let mut writer = create_asset_file(path).with_context(|| {
        format!(
            "failed to create next-epoch step output asset file: {}",
            path.display()
        )
    })?;
    write_step_output_fields(
        &mut writer,
        &asset.ivc_proof,
        &asset.next_accumulator,
        &asset.next_state,
        &asset.certificate_proof,
        &asset.message,
        &asset.message_preimage,
        &asset.aggregate_verification_key_merkle_root,
    )?;
    writer.flush().with_context(|| {
        format!(
            "failed to flush next-epoch step output asset: {}",
            path.display()
        )
    })?;
    Ok(())
}

/// Writes the genesis step output asset using the committed binary layout.
pub(crate) fn store_genesis_step_output_asset(
    path: &Path,
    asset: &GenesisStepOutputAsset,
) -> StmResult<()> {
    let mut writer = create_asset_file(path).with_context(|| {
        format!(
            "failed to create genesis step output asset file: {}",
            path.display()
        )
    })?;
    write_step_output_fields(
        &mut writer,
        &asset.ivc_proof,
        &asset.next_accumulator,
        &asset.next_state,
        &asset.certificate_proof,
        &asset.message,
        &asset.message_preimage,
        &asset.aggregate_verification_key_merkle_root,
    )?;
    writer.flush().with_context(|| {
        format!(
            "failed to flush genesis step output asset: {}",
            path.display()
        )
    })?;
    Ok(())
}

/// Writes the following-certificate-in-epoch step output asset using the committed
/// binary layout.
pub(crate) fn store_following_certificate_in_epoch_asset(
    path: &Path,
    asset: &FollowingCertificateInEpochAsset,
) -> StmResult<()> {
    let mut writer = create_asset_file(path).with_context(|| {
        format!(
            "failed to create following-certificate-in-epoch asset file: {}",
            path.display()
        )
    })?;
    write_step_output_fields(
        &mut writer,
        &asset.ivc_proof,
        &asset.next_accumulator,
        &asset.next_state,
        &asset.certificate_proof,
        &asset.message,
        &asset.message_preimage,
        &asset.aggregate_verification_key_merkle_root,
    )?;
    writer.flush().with_context(|| {
        format!(
            "failed to flush following-certificate-in-epoch asset: {}",
            path.display()
        )
    })?;
    Ok(())
}

/// Loads the first-certificate-in-epoch asset from the committed binary asset layout.
fn load_first_certificate_in_epoch_asset_from_reader<R: Read>(
    reader: &mut R,
) -> StmResult<FirstCertificateInEpochAsset> {
    let certificate_proof = CertificateProofBytes::from_certificate_circuit_proof_bytes(
        read_length_prefixed_proof(reader)?,
    );
    let next_state = read_state_public_input(reader)?;
    let message = read_fixed_length_prefixed::<32, _>(reader, "message")?;
    let message_preimage =
        read_fixed_length_prefixed::<PREIMAGE_SIZE, _>(reader, "message_preimage")?;
    let mut aggregate_verification_key_merkle_root = [0u8; 32];
    reader.read_exact(&mut aggregate_verification_key_merkle_root)?;

    Ok(FirstCertificateInEpochAsset {
        certificate_proof,
        next_state,
        message,
        message_preimage,
        aggregate_verification_key_merkle_root,
    })
}

/// Loads the embedded first-certificate-in-epoch asset compiled into the test binary.
pub(crate) fn load_embedded_first_certificate_in_epoch_asset()
-> StmResult<FirstCertificateInEpochAsset> {
    let mut reader = Cursor::new(FIRST_CERTIFICATE_IN_EPOCH_ASSET_BYTES);
    load_first_certificate_in_epoch_asset_from_reader(&mut reader)
        .context("failed to decode embedded first-certificate-in-epoch asset")
}

/// Writes the first-certificate-in-epoch asset using the committed binary layout.
pub(crate) fn store_first_certificate_in_epoch_asset(
    path: &Path,
    asset: &FirstCertificateInEpochAsset,
) -> StmResult<()> {
    let mut writer = create_asset_file(path).with_context(|| {
        format!(
            "failed to create first-certificate-in-epoch asset file: {}",
            path.display()
        )
    })?;

    write_length_prefixed_proof(&mut writer, asset.certificate_proof.as_bytes())?;
    write_state_public_input(&mut writer, &asset.next_state)?;
    write_length_prefixed_proof(&mut writer, &asset.message)?;
    write_length_prefixed_proof(&mut writer, &asset.message_preimage)?;
    writer.write_all(&asset.aggregate_verification_key_merkle_root)?;
    writer.flush().with_context(|| {
        format!(
            "failed to flush first-certificate-in-epoch asset: {}",
            path.display()
        )
    })?;
    Ok(())
}
