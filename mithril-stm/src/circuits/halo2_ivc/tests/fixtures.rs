use std::{
    collections::BTreeMap,
    fs::File,
    io::{self, BufReader, Read},
    path::{Path, PathBuf},
};

use midnight_proofs::{
    poly::kzg::params::ParamsVerifierKZG,
    utils::{SerdeFormat, helpers::ProcessedSerdeObject},
};

use crate::circuits::halo2_ivc::{
    Accumulator, C, E, F, KZGCommitmentScheme, VerifyingKey, circuit::IvcCircuit,
    helpers::utils::jubjub_base_from_le_bytes, io::Read as IvcRead, state::State,
};

/// Stored chain state fixture.
///
/// Layout:
/// - `global_field_elements`: 5 field elements
/// - `state`: 7 field elements
/// - `proof`: length-prefixed proof bytes
/// - `accumulator`: serialized accumulator
#[derive(Debug)]
pub(crate) struct RecursiveChainStateFixture {
    pub(crate) global_field_elements: Vec<F>,
    pub(crate) state: State,
    pub(crate) proof: Vec<u8>,
    pub(crate) accumulator: Accumulator<crate::circuits::halo2_ivc::S>,
}

/// Stored verification context fixture.
///
/// Layout:
/// - `global_field_elements`: 5 field elements
/// - `recursive_verifying_key`: serialized IVC verifying key
/// - `combined_fixed_bases`: count + named points
/// - `verifier_params`: shared verifier-side SRS data
#[derive(Debug)]
pub(crate) struct VerificationContextFixture {
    pub(crate) global_field_elements: Vec<F>,
    pub(crate) recursive_verifying_key: VerifyingKey<F, KZGCommitmentScheme<E>>,
    pub(crate) combined_fixed_bases: BTreeMap<String, C>,
    pub(crate) verifier_params: ParamsVerifierKZG<E>,
}

/// Stored recursive step output fixture.
///
/// Layout:
/// - `proof`: length-prefixed proof bytes
/// - `next_accumulator`: serialized accumulator
/// - `next_state`: 7 field elements
#[derive(Debug)]
pub(crate) struct RecursiveStepOutputFixture {
    pub(crate) proof: Vec<u8>,
    pub(crate) next_accumulator: Accumulator<crate::circuits::halo2_ivc::S>,
    pub(crate) next_state: State,
}

fn fixture_assets_directory() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/circuits/halo2_ivc/assets")
}

pub(super) fn recursive_chain_state_fixture_path() -> PathBuf {
    fixture_assets_directory().join("recursive_chain_state.bin")
}

pub(super) fn verification_context_fixture_path() -> PathBuf {
    fixture_assets_directory().join("verification_context.bin")
}

pub(super) fn recursive_step_output_fixture_path() -> PathBuf {
    fixture_assets_directory().join("recursive_step_output.bin")
}

fn open_fixture_file(path: &Path) -> io::Result<BufReader<File>> {
    File::open(path).map(BufReader::new)
}

fn read_field_element<R: Read>(reader: &mut R) -> io::Result<F> {
    let mut bytes = [0u8; 32];
    reader.read_exact(&mut bytes)?;
    Ok(jubjub_base_from_le_bytes(&bytes))
}

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

fn read_length_prefixed_proof<R: Read>(reader: &mut R) -> io::Result<Vec<u8>> {
    let mut len = [0u8; 4];
    reader.read_exact(&mut len)?;
    let proof_len = u32::from_le_bytes(len) as usize;

    let mut proof = vec![0u8; proof_len];
    reader.read_exact(&mut proof)?;

    Ok(proof)
}

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

pub(crate) fn load_recursive_chain_state(path: &Path) -> io::Result<RecursiveChainStateFixture> {
    let mut reader = open_fixture_file(path)?;

    let global_field_elements = (0..5)
        .map(|_| read_field_element(&mut reader))
        .collect::<Result<Vec<_>, _>>()?;
    let state = read_state_public_input(&mut reader)?;
    let proof = read_length_prefixed_proof(&mut reader)?;
    let accumulator = Accumulator::<crate::circuits::halo2_ivc::S>::read(
        &mut reader,
        SerdeFormat::RawBytesUnchecked,
    )?;

    Ok(RecursiveChainStateFixture {
        global_field_elements,
        state,
        proof,
        accumulator,
    })
}

pub(crate) fn load_verification_context(path: &Path) -> io::Result<VerificationContextFixture> {
    let mut reader = open_fixture_file(path)?;

    let global_field_elements = (0..5)
        .map(|_| read_field_element(&mut reader))
        .collect::<Result<Vec<_>, _>>()?;
    let recursive_verifying_key = VerifyingKey::<F, KZGCommitmentScheme<E>>::read::<_, IvcCircuit>(
        &mut reader,
        SerdeFormat::RawBytesUnchecked,
        (),
    )?;
    let combined_fixed_bases = read_named_fixed_bases(&mut reader)?;
    let verifier_params =
        ParamsVerifierKZG::<E>::read(&mut reader, SerdeFormat::RawBytesUnchecked)?;

    Ok(VerificationContextFixture {
        global_field_elements,
        recursive_verifying_key,
        combined_fixed_bases,
        verifier_params,
    })
}

pub(crate) fn load_recursive_step_output(path: &Path) -> io::Result<RecursiveStepOutputFixture> {
    let mut reader = open_fixture_file(path)?;

    let proof = read_length_prefixed_proof(&mut reader)?;
    let next_accumulator = Accumulator::<crate::circuits::halo2_ivc::S>::read(
        &mut reader,
        SerdeFormat::RawBytesUnchecked,
    )?;
    let next_state = read_state_public_input(&mut reader)?;

    Ok(RecursiveStepOutputFixture {
        proof,
        next_accumulator,
        next_state,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore]
    fn load_generated_fixtures_only() {
        let recursive_chain_state =
            load_recursive_chain_state(&recursive_chain_state_fixture_path())
                .expect("recursive chain state fixture should load");
        let verification_context = load_verification_context(&verification_context_fixture_path())
            .expect("verification context fixture should load");
        let recursive_step_output =
            load_recursive_step_output(&recursive_step_output_fixture_path())
                .expect("recursive step output fixture should load");

        assert_eq!(recursive_chain_state.global_field_elements.len(), 5);
        assert_eq!(verification_context.global_field_elements.len(), 5);
        assert!(!recursive_chain_state.proof.is_empty());
        assert!(!recursive_step_output.proof.is_empty());
        assert!(!verification_context.combined_fixed_bases.is_empty());

        let _ = &verification_context.recursive_verifying_key;
        let _ = &verification_context.verifier_params;
        let _ = &recursive_chain_state.state;
        let _ = &recursive_chain_state.accumulator;
        let _ = &recursive_step_output.next_accumulator;
        let _ = &recursive_step_output.next_state;
    }
}
