use std::{
    collections::BTreeMap,
    fs::File,
    io::{self, BufReader, Read},
    path::{Path, PathBuf},
};

use midnight_curves::serde::SerdeObject;
use midnight_proofs::utils::{SerdeFormat, helpers::ProcessedSerdeObject};

use crate::circuits::halo2_ivc::{
    Accumulator, C, E, F, KZGCommitmentScheme, VerifyingKey,
    circuit::IvcCircuit,
    io::Read as IvcRead,
    state::State,
};

/// Stored chain state fixture.
///
/// Layout:
/// - `global_fes`: 5 field elements
/// - `state`: 7 field elements
/// - `proof`: length-prefixed proof bytes
/// - `accumulator`: serialized accumulator
#[derive(Debug)]
pub(crate) struct ChainStateFixture {
    pub(crate) global_fes: Vec<F>,
    pub(crate) state: State,
    pub(crate) proof: Vec<u8>,
    pub(crate) accumulator: Accumulator<crate::circuits::halo2_ivc::S>,
}

/// Stored protocol data fixture.
///
/// Layout:
/// - `global_fes`: 5 field elements
/// - `self_vk`: serialized IVC verifying key
/// - `combined_fixed_bases`: count + named points
#[derive(Debug)]
pub(crate) struct ProtocolDataFixture {
    pub(crate) global_fes: Vec<F>,
    pub(crate) self_vk: VerifyingKey<F, KZGCommitmentScheme<E>>,
    pub(crate) combined_fixed_bases: BTreeMap<String, C>,
}

/// Stored aggregator result fixture.
///
/// Layout:
/// - `proof`: length-prefixed proof bytes
/// - `next_accumulator`: serialized accumulator
/// - `next_state`: 7 field elements
#[derive(Debug)]
pub(crate) struct AggregatorResultFixture {
    pub(crate) proof: Vec<u8>,
    pub(crate) next_accumulator: Accumulator<crate::circuits::halo2_ivc::S>,
    pub(crate) next_state: State,
}

fn assets_dir() -> PathBuf {
    PathBuf::from("mithril-stm/src/circuits/halo2_ivc/tests/assets")
}

pub(super) fn chain_state_fixture_path() -> PathBuf {
    assets_dir().join("chain_state.bin")
}

pub(super) fn protocol_data_fixture_path() -> PathBuf {
    assets_dir().join("protocol_data.bin")
}

pub(super) fn aggr_result_fixture_path() -> PathBuf {
    assets_dir().join("aggr_result.bin")
}

fn open_fixture(path: &Path) -> io::Result<BufReader<File>> {
    File::open(path).map(BufReader::new)
}

fn read_fe<R: Read>(reader: &mut R) -> io::Result<F> {
    F::read_raw(reader)
}

fn read_state<R: Read>(reader: &mut R) -> io::Result<State> {
    Ok(State::new(
        read_fe(reader)?,
        read_fe(reader)?,
        read_fe(reader)?,
        read_fe(reader)?,
        read_fe(reader)?,
        read_fe(reader)?,
        read_fe(reader)?,
    ))
}

fn read_proof<R: Read>(reader: &mut R) -> io::Result<Vec<u8>> {
    let mut len = [0u8; 4];
    reader.read_exact(&mut len)?;
    let proof_len = u32::from_le_bytes(len) as usize;

    let mut proof = vec![0u8; proof_len];
    reader.read_exact(&mut proof)?;

    Ok(proof)
}

fn read_combined_fixed_bases<R: Read>(reader: &mut R) -> io::Result<BTreeMap<String, C>> {
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

pub(crate) fn load_chain_state(path: &Path) -> io::Result<ChainStateFixture> {
    let mut reader = open_fixture(path)?;

    let global_fes = (0..5)
        .map(|_| read_fe(&mut reader))
        .collect::<Result<Vec<_>, _>>()?;
    let state = read_state(&mut reader)?;
    let proof = read_proof(&mut reader)?;
    let accumulator = Accumulator::<crate::circuits::halo2_ivc::S>::read(
        &mut reader,
        SerdeFormat::RawBytesUnchecked,
    )?;

    Ok(ChainStateFixture {
        global_fes,
        state,
        proof,
        accumulator,
    })
}

pub(crate) fn load_protocol_data(path: &Path) -> io::Result<ProtocolDataFixture> {
    let mut reader = open_fixture(path)?;

    let global_fes = (0..5)
        .map(|_| read_fe(&mut reader))
        .collect::<Result<Vec<_>, _>>()?;
    let self_vk = VerifyingKey::<F, KZGCommitmentScheme<E>>::read::<_, IvcCircuit>(
        &mut reader,
        SerdeFormat::RawBytesUnchecked,
        (),
    )?;
    let combined_fixed_bases = read_combined_fixed_bases(&mut reader)?;

    Ok(ProtocolDataFixture {
        global_fes,
        self_vk,
        combined_fixed_bases,
    })
}

pub(crate) fn load_aggr_result(path: &Path) -> io::Result<AggregatorResultFixture> {
    let mut reader = open_fixture(path)?;

    let proof = read_proof(&mut reader)?;
    let next_accumulator = Accumulator::<crate::circuits::halo2_ivc::S>::read(
        &mut reader,
        SerdeFormat::RawBytesUnchecked,
    )?;
    let next_state = read_state(&mut reader)?;

    Ok(AggregatorResultFixture {
        proof,
        next_accumulator,
        next_state,
    })
}
