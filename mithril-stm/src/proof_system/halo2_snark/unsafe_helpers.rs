//! Temporary helpers for SNARK setup during the build/prototyping phase.
//!
//! This module bundles SRS loading, circuit compilation, and key derivation into
//! [`SnarkSetup`]. The current implementation uses pre-generated SRS files with an
//! unsafe deterministic fallback for testing. It will be replaced by production-ready
//! setup code (trusted SRS ceremony, proper key management) before release.

use anyhow::{Context, anyhow};
use midnight_curves::Bls12;
use midnight_proofs::{poly::kzg::params::ParamsKZG, utils::SerdeFormat};
use midnight_zk_stdlib::{self as zk, MidnightPK, MidnightVK};
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;
use std::{
    collections::HashMap,
    fs::File,
    io::{BufReader, BufWriter, ErrorKind},
    path::PathBuf,
    sync::{Arc, LazyLock, RwLock},
};

use crate::{Parameters, StmResult, circuits::halo2::circuit::StmCircuit};

/// Cache key for derived VK/PK pairs, scoped to a specific circuit configuration.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct SnarkSetupCacheKey {
    circuit_degree: u32,
    k: u64,
    m: u64,
    merkle_tree_depth: u32,
}

type SnarkSetupKeyPair = Arc<(MidnightVK, MidnightPK<StmCircuit>)>;

static SNARK_SETUP_KEY_CACHE: LazyLock<RwLock<HashMap<SnarkSetupCacheKey, SnarkSetupKeyPair>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Base degree of the circuit when `k = 0`.
///
/// The actual circuit degree is `BASE_CIRCUIT_DEGREE + ceil_log2(k)`.
const BASE_CIRCUIT_DEGREE: u32 = 11;

/// Bundles the one-time setup artifacts needed to prove and verify SNARK proofs.
///
/// This includes the Structured Reference String (SRS), the compiled circuit, and the
/// proving and verification keys derived from them.
// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
pub struct SnarkSetup {
    /// KZG Structured Reference String.
    pub(crate) srs: ParamsKZG<Bls12>,
    /// Compiled STM circuit.
    pub(crate) circuit: StmCircuit,
    /// Verification key for the SNARK proof.
    pub(crate) verification_key: MidnightVK,
    /// Proving key for the SNARK proof.
    pub(crate) proving_key: MidnightPK<StmCircuit>,
}

impl SnarkSetup {
    /// Build a new `SnarkSetup` from protocol parameters and Merkle tree depth.
    ///
    /// The setup pipeline:
    /// 1. Computes the circuit degree from the protocol parameter `k`.
    /// 2. Loads the SRS from a pre-generated file, or generates one with an unsafe
    ///    deterministic seed if the file is not found.
    /// 3. Compiles the STM circuit for the given parameters and tree depth.
    /// 4. Derives the verification and proving keys from the SRS and circuit.
    ///
    /// Returns an error if the SRS cannot be loaded or generated, or if the circuit
    /// cannot be compiled with the given parameters.
    pub(crate) fn try_new(params: &Parameters, merkle_tree_depth: u32) -> StmResult<Self> {
        let circuit_degree = compute_circuit_degree(params.k)?;

        let srs_path = srs_assets_dir()
            .join(format!("params_kzg_unsafe_{}", circuit_degree));

        let srs = load_or_generate_srs(
            circuit_degree,
            srs_path.to_str().with_context(|| "SRS path contains invalid UTF-8")?,
        )?;

        let circuit = StmCircuit::try_new(params, merkle_tree_depth)?;

        let cache_key = SnarkSetupCacheKey {
            circuit_degree,
            k: params.k,
            m: params.m,
            merkle_tree_depth,
        };
        let key_pair = get_or_build_snark_keys(cache_key, &circuit, &srs)?;
        let (verification_key, proving_key) = (&key_pair.0, &key_pair.1);

        Ok(Self {
            srs,
            circuit,
            verification_key: verification_key.clone(),
            proving_key: proving_key.clone(),
        })
    }
}

/// Resolve the directory where SRS asset files are stored.
///
/// Checks `MITHRIL_STM_SRS_DIR` at runtime first, then falls back to the path
/// embedded at compile time (`CARGO_MANIFEST_DIR/src/circuits/halo2/assets`).
/// The environment variable allows e2e tests and CI pipelines to supply a custom
/// directory without requiring the original source tree to be present at runtime.
fn srs_assets_dir() -> PathBuf {
    std::env::var("MITHRIL_STM_SRS_DIR")
        .ok()
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("src")
                .join("circuits")
                .join("halo2")
                .join("assets")
        })
}

/// Load the KZG SRS from `path`, or generate one with an unsafe deterministic seed
/// if the file does not exist. When generated, the result is persisted to `path` so
/// subsequent calls can load it quickly.
fn load_or_generate_srs(circuit_degree: u32, path: &str) -> StmResult<ParamsKZG<Bls12>> {
    let file = File::open(path);
    match file {
        Ok(f) => {
            let mut reader = BufReader::new(f);
            Ok(
                ParamsKZG::read_custom(&mut reader, SerdeFormat::RawBytesUnchecked)
                    .with_context(|| "Failed to read the srs bytes.")?,
            )
        }
        Err(e) if e.kind() == ErrorKind::NotFound => {
            let srs = ParamsKZG::unsafe_setup(circuit_degree, ChaCha20Rng::seed_from_u64(42));
            if let Err(persist_err) = persist_srs(&srs, path) {
                // Persistence failure is non-fatal: the SRS is still usable in memory.
                eprintln!("Warning: failed to persist generated SRS to '{path}': {persist_err}");
            }
            Ok(srs)
        }
        Err(e) => Err(anyhow!("Failed to open SRS file at path '{path}': {e}")),
    }
}

/// Persist a KZG SRS to disk so subsequent calls to `load_or_generate_srs` can load it.
fn persist_srs(srs: &ParamsKZG<Bls12>, path: &str) -> StmResult<()> {
    if let Some(parent) = std::path::Path::new(path).parent() {
        std::fs::create_dir_all(parent).with_context(|| {
            format!("Failed to create SRS directory at '{}'", parent.display())
        })?;
    }
    let file = File::create(path)
        .with_context(|| format!("Failed to create SRS file at '{path}'"))?;
    let mut writer = BufWriter::new(file);
    srs.write_custom(&mut writer, SerdeFormat::RawBytesUnchecked)
        .with_context(|| format!("Failed to write SRS to '{path}'"))
}

/// Return a cached VK/PK pair for `cache_key`, or derive and cache it on first call.
///
/// The proving-key derivation (`zk::setup_pk`) is the dominant cost in `SnarkSetup::try_new`.
/// Caching it per `(circuit_degree, k, m, merkle_tree_depth)` avoids redundant work across
/// multiple certificate generations within the same process.
fn get_or_build_snark_keys(
    cache_key: SnarkSetupCacheKey,
    circuit: &StmCircuit,
    srs: &ParamsKZG<Bls12>,
) -> StmResult<SnarkSetupKeyPair> {
    if let Some(key_pair) = SNARK_SETUP_KEY_CACHE
        .read()
        .map_err(|_| anyhow!("SNARK setup key cache lock poisoned on read"))?
        .get(&cache_key)
        .cloned()
    {
        return Ok(key_pair);
    }

    let vk = zk::setup_vk(srs, circuit);
    let pk = zk::setup_pk(circuit, &vk);
    let key_pair = Arc::new((vk, pk));

    SNARK_SETUP_KEY_CACHE
        .write()
        .map_err(|_| anyhow!("SNARK setup key cache lock poisoned on write"))?
        .insert(cache_key, key_pair.clone());

    Ok(key_pair)
}

/// Compute the circuit degree from the protocol parameter `k`.
///
/// Returns an error if `k` is zero, since a quorum threshold of zero is invalid.
pub(crate) fn compute_circuit_degree(k: u64) -> StmResult<u32> {
    if k == 0 {
        return Err(anyhow!("Protocol parameter k must be greater than zero"));
    }
    Ok(BASE_CIRCUIT_DEGREE + k.next_power_of_two().trailing_zeros())
}
