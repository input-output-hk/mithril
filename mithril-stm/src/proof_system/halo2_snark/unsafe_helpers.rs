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
    fs::File,
    io::{BufReader, ErrorKind},
    path::PathBuf,
};

use crate::{Parameters, StmResult, circuits::halo2::circuit::StmCircuit};

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

        let srs_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("src")
            .join("circuits")
            .join("halo2")
            .join("assets")
            .join(format!("params_kzg_unsafe_{}", circuit_degree));

        let srs = load_or_generate_srs(
            circuit_degree,
            srs_path.to_str().with_context(|| "SRS path contains invalid UTF-8")?,
        )?;

        let circuit = StmCircuit::try_new(params, merkle_tree_depth)?;
        let verification_key = zk::setup_vk(&srs, &circuit);
        let proving_key = zk::setup_pk(&circuit, &verification_key);

        Ok(Self {
            srs,
            circuit,
            verification_key,
            proving_key,
        })
    }
}

/// Load the KZG SRS from `path`, or generate one with an unsafe deterministic seed
/// if the file does not exist.
///
/// Generation can be slow for degrees above 13-14 and the result is **not** persisted.
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
        Err(e) if e.kind() == ErrorKind::NotFound => Ok(ParamsKZG::unsafe_setup(
            circuit_degree,
            ChaCha20Rng::seed_from_u64(42),
        )),
        Err(e) => Err(anyhow!("Failed to open SRS file at path '{path}': {e}")),
    }
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
