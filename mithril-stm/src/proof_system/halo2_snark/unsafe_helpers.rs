//! Temporary helpers for SNARK setup during the build/prototyping phase.
//!
//! This module bundles SRS loading, circuit compilation, and key derivation into
//! [`SnarkSetup`]. The current implementation uses pre-generated SRS files with an
//! unsafe deterministic fallback for testing. It will be replaced by production-ready
//! setup code (trusted SRS ceremony, proper key management) before release.
use std::{
    env,
    fs::File,
    io::{BufReader, BufWriter, ErrorKind},
    sync::{Arc, LazyLock, RwLock},
};

use anyhow::{Context, anyhow};
use midnight_curves::Bls12;
use midnight_proofs::{
    poly::kzg::params::{ParamsKZG, ParamsVerifierKZG},
    utils::SerdeFormat,
};
use midnight_zk_stdlib::{self as zk, MidnightCircuit, MidnightPK, MidnightVK};
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;

use crate::{Parameters, StmResult, circuits::halo2::circuit::StmCertificateCircuit};

/// Cache key for derived VK/PK pairs, scoped to a specific circuit configuration.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct SnarkSetupCacheKey {
    circuit_degree: u32,
    k: u64,
    m: u64,
    merkle_tree_depth: u32,
}

type SnarkSetupKeyPair = Arc<(MidnightVK, MidnightPK<StmCertificateCircuit>)>;

static SNARK_SETUP_KEY_CACHE: LazyLock<RwLock<Option<(SnarkSetupCacheKey, SnarkSetupKeyPair)>>> =
    LazyLock::new(|| RwLock::new(None));

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
    pub(crate) circuit: StmCertificateCircuit,
    /// Verification key for the SNARK proof.
    pub(crate) verification_key: MidnightVK,
    /// Proving key for the SNARK proof.
    pub(crate) proving_key: MidnightPK<StmCertificateCircuit>,
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
        let circuit = StmCertificateCircuit::try_new(params, merkle_tree_depth)?;
        let circuit_degree = MidnightCircuit::from_relation(&circuit).min_k();

        // Uses a temporary directory to store the srs generated
        // and to access it again during execution
        // TODO: Remove the use of temporary directory once the srs is
        // properly handled
        let srs_path = env::temp_dir()
            .join("mithril-srs")
            .join(format!("params_kzg_unsafe_{}", circuit_degree));

        let srs = load_or_generate_srs(
            circuit_degree,
            srs_path.to_str().with_context(|| "SRS path contains invalid UTF-8")?,
        )?;

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

/// Bundles the minimal setup artifacts needed to verify SNARK proofs.
///
/// Only the KZG verifier parameters are required for verification, and they depend solely on the
/// toxic secret `s = Fq::random(ChaCha20Rng::seed_from_u64(42))`. That secret is independent of
/// the circuit degree `k`, so a single hard-coded serialization of `s_g2` suffices for every
/// circuit produced by the deterministic unsafe setup.
pub(crate) struct SnarkVerifierSetup {
    /// KZG verifier parameters derived from `s_g2`.
    pub(crate) verifier_params: ParamsVerifierKZG<Bls12>,
}

/// Serialized `s_g2` (the only material the KZG verifier needs) for the deterministic SNARK setup
/// seeded with `ChaCha20Rng::seed_from_u64(42)`.
///
/// Regenerate by running the `golden_snark_verifier_params_bytes` test and copying its printed
/// decimal byte sequence.
const SNARK_VERIFIER_PARAMS_BYTES: [u8; 192] = [
    9, 133, 52, 70, 100, 186, 221, 42, 162, 210, 65, 103, 250, 71, 142, 192, 58, 111, 199, 110,
    176, 91, 161, 195, 250, 201, 221, 136, 183, 74, 68, 204, 221, 93, 8, 139, 182, 151, 92, 6, 168,
    223, 75, 16, 6, 248, 229, 53, 10, 219, 248, 43, 58, 117, 134, 19, 245, 109, 69, 25, 218, 98,
    249, 7, 90, 223, 221, 136, 43, 53, 243, 90, 85, 245, 50, 71, 17, 145, 52, 137, 36, 165, 195,
    133, 133, 41, 248, 60, 251, 3, 44, 200, 150, 47, 121, 34, 9, 231, 248, 39, 163, 121, 37, 113,
    212, 227, 126, 233, 45, 198, 96, 170, 240, 47, 77, 250, 32, 66, 193, 18, 103, 251, 89, 161,
    202, 162, 45, 89, 203, 163, 70, 170, 27, 1, 80, 237, 9, 87, 173, 20, 38, 94, 11, 146, 14, 217,
    151, 197, 170, 203, 108, 179, 227, 90, 187, 9, 128, 97, 0, 213, 149, 128, 132, 129, 179, 255,
    247, 26, 178, 177, 112, 81, 142, 4, 53, 235, 114, 223, 242, 209, 244, 23, 177, 150, 185, 252,
    175, 141, 204, 205, 242, 78,
];

impl SnarkVerifierSetup {
    /// Build the verifier setup from the embedded constant verifier params bytes.
    pub(crate) fn try_new() -> StmResult<Self> {
        let verifier_params = ParamsVerifierKZG::<Bls12>::read(
            &mut &SNARK_VERIFIER_PARAMS_BYTES[..],
            SerdeFormat::RawBytesUnchecked,
        )
        .with_context(|| "Failed to read embedded SNARK verifier params bytes")?;

        Ok(Self { verifier_params })
    }
}

/// Compute the deterministic verifier params bytes for the unsafe SNARK setup seeded with
/// `ChaCha20Rng::seed_from_u64(42)`.
///
/// Kept available so the embedded `SNARK_VERIFIER_PARAMS_BYTES` constant can be regenerated by
/// running the `golden_snark_verifier_params_bytes` test.
#[cfg(test)]
fn compute_snark_verifier_params_bytes() -> StmResult<Vec<u8>> {
    use ff::Field;
    use group::Group;
    use midnight_curves::{Fq, G2Projective};
    use midnight_proofs::utils::helpers::ProcessedSerdeObject;

    let mut rng = ChaCha20Rng::seed_from_u64(42);
    let s = Fq::random(&mut rng);
    let s_g2 = G2Projective::generator() * s;

    let mut buf = Vec::new();
    s_g2.write(&mut buf, SerdeFormat::RawBytesUnchecked)
        .with_context(|| "Failed to serialize s_g2")?;

    Ok(buf)
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
        std::fs::create_dir_all(parent)
            .with_context(|| format!("Failed to create SRS directory at '{}'", parent.display()))?;
    }
    let file =
        File::create(path).with_context(|| format!("Failed to create SRS file at '{path}'"))?;
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
    circuit: &StmCertificateCircuit,
    srs: &ParamsKZG<Bls12>,
) -> StmResult<SnarkSetupKeyPair> {
    if let Some(key_pair) = SNARK_SETUP_KEY_CACHE
        .read()
        .map_err(|_| anyhow!("SNARK setup key cache lock poisoned on read"))?
        .as_ref()
        .filter(|(k, _)| *k == cache_key)
        .map(|(_, v)| v.clone())
    {
        return Ok(key_pair);
    }

    let vk = zk::setup_vk(srs, circuit);
    let pk = zk::setup_pk(circuit, &vk);
    let key_pair = Arc::new((vk, pk));

    let mut cache = SNARK_SETUP_KEY_CACHE
        .write()
        .map_err(|_| anyhow!("SNARK setup key cache lock poisoned on write"))?;
    *cache = Some((cache_key, key_pair.clone()));

    Ok(key_pair)
}

#[cfg(test)]
mod test {
    use std::{fs, sync::Arc};

    use midnight_curves::Bls12;
    use midnight_proofs::{poly::kzg::params::ParamsKZG, utils::SerdeFormat};
    use midnight_zk_stdlib::MidnightCircuit;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{
        Parameters, circuits::halo2::circuit::StmCertificateCircuit,
        proof_system::halo2_snark::SnarkSetup,
    };

    use super::{
        SNARK_VERIFIER_PARAMS_BYTES, SnarkSetupCacheKey, SnarkVerifierSetup,
        compute_snark_verifier_params_bytes, get_or_build_snark_keys, load_or_generate_srs,
        persist_srs,
    };

    fn small_srs() -> ParamsKZG<Bls12> {
        ParamsKZG::unsafe_setup(3, ChaCha20Rng::seed_from_u64(42))
    }

    fn default_params() -> Parameters {
        Parameters {
            k: 3,
            m: 10,
            phi_f: 0.2,
        }
    }

    #[test]
    fn persist_srs_creates_file() {
        let dir = std::env::temp_dir().join("mithril-test-srs-creates-file");
        let path = dir.join("params");
        fs::remove_dir_all(&dir).ok();

        persist_srs(&small_srs(), path.to_str().unwrap()).unwrap();

        assert!(path.exists());
        assert!(fs::metadata(&path).unwrap().len() > 0);
        fs::remove_dir_all(&dir).ok();
    }

    #[test]
    fn persist_srs_creates_missing_parent_directories() {
        let root = std::env::temp_dir().join("mithril-test-srs-parent-dirs");
        let path = root.join("nested").join("deep").join("params");
        fs::remove_dir_all(&root).ok();

        assert!(!root.exists());
        persist_srs(&small_srs(), path.to_str().unwrap()).unwrap();

        assert!(path.exists());
        fs::remove_dir_all(&root).ok();
    }

    #[test]
    fn load_or_generate_srs_loads_from_existing_file() {
        let dir = std::env::temp_dir().join("mithril-test-srs-load-existing");
        let path = dir.join("params_kzg_unsafe_3");
        fs::remove_dir_all(&dir).ok();

        let original = small_srs();
        persist_srs(&original, path.to_str().unwrap()).unwrap();

        let loaded = load_or_generate_srs(3, path.to_str().unwrap()).unwrap();

        let mut original_bytes = vec![];
        original
            .write_custom(&mut original_bytes, SerdeFormat::RawBytesUnchecked)
            .unwrap();
        let mut loaded_bytes = vec![];
        loaded
            .write_custom(&mut loaded_bytes, SerdeFormat::RawBytesUnchecked)
            .unwrap();
        assert_eq!(
            original_bytes, loaded_bytes,
            "loaded SRS must match the persisted one"
        );
        fs::remove_dir_all(&dir).ok();
    }

    #[test]
    fn load_or_generate_srs_generates_and_persists_when_file_is_missing() {
        let dir = std::env::temp_dir().join("mithril-test-srs-generates");
        let path = dir.join("params_kzg_unsafe_3");
        fs::remove_dir_all(&dir).ok();

        assert!(!path.exists());
        let _srs = load_or_generate_srs(3, path.to_str().unwrap()).unwrap();

        assert!(
            path.exists(),
            "generated SRS should have been persisted to disk"
        );
        assert!(fs::metadata(&path).unwrap().len() > 0);
        fs::remove_dir_all(&dir).ok();
    }

    #[test]
    fn get_or_build_snark_keys_caching_behavior() {
        let params = default_params();
        let circuit_a = StmCertificateCircuit::try_new(&params, 2).unwrap();
        let circuit_b = StmCertificateCircuit::try_new(&params, 3).unwrap();
        let degree = MidnightCircuit::from_relation(&circuit_a).min_k();
        let srs = ParamsKZG::unsafe_setup(degree, ChaCha20Rng::seed_from_u64(42));
        let key_a = SnarkSetupCacheKey {
            circuit_degree: degree,
            k: params.k,
            m: params.m,
            merkle_tree_depth: 2,
        };
        let key_b = SnarkSetupCacheKey {
            circuit_degree: degree,
            k: params.k,
            m: params.m,
            merkle_tree_depth: 3,
        };

        let pair_a1 = get_or_build_snark_keys(key_a, &circuit_a, &srs).unwrap();
        let pair_a2 = get_or_build_snark_keys(key_a, &circuit_a, &srs).unwrap();
        assert!(
            Arc::ptr_eq(&pair_a1, &pair_a2),
            "same key should return the cached Arc"
        );

        let pair_b = get_or_build_snark_keys(key_b, &circuit_b, &srs).unwrap();
        assert!(
            !Arc::ptr_eq(&pair_a1, &pair_b),
            "different key must produce a different Arc"
        );
    }

    #[test]
    fn try_new_succeeds_with_valid_parameters() {
        let result = SnarkSetup::try_new(&default_params(), 4);
        assert!(result.is_ok());
    }

    #[test]
    fn try_new_returns_same_verification_key_for_same_parameters() {
        let params = default_params();
        let setup1 = SnarkSetup::try_new(&params, 4).unwrap();
        let setup2 = SnarkSetup::try_new(&params, 4).unwrap();

        let mut vk_bytes1 = vec![];
        setup1
            .verification_key
            .write(&mut vk_bytes1, SerdeFormat::RawBytes)
            .unwrap();
        let mut vk_bytes2 = vec![];
        setup2
            .verification_key
            .write(&mut vk_bytes2, SerdeFormat::RawBytes)
            .unwrap();

        assert_eq!(
            vk_bytes1, vk_bytes2,
            "same parameters must produce the same verification key"
        );
    }

    #[test]
    fn golden_snark_verifier_params_bytes() {
        let bytes = compute_snark_verifier_params_bytes().unwrap();
        println!("SNARK_VERIFIER_PARAMS_BYTES len = {}", bytes.len());
        println!("SNARK_VERIFIER_PARAMS_BYTES hex = {}", hex::encode(&bytes));

        assert_eq!(
            bytes.as_slice(),
            &SNARK_VERIFIER_PARAMS_BYTES,
            "computed verifier params bytes do not match the hard-coded constant"
        );

        let _ = SnarkVerifierSetup::try_new().expect("verifier setup must build from constant");
    }
}
