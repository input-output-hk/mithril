//! SNARK setup for the STM certificate circuit.
//!
//! Bundles circuit compilation and key derivation into [`SnarkSetup`], and wires in the
//! two-level cache: an in-process [`LazyLock`] for within-run reuse, and the on-disk
//! [`CircuitKeyCache`] for reuse across process restarts.
use std::{
    fs::{self, File},
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

use crate::{
    Parameters, StmResult,
    circuits::{
        halo2::circuit::StmCertificateCircuit,
        key_cache::{CacheState, CircuitKeyCache},
        trusted_setup::TrustedSetupProvider,
    },
};

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
    /// Loads the trusted SRS via [`TrustedSetupProvider`] and delegates to
    /// [`Self::try_new_with_srs`].
    pub(crate) fn try_new(params: &Parameters, merkle_tree_depth: u32) -> StmResult<Self> {
        let srs = TrustedSetupProvider::default().get_trusted_setup_parameters()?;
        Self::try_new_with_srs(params, merkle_tree_depth, srs)
    }

    /// Build a new `SnarkSetup` from protocol parameters, Merkle tree depth, and a caller-supplied
    /// SRS. The SRS must have `max_k >= circuit.min_k()`; it is downsized to the exact circuit
    /// degree before key derivation (required by `keygen_vk`).
    pub(crate) fn try_new_with_srs(
        params: &Parameters,
        merkle_tree_depth: u32,
        mut srs: ParamsKZG<Bls12>,
    ) -> StmResult<Self> {
        let circuit = StmCertificateCircuit::try_new(params, merkle_tree_depth)?;
        let circuit_degree = MidnightCircuit::from_relation(&circuit).min_k();
        zk::downsize_srs_for_relation(&mut srs, &circuit);

        let cache_key = SnarkSetupCacheKey {
            circuit_degree,
            k: params.k,
            m: params.m,
            merkle_tree_depth,
        };
        let key_pair = get_or_build_snark_keys_with_disk_cache(
            cache_key,
            &circuit,
            &srs,
            &CircuitKeyCache::for_non_recursive_circuit(),
        )?;

        Ok(Self {
            srs,
            circuit,
            verification_key: key_pair.0.clone(),
            proving_key: key_pair.1.clone(),
        })
    }
}

/// Bundles the minimal setup artifacts needed to verify SNARK proofs.
pub(crate) struct SnarkVerifierSetup {
    /// KZG verifier parameters derived from `s_g2`.
    pub(crate) verifier_params: ParamsVerifierKZG<Bls12>,
}

impl SnarkVerifierSetup {
    /// Build the verifier setup from the Midnight trusted SRS, which must match the SRS used by
    /// the prover. The SRS is loaded (and cached on disk) via [`TrustedSetupProvider`].
    pub(crate) fn try_new() -> StmResult<Self> {
        let srs = TrustedSetupProvider::default().get_trusted_setup_parameters()?;
        Ok(Self {
            verifier_params: srs.verifier_params(),
        })
    }
}

/// Load the KZG SRS from `path`, or generate one with an unsafe deterministic seed
/// if the file does not exist. When generated, the result is persisted to `path` so
/// subsequent calls can load it quickly.
#[cfg(test)]
fn load_or_generate_srs(circuit_degree: u32, path: &str) -> StmResult<ParamsKZG<Bls12>> {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

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
#[cfg(test)]
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
#[cfg(test)]
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

/// Return a cached VK/PK pair, consulting two cache levels before recomputing.
///
/// 1. **In-process (`SNARK_SETUP_KEY_CACHE`)**: a `LazyLock<RwLock>` that avoids recomputing
///    keys when `SnarkSetup::try_new` is called multiple times within the same process.
/// 2. **On-disk (`disk_cache`)**: persists across process restarts. [`CircuitKeyCache::validate`]
///    guards this level: a stale VK (left from a previous circuit version) is wiped before the
///    cached keys are used, so the prover never starts with a silently wrong key pair.
///
/// If both caches miss, the VK and PK are derived from the SRS and written to disk. The PK
/// write is wrapped so that a failure removes the already-written VK, keeping the cache
/// directory in a clean state for the next startup.
fn get_or_build_snark_keys_with_disk_cache(
    cache_key: SnarkSetupCacheKey,
    circuit: &StmCertificateCircuit,
    srs: &ParamsKZG<Bls12>,
    disk_cache: &CircuitKeyCache,
) -> StmResult<SnarkSetupKeyPair> {
    // LazyLock is the hot in-process cache; disk survives process restarts.
    if let Some(pair) = SNARK_SETUP_KEY_CACHE
        .read()
        .map_err(|_| anyhow!("SNARK setup key cache lock poisoned on read"))?
        .as_ref()
        .filter(|(k, _)| *k == cache_key)
        .map(|(_, v)| v.clone())
    {
        return Ok(pair);
    }

    let (vk, pk) = 'keys: {
        if let CacheState::Valid = disk_cache.validate()? {
            let vk = MidnightVK::read(
                &mut BufReader::new(
                    File::open(disk_cache.vk_path()).with_context(|| "Failed to open cached VK")?,
                ),
                SerdeFormat::RawBytes,
            )
            .with_context(|| "Failed to deserialize cached VK")?;
            match File::open(disk_cache.pk_path()) {
                Ok(f) => {
                    let pk = MidnightPK::<StmCertificateCircuit>::read(
                        &mut BufReader::new(f),
                        SerdeFormat::RawBytes,
                    )
                    .with_context(|| "Failed to deserialize cached PK")?;
                    break 'keys (vk, pk);
                }
                Err(e) if e.kind() == ErrorKind::NotFound => {}
                Err(e) => return Err(anyhow!("Failed to open cached PK: {e}")),
            }
        }
        let vk = zk::setup_vk(srs, circuit);
        let pk = zk::setup_pk(circuit, &vk);
        if let Some(parent) = disk_cache.vk_path().parent() {
            fs::create_dir_all(parent)
                .with_context(|| "Failed to create circuit key cache directory")?;
        }
        vk.write(
            &mut BufWriter::new(
                File::create(disk_cache.vk_path())
                    .with_context(|| "Failed to create VK cache file")?,
            ),
            SerdeFormat::RawBytes,
        )
        .with_context(|| "Failed to write VK to disk")?;
        if let Err(e) = (|| -> StmResult<()> {
            pk.write(
                &mut BufWriter::new(
                    File::create(disk_cache.pk_path())
                        .with_context(|| "Failed to create PK cache file")?,
                ),
                SerdeFormat::RawBytes,
            )
            .with_context(|| "Failed to write PK to disk")
        })() {
            let _ = fs::remove_file(disk_cache.vk_path());
            return Err(e);
        }
        (vk, pk)
    };

    let key_pair = Arc::new((vk, pk));
    *SNARK_SETUP_KEY_CACHE
        .write()
        .map_err(|_| anyhow!("SNARK setup key cache lock poisoned on write"))? =
        Some((cache_key, key_pair.clone()));

    Ok(key_pair)
}

#[cfg(test)]
mod test {
    use std::{fs, sync::Arc};

    use midnight_curves::Bls12;
    use midnight_proofs::{poly::kzg::params::ParamsKZG, utils::SerdeFormat};
    use midnight_zk_stdlib::{self as zk, MidnightCircuit};
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{
        Parameters, circuits::halo2::circuit::StmCertificateCircuit,
        circuits::key_cache::CircuitKeyCache, proof_system::halo2_snark::SnarkSetup,
    };

    use super::{
        SnarkSetupCacheKey, get_or_build_snark_keys, get_or_build_snark_keys_with_disk_cache,
        load_or_generate_srs, persist_srs,
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
    fn try_new_with_srs_succeeds_with_valid_parameters() {
        let params = default_params();
        let circuit = StmCertificateCircuit::try_new(&params, 4).unwrap();
        let degree = MidnightCircuit::from_relation(&circuit).min_k();
        let srs = ParamsKZG::unsafe_setup(degree, ChaCha20Rng::seed_from_u64(42));
        let result = SnarkSetup::try_new_with_srs(&params, 4, srs);
        assert!(result.is_ok());
    }

    #[test]
    fn try_new_with_srs_returns_same_verification_key_for_same_parameters() {
        let params = default_params();
        let circuit = StmCertificateCircuit::try_new(&params, 4).unwrap();
        let degree = MidnightCircuit::from_relation(&circuit).min_k();
        let make_srs = || ParamsKZG::unsafe_setup(degree, ChaCha20Rng::seed_from_u64(42));
        let setup1 = SnarkSetup::try_new_with_srs(&params, 4, make_srs()).unwrap();
        let setup2 = SnarkSetup::try_new_with_srs(&params, 4, make_srs()).unwrap();

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

    mod verifier_setup {
        use super::*;
        use crate::circuits::trusted_setup::TrustedSetupProvider;
        use crate::proof_system::halo2_snark::SnarkVerifierSetup;

        #[test]
        fn verifier_setup_matches_trusted_srs() {
            let setup = SnarkVerifierSetup::try_new().unwrap();
            let srs = TrustedSetupProvider::default()
                .get_trusted_setup_parameters()
                .unwrap();
            let expected = srs.verifier_params();
            let mut expected_bytes = vec![];
            expected
                .write(&mut expected_bytes, SerdeFormat::RawBytesUnchecked)
                .unwrap();
            let mut actual_bytes = vec![];
            setup
                .verifier_params
                .write(&mut actual_bytes, SerdeFormat::RawBytesUnchecked)
                .unwrap();
            assert_eq!(
                expected_bytes, actual_bytes,
                "verifier params must match the Midnight trusted SRS"
            );
        }
    }

    #[test]
    fn disk_cache_empty_writes_keys_to_disk() {
        let params = default_params();
        let circuit = StmCertificateCircuit::try_new(&params, 100).unwrap();
        let degree = MidnightCircuit::from_relation(&circuit).min_k();
        let srs = ParamsKZG::unsafe_setup(degree, ChaCha20Rng::seed_from_u64(42));
        let cache_key = SnarkSetupCacheKey {
            circuit_degree: degree,
            k: params.k,
            m: params.m,
            merkle_tree_depth: 100,
        };

        let base_dir = std::env::temp_dir().join("mithril-test-snark-disk-empty");
        fs::remove_dir_all(&base_dir).ok();
        let disk_cache = CircuitKeyCache::new(base_dir.clone(), "non-recursive", b"placeholder");

        assert!(!disk_cache.vk_path().exists());
        get_or_build_snark_keys_with_disk_cache(cache_key, &circuit, &srs, &disk_cache).unwrap();
        assert!(
            disk_cache.vk_path().exists(),
            "VK should be written to disk on cache miss"
        );
        assert!(
            disk_cache.pk_path().exists(),
            "PK should be written to disk on cache miss"
        );
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn disk_cache_valid_returns_expected_vk_bytes() {
        let params = default_params();
        let circuit = StmCertificateCircuit::try_new(&params, 101).unwrap();
        let degree = MidnightCircuit::from_relation(&circuit).min_k();
        let srs = ParamsKZG::unsafe_setup(degree, ChaCha20Rng::seed_from_u64(42));
        let cache_key = SnarkSetupCacheKey {
            circuit_degree: degree,
            k: params.k,
            m: params.m,
            merkle_tree_depth: 101,
        };

        let vk = zk::setup_vk(&srs, &circuit);
        let pk = zk::setup_pk(&circuit, &vk);
        let mut vk_bytes = vec![];
        vk.write(&mut vk_bytes, SerdeFormat::RawBytes).unwrap();
        let mut pk_bytes = vec![];
        pk.write(&mut pk_bytes, SerdeFormat::RawBytes).unwrap();

        // Box::leak converts runtime bytes to &'static [u8] for CircuitKeyCache.
        let expected: &'static [u8] = Box::leak(vk_bytes.clone().into_boxed_slice());

        let base_dir = std::env::temp_dir().join("mithril-test-snark-disk-valid");
        fs::remove_dir_all(&base_dir).ok();
        let disk_cache = CircuitKeyCache::new(base_dir.clone(), "non-recursive", expected);

        fs::create_dir_all(disk_cache.vk_path().parent().unwrap()).unwrap();
        fs::write(disk_cache.vk_path(), &vk_bytes).unwrap();
        fs::write(disk_cache.pk_path(), &pk_bytes).unwrap();

        let loaded =
            get_or_build_snark_keys_with_disk_cache(cache_key, &circuit, &srs, &disk_cache)
                .unwrap();

        let mut loaded_vk_bytes = vec![];
        loaded.0.write(&mut loaded_vk_bytes, SerdeFormat::RawBytes).unwrap();
        assert_eq!(
            loaded_vk_bytes, vk_bytes,
            "loaded VK must match what was written to disk"
        );
        fs::remove_dir_all(&base_dir).ok();
    }
}
