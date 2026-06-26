// Compute-on-miss verification key provider for one circuit.
//
// Owns the on-disk cache (the VK/PK file paths and the expected VK bytes for staleness detection)
// together with a [`CircuitKeyGenerator`] circuit. On read it compares the on-disk verifying key
// against the expected bytes — a mismatch means the circuit was rotated, so the stale files are
// removed and the caller recomputes. On a miss it derives the key pair from the SRS via the circuit
// and stores it atomically.
use std::{
    fs,
    io::{ErrorKind, Write},
    path::{Path, PathBuf},
    sync::{
        Arc,
        atomic::{AtomicU64, Ordering},
    },
};

use anyhow::Context;
use midnight_curves::Bls12;
use midnight_proofs::poly::kzg::params::ParamsKZG;

use crate::StmResult;
use crate::codec::{TryFromBytes, TryToBytes};

use super::MITHRIL_CIRCUIT_CACHE_FOLDER;
use super::circuit_key_generator::CircuitKeyGenerator;

/// Provides a circuit's verifying and proving keys: an on-disk cache (with staleness detection)
/// plus the [`CircuitKeyGenerator`] circuit that computes them on a miss.
#[allow(dead_code)] // consumed by the certificate and recursive setups
pub(crate) struct CircuitVerificationKeyProvider<C: CircuitKeyGenerator> {
    /// Path to the on-disk verification key file.
    verification_key_path: PathBuf,
    /// Path to the on-disk proving key file.
    proving_key_path: PathBuf,
    /// Expected VK bytes used for staleness detection. Owned (`Arc<[u8]>`) so it can hold either the
    /// embedded production constants or runtime-derived bytes (e.g. a deterministic test SRS),
    /// without leaking the latter to `'static`.
    expected_verification_key_bytes: Arc<[u8]>,
    /// Circuit that derives the key pair on a cache miss.
    circuit: C,
}

fn remove_if_exists(path: &Path) -> StmResult<()> {
    fs::remove_file(path).or_else(|e| {
        if e.kind() == ErrorKind::NotFound {
            Ok(())
        } else {
            Err(e.into())
        }
    })
}

/// Writes `bytes` to a uniquely-named temporary sibling of `final_path`, fsyncing before returning
/// its path so the subsequent rename is atomic.
fn write_temporary_sibling(final_path: &Path, bytes: &[u8]) -> StmResult<PathBuf> {
    let temp_path = unique_temporary_path(final_path);
    let mut file = fs::File::create(&temp_path)
        .with_context(|| format!("Failed to create the temporary key file at {temp_path:?}"))?;
    file.write_all(bytes)?;
    file.sync_all()
        .with_context(|| "Failed to fsync the temporary key file before rename")?;
    Ok(temp_path)
}

/// A temporary sibling path unique to this writer, so concurrent cold-miss writers never collide.
fn unique_temporary_path(final_path: &Path) -> PathBuf {
    static WRITE_COUNTER: AtomicU64 = AtomicU64::new(0);
    let sequence = WRITE_COUNTER.fetch_add(1, Ordering::Relaxed);
    let process_id = std::process::id();
    let mut file_name = final_path.file_name().unwrap_or_default().to_os_string();
    file_name.push(format!(".{process_id}.{sequence}.temp"));
    final_path.with_file_name(file_name)
}

/// Fsyncs the directory containing `path` so the renames are durable.
fn sync_directory_of(path: &Path) -> StmResult<()> {
    if let Some(parent) = path.parent() {
        fs::File::open(parent)
            .and_then(|directory| directory.sync_all())
            .with_context(|| "Failed to fsync the cache directory after rename")?;
    }
    Ok(())
}

#[allow(dead_code)] // consumed by the certificate and recursive setups
impl<C: CircuitKeyGenerator> CircuitVerificationKeyProvider<C> {
    /// Builds a provider rooted at `base_dir / MITHRIL_CIRCUIT_CACHE_FOLDER / circuit_name`. On read,
    /// the cached verifying key is compared against `expected_verification_key_bytes` and recomputed
    /// on a mismatch; empty expected bytes therefore always recompute (no real key matches them).
    /// Keys are computed from `circuit` on a miss.
    pub(crate) fn new(
        base_dir: PathBuf,
        circuit_name: &str,
        expected_verification_key_bytes: &[u8],
        circuit: C,
    ) -> Self {
        let circuit_dir = base_dir.join(MITHRIL_CIRCUIT_CACHE_FOLDER).join(circuit_name);
        Self {
            verification_key_path: circuit_dir.join("verification-key"),
            proving_key_path: circuit_dir.join("proving-key"),
            expected_verification_key_bytes: Arc::from(expected_verification_key_bytes),
            circuit,
        }
    }

    /// The circuit this provider derives keys for.
    pub(crate) fn circuit(&self) -> &C {
        &self.circuit
    }

    /// Returns the verifying key, computing and caching the key pair from `srs` on a miss.
    pub(crate) fn verification_key(&self, srs: &ParamsKZG<Bls12>) -> StmResult<C::VerifyingKey> {
        if let Some(verification_key) = self.get_verification_key::<C::VerifyingKey>()? {
            return Ok(verification_key);
        }
        let (verification_key, _proving_key) = self.compute_and_store(srs)?;
        Ok(verification_key)
    }

    /// Returns the verifying and proving key pair, computing and caching it from `srs` on a miss.
    pub(crate) fn key_pair(
        &self,
        srs: &ParamsKZG<Bls12>,
    ) -> StmResult<(C::VerifyingKey, C::ProvingKey)> {
        // The proving key is read only once the verifying key is present, so an orphan proving key
        // left by an interrupted store is recomputed instead of surfacing a deserialization error.
        if let Some(verification_key) = self.get_verification_key::<C::VerifyingKey>()?
            && let Some(proving_key) = self.get_proving_key::<C::ProvingKey>()?
        {
            return Ok((verification_key, proving_key));
        }
        self.compute_and_store(srs)
    }

    /// Derives the key pair from `srs` via the circuit and stores it in the cache.
    fn compute_and_store(
        &self,
        srs: &ParamsKZG<Bls12>,
    ) -> StmResult<(C::VerifyingKey, C::ProvingKey)> {
        let (verification_key, proving_key) = self.circuit.generate_key_pair(srs)?;
        self.store_key_pair(
            &verification_key.to_bytes_vec()?,
            &proving_key.to_bytes_vec()?,
        )?;
        Ok((verification_key, proving_key))
    }

    /// Returns the cached verifying key when present and fresh, deserialized via [`TryFromBytes`].
    /// Returns `None` when absent, or stale after a circuit rotation — in which case the stale files
    /// are removed so the caller recomputes. Returns an error when the stored bytes fail to
    /// deserialize.
    fn get_verification_key<K: TryFromBytes>(&self) -> StmResult<Option<K>> {
        let Some(bytes) = self.read_verification_key_bytes()? else {
            return Ok(None);
        };
        if bytes.as_slice() != self.expected_verification_key_bytes.as_ref() {
            self.clear_stale_files()?;
            return Ok(None);
        }
        Ok(Some(K::try_from_bytes(&bytes)?))
    }

    /// Returns the cached proving key, deserialized via [`TryFromBytes`]. Returns `None` when the
    /// proving key file is absent, and an error when the stored bytes fail to deserialize.
    ///
    /// Callers must read this only after [`Self::get_verification_key`] has returned `Some`: a failed
    /// [`Self::store_key_pair`] can leave an orphan proving key with no verifying key, and reading it
    /// here would surface a deserialization error instead of a recomputable cache miss.
    fn get_proving_key<K: TryFromBytes>(&self) -> StmResult<Option<K>> {
        let proving_key_bytes = match fs::read(&self.proving_key_path) {
            Ok(bytes) => bytes,
            Err(error) if error.kind() == ErrorKind::NotFound => return Ok(None),
            Err(error) => return Err(error.into()),
        };
        Ok(Some(K::try_from_bytes(&proving_key_bytes)?))
    }

    /// Writes the verifying and proving key bytes to disk as a pair: each to a per-writer-unique
    /// temporary sibling, fsynced, then renamed (the proving key first, the verifying key last),
    /// with a final directory fsync. A partial or interrupted store can leave an orphan key on disk;
    /// readers require both keys present and recompute otherwise, so correctness does not depend on
    /// the rename order or on crash durability. Unique temporary names let concurrent lock-free
    /// writers proceed without clobbering each other.
    fn store_key_pair(
        &self,
        verification_key_bytes: &[u8],
        proving_key_bytes: &[u8],
    ) -> StmResult<()> {
        if let Some(parent) = self.verification_key_path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| "Failed to create the circuit key cache directory")?;
        }

        let proving_key_temp = write_temporary_sibling(&self.proving_key_path, proving_key_bytes)?;
        let verification_key_temp =
            match write_temporary_sibling(&self.verification_key_path, verification_key_bytes) {
                Ok(path) => path,
                Err(error) => {
                    let _ = fs::remove_file(&proving_key_temp);
                    return Err(error);
                }
            };

        if let Err(error) = fs::rename(&proving_key_temp, &self.proving_key_path) {
            let _ = fs::remove_file(&proving_key_temp);
            let _ = fs::remove_file(&verification_key_temp);
            return Err(error).with_context(|| "Failed to store the proving key in the cache");
        }
        if let Err(error) = fs::rename(&verification_key_temp, &self.verification_key_path) {
            let _ = fs::remove_file(&verification_key_temp);
            return Err(error).with_context(|| "Failed to store the verification key in the cache");
        }
        sync_directory_of(&self.verification_key_path)
    }

    /// Read the on-disk VK bytes, returning `None` if the file does not exist.
    fn read_verification_key_bytes(&self) -> StmResult<Option<Vec<u8>>> {
        match fs::read(&self.verification_key_path) {
            Ok(b) => Ok(Some(b)),
            Err(e) if e.kind() == ErrorKind::NotFound => Ok(None),
            Err(e) => Err(e.into()),
        }
    }

    /// Remove both key files, ignoring not-found errors (idempotent).
    fn clear_stale_files(&self) -> StmResult<()> {
        remove_if_exists(&self.verification_key_path)?;
        remove_if_exists(&self.proving_key_path)
    }
}

#[cfg(test)]
impl<C: CircuitKeyGenerator> CircuitVerificationKeyProvider<C> {
    pub(crate) fn verification_key_path(&self) -> &Path {
        &self.verification_key_path
    }

    pub(crate) fn proving_key_path(&self) -> &Path {
        &self.proving_key_path
    }
}

#[cfg(test)]
mod tests {
    use std::cell::Cell;
    use std::{env, fs};

    use midnight_curves::Bls12;
    use midnight_proofs::poly::kzg::params::ParamsKZG;
    use midnight_zk_stdlib::{MidnightPK, MidnightVK};
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use super::{CircuitKeyGenerator, CircuitVerificationKeyProvider};
    use crate::StmResult;
    use crate::circuits::halo2::NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION;
    use crate::circuits::halo2::circuit::StmCertificateCircuit;
    use crate::codec::{TryFromBytes, TryToBytes};

    /// Key backed by raw bytes, so the provider mechanics can be tested without real keygen.
    #[derive(Clone, Debug, PartialEq)]
    struct ByteKey(Vec<u8>);

    impl TryToBytes for ByteKey {
        fn to_bytes_vec(&self) -> StmResult<Vec<u8>> {
            Ok(self.0.clone())
        }
    }

    impl TryFromBytes for ByteKey {
        fn try_from_bytes(bytes: &[u8]) -> StmResult<Self> {
            Ok(Self(bytes.to_vec()))
        }
    }

    /// Generator returning fixed key bytes and counting how often it is invoked. Doubles as a stand-in
    /// circuit for the byte-level cache tests, which never invoke `generate_key_pair`.
    struct CountingGenerator {
        verification_key: Vec<u8>,
        proving_key: Vec<u8>,
        calls: Cell<u32>,
    }

    impl CountingGenerator {
        fn new(verification_key: &[u8], proving_key: &[u8]) -> Self {
            Self {
                verification_key: verification_key.to_vec(),
                proving_key: proving_key.to_vec(),
                calls: Cell::new(0),
            }
        }
    }

    impl CircuitKeyGenerator for CountingGenerator {
        type VerifyingKey = ByteKey;
        type ProvingKey = ByteKey;

        fn generate_key_pair(&self, _srs: &ParamsKZG<Bls12>) -> StmResult<(ByteKey, ByteKey)> {
            self.calls.set(self.calls.get() + 1);
            Ok((
                ByteKey(self.verification_key.clone()),
                ByteKey(self.proving_key.clone()),
            ))
        }
    }

    // The generator ignores the SRS, so the smallest constructible parameters are enough.
    fn negligible_srs() -> ParamsKZG<Bls12> {
        ParamsKZG::unsafe_setup(1, ChaCha20Rng::seed_from_u64(0))
    }

    /// A provider over a fresh temporary directory with a dummy counting generator, for the
    /// byte-level cache tests (staleness, store, deserialize). Removes any prior contents.
    fn make_test_provider(
        name: &str,
        expected_vk_bytes: &[u8],
    ) -> CircuitVerificationKeyProvider<CountingGenerator> {
        let base_dir = env::temp_dir().join(name);
        fs::remove_dir_all(&base_dir).ok();
        CircuitVerificationKeyProvider::new(
            base_dir,
            "test-circuit",
            expected_vk_bytes,
            CountingGenerator::new(expected_vk_bytes, b"pk"),
        )
    }

    #[test]
    fn cold_miss_generates_stores_and_returns_the_pair() {
        let base_dir = env::temp_dir().join(current_function!());
        fs::remove_dir_all(&base_dir).ok();
        let provider = CircuitVerificationKeyProvider::new(
            base_dir.clone(),
            "counting",
            b"vk",
            CountingGenerator::new(b"vk", b"pk"),
        );

        let (verification_key, proving_key) = provider.key_pair(&negligible_srs()).unwrap();

        assert_eq!(verification_key, ByteKey(b"vk".to_vec()));
        assert_eq!(proving_key, ByteKey(b"pk".to_vec()));
        assert_eq!(
            provider.circuit().calls.get(),
            1,
            "a cold miss must generate exactly once"
        );
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn warm_hit_reads_from_cache_without_regenerating() {
        let base_dir = env::temp_dir().join(current_function!());
        fs::remove_dir_all(&base_dir).ok();
        let provider = CircuitVerificationKeyProvider::new(
            base_dir.clone(),
            "counting",
            b"vk",
            CountingGenerator::new(b"vk", b"pk"),
        );

        provider.key_pair(&negligible_srs()).unwrap();
        let (verification_key, proving_key) = provider.key_pair(&negligible_srs()).unwrap();

        assert_eq!(verification_key, ByteKey(b"vk".to_vec()));
        assert_eq!(proving_key, ByteKey(b"pk".to_vec()));
        assert_eq!(
            provider.circuit().calls.get(),
            1,
            "the second call must hit the cache"
        );
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn missing_verifying_key_recomputes_the_pair() {
        let base_dir = env::temp_dir().join(current_function!());
        fs::remove_dir_all(&base_dir).ok();
        let provider = CircuitVerificationKeyProvider::new(
            base_dir.clone(),
            "counting",
            b"vk",
            CountingGenerator::new(b"vk", b"pk"),
        );
        provider.key_pair(&negligible_srs()).unwrap();

        // Removing the verifying key leaves an orphan proving key; the next read must recompute
        // rather than trust the orphan.
        fs::remove_file(provider.verification_key_path()).unwrap();
        provider.key_pair(&negligible_srs()).unwrap();

        assert_eq!(
            provider.circuit().calls.get(),
            2,
            "an orphan proving key must trigger a recompute"
        );
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn get_verification_key_clears_stale_files_and_returns_none() {
        let provider = make_test_provider(current_function!(), b"expected-vk-bytes");
        let vk_path = provider.verification_key_path().to_path_buf();
        let pk_path = provider.proving_key_path().to_path_buf();
        fs::create_dir_all(vk_path.parent().unwrap()).unwrap();
        fs::write(&vk_path, b"stale-bytes").unwrap();
        fs::write(&pk_path, b"stale-pk").unwrap();

        let result: Option<MidnightVK> = provider.get_verification_key().unwrap();
        assert!(result.is_none(), "a stale cache must be a miss");
        assert!(!vk_path.exists(), "stale VK should be removed");
        assert!(!pk_path.exists(), "stale PK should be removed");
        fs::remove_dir_all(vk_path.parent().unwrap()).ok();
    }

    #[test]
    fn get_verification_key_surfaces_deserialization_error() {
        // The on-disk VK matches the expected bytes (fresh) but is not a valid serialized key, so
        // the deserialization error is surfaced rather than masked as a miss.
        let provider = make_test_provider(current_function!(), b"expected-vk-bytes");
        fs::create_dir_all(provider.verification_key_path().parent().unwrap()).unwrap();
        fs::write(provider.verification_key_path(), b"expected-vk-bytes").unwrap();

        let result: StmResult<Option<MidnightVK>> = provider.get_verification_key();
        assert!(
            result.is_err(),
            "corrupt but fresh verifying key bytes must surface a deserialization error"
        );
        fs::remove_dir_all(provider.verification_key_path().parent().unwrap()).ok();
    }

    #[test]
    fn store_key_pair_writes_both_files() {
        let provider = make_test_provider(current_function!(), b"expected-vk-bytes");
        provider
            .store_key_pair(b"expected-vk-bytes", b"some-pk-bytes")
            .unwrap();
        assert_eq!(
            fs::read(provider.verification_key_path()).unwrap(),
            b"expected-vk-bytes"
        );
        assert_eq!(
            fs::read(provider.proving_key_path()).unwrap(),
            b"some-pk-bytes"
        );
        fs::remove_dir_all(provider.verification_key_path().parent().unwrap()).ok();
    }

    #[test]
    fn store_key_pair_leaves_no_verification_key_when_proving_key_store_fails() {
        let provider = make_test_provider(current_function!(), b"expected-vk-bytes");
        // A directory at the proving-key path makes the proving-key rename fail. Because the
        // verifying key is renamed last (only after the proving key), it is never written.
        fs::create_dir_all(provider.proving_key_path()).unwrap();

        let result = provider.store_key_pair(b"vk-bytes", b"pk-bytes");

        assert!(
            result.is_err(),
            "store should fail when the proving key cannot be written"
        );
        assert!(
            !provider.verification_key_path().exists(),
            "no verification key must be left behind when the proving key store fails"
        );
        fs::remove_dir_all(provider.verification_key_path().parent().unwrap()).ok();
    }

    #[test]
    fn get_proving_key_returns_none_when_proving_key_absent() {
        let provider = make_test_provider(current_function!(), b"expected-vk-bytes");
        let proving_key: Option<MidnightPK<StmCertificateCircuit>> =
            provider.get_proving_key().unwrap();
        assert!(proving_key.is_none());
        fs::remove_dir_all(provider.verification_key_path().parent().unwrap()).ok();
    }

    #[test]
    fn get_proving_key_surfaces_deserialization_error() {
        let provider = make_test_provider(current_function!(), b"expected-vk-bytes");
        fs::create_dir_all(provider.proving_key_path().parent().unwrap()).unwrap();
        fs::write(provider.proving_key_path(), b"not-a-valid-proving-key").unwrap();

        let result: StmResult<Option<MidnightPK<StmCertificateCircuit>>> =
            provider.get_proving_key();
        assert!(
            result.is_err(),
            "corrupt proving key bytes must surface a deserialization error"
        );
        fs::remove_dir_all(provider.verification_key_path().parent().unwrap()).ok();
    }

    #[test]
    fn get_verification_key_deserializes_present_production_vk() {
        // A provider whose expected bytes and on-disk VK are the production certificate VK, so
        // get_verification_key validates (match) and deserializes a real MidnightVK.
        let provider = make_test_provider(
            current_function!(),
            NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
        );
        fs::create_dir_all(provider.verification_key_path().parent().unwrap()).unwrap();
        fs::write(
            provider.verification_key_path(),
            NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
        )
        .unwrap();

        let verifying_key: Option<MidnightVK> = provider.get_verification_key().unwrap();
        assert!(
            verifying_key.is_some(),
            "production VK bytes on disk should deserialize as the verifying key"
        );
        fs::remove_dir_all(provider.verification_key_path().parent().unwrap()).ok();
    }

    #[test]
    fn new_roots_cache_paths_under_the_circuit_cache_folder() {
        let provider = make_test_provider(current_function!(), b"vk");
        assert!(
            provider
                .verification_key_path()
                .ends_with("mithril-circuit/test-circuit/verification-key"),
            "verification key path must be rooted under the circuit cache folder"
        );
        assert!(
            provider
                .proving_key_path()
                .ends_with("mithril-circuit/test-circuit/proving-key"),
            "proving key path must be rooted under the circuit cache folder"
        );
    }
}
