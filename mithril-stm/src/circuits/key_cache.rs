// On-disk circuit key cache with staleness detection.
//
// [`CircuitKeyCache`] guards the VK/PK files written by [`crate::proof_system`]: on each
// startup it compares the on-disk verification key against the hard-coded expected bytes
// embedded in the binary. A mismatch means the circuit was rotated; the stale files are
// removed so the caller recomputes fresh keys from the SRS.
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

use crate::{StmResult, codec::TryFromBytes};

use super::{
    MITHRIL_CIRCUIT_CACHE_FOLDER, halo2::NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
    halo2_ivc::RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
};

/// Owns the disk paths and expected VK bytes for one circuit's key cache.
///
/// The on-disk verifying key is compared against the expected bytes to detect a stale cache after a
/// circuit rotation; see `get_verification_key` and `get_proving_key`.
pub struct CircuitKeyCache {
    /// Path to the on-disk verification key file.
    verification_key_path: PathBuf,
    /// Path to the on-disk proving key file.
    proving_key_path: PathBuf,
    /// Expected VK bytes used for staleness detection. Owned (`Arc<[u8]>`) so it can hold either
    /// the embedded production constants or runtime-derived bytes (e.g. a deterministic test SRS),
    /// without leaking the latter to `'static`.
    expected_verification_key_bytes: Arc<[u8]>,
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

impl CircuitKeyCache {
    /// Build a cache rooted at `base_dir / MITHRIL_CIRCUIT_CACHE_FOLDER / circuit_name`.
    pub fn new(base_dir: PathBuf, circuit_name: &str, expected_vk_bytes: &[u8]) -> Self {
        let circuit_dir = base_dir.join(MITHRIL_CIRCUIT_CACHE_FOLDER).join(circuit_name);
        Self {
            verification_key_path: circuit_dir.join("verification-key"),
            proving_key_path: circuit_dir.join("proving-key"),
            expected_verification_key_bytes: Arc::from(expected_vk_bytes),
        }
    }

    /// Returns the cached verifying key when it is present and fresh, deserialized via
    /// [`TryFromBytes`]. Returns `None` when the verifying key is absent, or stale after a circuit
    /// rotation — in which case the stale files are removed so the caller recomputes. Returns an
    /// error when the stored bytes fail to deserialize.
    pub(crate) fn get_verification_key<K: TryFromBytes>(&self) -> StmResult<Option<K>> {
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
    pub(crate) fn get_proving_key<K: TryFromBytes>(&self) -> StmResult<Option<K>> {
        let proving_key_bytes = match fs::read(&self.proving_key_path) {
            Ok(bytes) => bytes,
            Err(error) if error.kind() == ErrorKind::NotFound => return Ok(None),
            Err(error) => return Err(error.into()),
        };
        Ok(Some(K::try_from_bytes(&proving_key_bytes)?))
    }

    /// Writes the verifying and proving key bytes to disk as a pair: each to a per-writer-unique
    /// temporary sibling, fsynced, then renamed (the proving key first, the verifying key last),
    /// with a final directory fsync. A partial or interrupted store — including a crash whose
    /// rename metadata persists out of order — can leave an orphan key on disk; readers require
    /// both keys present and recompute otherwise, so correctness does not depend on the rename
    /// order or on crash durability. Unique temporary names let concurrent lock-free writers
    /// proceed without clobbering each other.
    ///
    /// The caller serializes the keys via [`TryToBytes::to_bytes_vec`].
    pub(crate) fn store_key_pair(
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

    /// Returns the path to the on-disk verification key file.
    pub fn verification_key_path(&self) -> &Path {
        &self.verification_key_path
    }

    /// Returns the path to the on-disk proving key file.
    pub fn proving_key_path(&self) -> &Path {
        &self.proving_key_path
    }

    /// Cache for the non-recursive STM certificate circuit, rooted at `std::env::temp_dir()`.
    pub fn for_non_recursive_circuit() -> Self {
        Self::new(
            std::env::temp_dir(),
            "non-recursive-keys",
            NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
        )
    }

    /// Cache for the recursive (IVC) circuit, rooted at `std::env::temp_dir()`.
    pub fn for_recursive_circuit() -> Self {
        Self::new(
            std::env::temp_dir(),
            "recursive-keys",
            RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
        )
    }
}

#[cfg(test)]
mod tests {
    use std::{env, fs, path::PathBuf};

    use midnight_zk_stdlib::{MidnightPK, MidnightVK};

    use crate::circuits::halo2::NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION;
    use crate::circuits::halo2::circuit::StmCertificateCircuit;
    use crate::circuits::halo2_ivc::RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION;
    use crate::proof_system::ivc_halo2_snark::PlonkVerifyingKey;

    use super::*;

    fn make_test_cache(name: &str) -> (PathBuf, CircuitKeyCache) {
        let base_dir = env::temp_dir().join(name);
        fs::remove_dir_all(&base_dir).ok();
        let cache = CircuitKeyCache::new(base_dir.clone(), "test-circuit", b"expected-vk-bytes");
        (base_dir, cache)
    }

    #[test]
    fn get_verification_key_clears_stale_files_and_returns_none() {
        let (base_dir, cache) = make_test_cache(current_function!());
        let vk_path = cache.verification_key_path().to_path_buf();
        let pk_path = cache.proving_key_path().to_path_buf();
        fs::create_dir_all(vk_path.parent().unwrap()).unwrap();
        fs::write(&vk_path, b"stale-bytes").unwrap();
        fs::write(&pk_path, b"stale-pk").unwrap();

        let result: Option<MidnightVK> = cache.get_verification_key().unwrap();
        assert!(result.is_none(), "a stale cache must be a miss");
        assert!(!vk_path.exists(), "stale VK should be removed");
        assert!(!pk_path.exists(), "stale PK should be removed");
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn get_verification_key_surfaces_deserialization_error() {
        // The on-disk VK matches the expected bytes (fresh) but is not a valid serialized key, so
        // the deserialization error is surfaced rather than masked as a miss.
        let (base_dir, cache) = make_test_cache(current_function!());
        fs::create_dir_all(cache.verification_key_path().parent().unwrap()).unwrap();
        fs::write(cache.verification_key_path(), b"expected-vk-bytes").unwrap();

        let result: StmResult<Option<MidnightVK>> = cache.get_verification_key();
        assert!(
            result.is_err(),
            "corrupt but fresh verifying key bytes must surface a deserialization error"
        );
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn store_key_pair_writes_both_files() {
        let (base_dir, cache) = make_test_cache(current_function!());
        cache.store_key_pair(b"expected-vk-bytes", b"some-pk-bytes").unwrap();
        assert_eq!(
            fs::read(cache.verification_key_path()).unwrap(),
            b"expected-vk-bytes"
        );
        assert_eq!(
            fs::read(cache.proving_key_path()).unwrap(),
            b"some-pk-bytes"
        );
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn store_key_pair_leaves_no_verification_key_when_proving_key_store_fails() {
        let (base_dir, cache) = make_test_cache(current_function!());
        // A directory at the proving-key path makes the proving-key rename fail. Because the
        // verifying key is renamed last (only after the proving key), it is never written.
        fs::create_dir_all(cache.proving_key_path()).unwrap();

        let result = cache.store_key_pair(b"vk-bytes", b"pk-bytes");

        assert!(
            result.is_err(),
            "store should fail when the proving key cannot be written"
        );
        assert!(
            !cache.verification_key_path().exists(),
            "no verification key must be left behind when the proving key store fails"
        );
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn get_verification_key_returns_none_when_empty() {
        let (base_dir, cache) = make_test_cache(current_function!());
        let result: Option<MidnightVK> = cache.get_verification_key().unwrap();
        assert!(result.is_none());
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn get_verification_key_deserializes_present_vk() {
        // Cache whose expected bytes and on-disk VK are the production certificate VK, so
        // get_verification_key validates (match) and deserializes a real MidnightVK. The PK is
        // absent (VerificationKeyOnly), which is enough for verifying-key access.
        let base_dir = env::temp_dir().join(current_function!());
        fs::remove_dir_all(&base_dir).ok();
        let cache = CircuitKeyCache::new(
            base_dir.clone(),
            "test-circuit",
            NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
        );
        fs::create_dir_all(cache.verification_key_path().parent().unwrap()).unwrap();
        fs::write(
            cache.verification_key_path(),
            NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
        )
        .unwrap();

        let verifying_key: Option<MidnightVK> = cache.get_verification_key().unwrap();
        assert!(verifying_key.is_some());
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn get_proving_key_returns_none_when_proving_key_absent() {
        let (base_dir, cache) = make_test_cache(current_function!());
        let proving_key: Option<MidnightPK<StmCertificateCircuit>> =
            cache.get_proving_key().unwrap();
        assert!(proving_key.is_none());
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn get_proving_key_surfaces_deserialization_error() {
        // A present proving key whose bytes are not a valid serialized key surfaces the
        // deserialization error.
        let (base_dir, cache) = make_test_cache(current_function!());
        fs::create_dir_all(cache.proving_key_path().parent().unwrap()).unwrap();
        fs::write(cache.proving_key_path(), b"not-a-valid-proving-key").unwrap();

        let result: StmResult<Option<MidnightPK<StmCertificateCircuit>>> = cache.get_proving_key();
        assert!(
            result.is_err(),
            "corrupt proving key bytes must surface a deserialization error"
        );
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn for_non_recursive_circuit_path_and_production_vk_validation() {
        let cache = CircuitKeyCache::for_non_recursive_circuit();

        assert!(
            cache
                .verification_key_path()
                .ends_with("mithril-circuit/non-recursive-keys/verification-key"),
            "verification_key_path should end with mithril-circuit/non-recursive-keys/verification-key"
        );
        assert!(
            cache
                .proving_key_path()
                .ends_with("mithril-circuit/non-recursive-keys/proving-key"),
            "proving_key_path should end with mithril-circuit/non-recursive-keys/proving-key"
        );

        let base_dir = env::temp_dir().join(current_function!());
        fs::remove_dir_all(&base_dir).ok();
        let isolated_cache = CircuitKeyCache::new(
            base_dir.clone(),
            "non-recursive-keys",
            NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
        );
        isolated_cache
            .store_key_pair(
                NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
                b"any-pk-bytes",
            )
            .unwrap();
        let verifying_key: Option<MidnightVK> = isolated_cache.get_verification_key().unwrap();
        assert!(
            verifying_key.is_some(),
            "production VK bytes on disk should deserialize as the verifying key"
        );
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn for_recursive_circuit_path_and_production_vk_validation() {
        let cache = CircuitKeyCache::for_recursive_circuit();

        assert!(
            cache
                .verification_key_path()
                .ends_with("mithril-circuit/recursive-keys/verification-key"),
            "verification_key_path should end with mithril-circuit/recursive-keys/verification-key"
        );
        assert!(
            cache
                .proving_key_path()
                .ends_with("mithril-circuit/recursive-keys/proving-key"),
            "proving_key_path should end with mithril-circuit/recursive-keys/proving-key"
        );

        let base_dir = env::temp_dir().join(current_function!());
        fs::remove_dir_all(&base_dir).ok();
        let isolated_cache = CircuitKeyCache::new(
            base_dir.clone(),
            "recursive-keys",
            RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
        );
        isolated_cache
            .store_key_pair(
                RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
                b"any-pk-bytes",
            )
            .unwrap();
        let verifying_key: Option<PlonkVerifyingKey> =
            isolated_cache.get_verification_key().unwrap();
        assert!(
            verifying_key.is_some(),
            "production VK bytes on disk should deserialize as the verifying key"
        );
        fs::remove_dir_all(&base_dir).ok();
    }
}
