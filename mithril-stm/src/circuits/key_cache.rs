// On-disk circuit key cache with staleness detection.
//
// [`CircuitKeyCache`] guards the VK/PK files written by [`crate::proof_system`]: on each
// startup it compares the on-disk verification key against the hard-coded expected bytes
// embedded in the binary. A mismatch means the circuit was rotated; the stale files are
// removed so the caller recomputes fresh keys from the SRS.
use std::{
    fs,
    io::ErrorKind,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::Context;

use crate::StmResult;

use super::{
    MITHRIL_CIRCUIT_CACHE_FOLDER, halo2::NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
    halo2_ivc::RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
    key_serialization::CircuitKeySerialization,
};

/// Outcome of [`CircuitKeyCache::validate`].
pub enum CacheState {
    /// No VK on disk, or the cache was stale and has been cleared.
    Empty,
    /// On-disk VK matches the expected bytes, but the PK file is absent (partial cache).
    VerificationKeyOnly,
    /// On-disk VK matches the expected bytes and the PK file is also present.
    ValidKeyPair,
}

/// Owns the disk paths and expected VK bytes for one circuit's key cache.
///
/// See [`CircuitKeyCache::validate`] for the decision logic.
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

    /// Inspect the on-disk VK and decide whether the cache is usable.
    ///
    /// - **No VK file** → [`CacheState::Empty`] (first run or cache was cleared).
    /// - **VK matches expected bytes, PK present** → [`CacheState::ValidKeyPair`].
    /// - **VK matches expected bytes, PK missing** → [`CacheState::VerificationKeyOnly`].
    /// - **VK does not match** → both files are removed (stale cache after a key rotation) and
    ///   [`CacheState::Empty`] is returned. Both removals are idempotent.
    pub fn validate(&self) -> StmResult<CacheState> {
        let Some(bytes) = self.read_verification_key_bytes()? else {
            return Ok(CacheState::Empty);
        };

        if bytes.as_slice() == self.expected_verification_key_bytes.as_ref() {
            return Ok(if self.proving_key_file_exists()? {
                CacheState::ValidKeyPair
            } else {
                CacheState::VerificationKeyOnly
            });
        }

        self.clear_stale_files()?;
        Ok(CacheState::Empty)
    }

    /// Returns the cached verifying key when the VK file is present and matches the expected
    /// bytes (whether or not the PK is present), deserialized via [`CircuitKeySerialization`].
    /// Returns `None` when the cache is empty or stale.
    pub(crate) fn get_verification_key<K: CircuitKeySerialization>(&self) -> StmResult<Option<K>> {
        match self.validate()? {
            CacheState::Empty => Ok(None),
            CacheState::VerificationKeyOnly | CacheState::ValidKeyPair => {
                let Some(bytes) = self.read_verification_key_bytes()? else {
                    return Ok(None);
                };
                Ok(Some(K::deserialize_key(&bytes)?))
            }
        }
    }

    /// Returns the cached proving key only when the cache holds a fresh, complete key pair
    /// ([`CacheState::ValidKeyPair`]), deserialized via [`CircuitKeySerialization`]. Returns
    /// `None` otherwise (so the caller recomputes).
    pub(crate) fn get_proving_key<K: CircuitKeySerialization>(&self) -> StmResult<Option<K>> {
        match self.validate()? {
            CacheState::ValidKeyPair => {
                let bytes = match fs::read(&self.proving_key_path) {
                    Ok(bytes) => bytes,
                    // The PK vanished between validation and read; treat as a cache miss.
                    Err(e) if e.kind() == ErrorKind::NotFound => return Ok(None),
                    Err(e) => return Err(e.into()),
                };
                Ok(Some(K::deserialize_key(&bytes)?))
            }
            CacheState::Empty | CacheState::VerificationKeyOnly => Ok(None),
        }
    }

    /// Writes the verifying and proving key bytes to disk as a pair. If the proving-key write
    /// fails, removal of the just-written verifying key is attempted (best-effort; a cleanup
    /// error is ignored) so the next [`validate`](Self::validate) recomputes rather than trusting
    /// a VK-matches-but-PK-broken state.
    ///
    /// The caller serializes the keys via [`CircuitKeySerialization::serialize_key`].
    pub(crate) fn store_key_pair(
        &self,
        verification_key_bytes: &[u8],
        proving_key_bytes: &[u8],
    ) -> StmResult<()> {
        if let Some(parent) = self.verification_key_path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| "Failed to create the circuit key cache directory")?;
        }
        fs::write(&self.verification_key_path, verification_key_bytes)
            .with_context(|| "Failed to write the verification key to the cache")?;
        if let Err(error) = fs::write(&self.proving_key_path, proving_key_bytes) {
            let _ = fs::remove_file(&self.verification_key_path);
            return Err(error).with_context(|| "Failed to write the proving key to the cache");
        }
        Ok(())
    }

    /// Read the on-disk VK bytes, returning `None` if the file does not exist.
    fn read_verification_key_bytes(&self) -> StmResult<Option<Vec<u8>>> {
        match fs::read(&self.verification_key_path) {
            Ok(b) => Ok(Some(b)),
            Err(e) if e.kind() == ErrorKind::NotFound => Ok(None),
            Err(e) => Err(e.into()),
        }
    }

    /// Return `true` if the PK file is present on disk.
    fn proving_key_file_exists(&self) -> StmResult<bool> {
        match fs::metadata(&self.proving_key_path) {
            Ok(_) => Ok(true),
            Err(e) if e.kind() == ErrorKind::NotFound => Ok(false),
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

    use midnight_zk_stdlib::MidnightVK;

    use crate::circuits::halo2::NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION;
    use crate::circuits::halo2_ivc::RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION;

    use super::*;

    fn make_test_cache(name: &str) -> (PathBuf, CircuitKeyCache) {
        let base_dir = env::temp_dir().join(name);
        fs::remove_dir_all(&base_dir).ok();
        let cache = CircuitKeyCache::new(base_dir.clone(), "test-circuit", b"expected-vk-bytes");
        (base_dir, cache)
    }

    #[test]
    fn validate_returns_empty_when_vk_absent() {
        let (base_dir, cache) = make_test_cache(current_function!());
        assert!(matches!(cache.validate().unwrap(), CacheState::Empty));
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn validate_returns_valid_key_pair_when_vk_and_pk_present() {
        let (base_dir, cache) = make_test_cache(current_function!());
        fs::create_dir_all(cache.verification_key_path().parent().unwrap()).unwrap();
        fs::write(cache.verification_key_path(), b"expected-vk-bytes").unwrap();
        fs::write(cache.proving_key_path(), b"some-pk-bytes").unwrap();
        assert!(matches!(
            cache.validate().unwrap(),
            CacheState::ValidKeyPair
        ));
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn validate_returns_verification_key_only_when_pk_absent() {
        let (base_dir, cache) = make_test_cache(current_function!());
        fs::create_dir_all(cache.verification_key_path().parent().unwrap()).unwrap();
        fs::write(cache.verification_key_path(), b"expected-vk-bytes").unwrap();
        assert!(matches!(
            cache.validate().unwrap(),
            CacheState::VerificationKeyOnly
        ));
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn validate_removes_stale_files_and_returns_empty() {
        let (base_dir, cache) = make_test_cache(current_function!());
        let vk_path = cache.verification_key_path().to_path_buf();
        let pk_path = cache.proving_key_path().to_path_buf();
        fs::create_dir_all(vk_path.parent().unwrap()).unwrap();
        fs::write(&vk_path, b"stale-bytes").unwrap();
        fs::write(&pk_path, b"stale-pk").unwrap();

        assert!(matches!(cache.validate().unwrap(), CacheState::Empty));
        assert!(!vk_path.exists(), "stale VK should be removed");
        assert!(!pk_path.exists(), "stale PK should be removed");
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn validate_handles_absent_pk_during_stale_cleanup() {
        let (base_dir, cache) = make_test_cache(current_function!());
        let vk_path = cache.verification_key_path().to_path_buf();
        let pk_path = cache.proving_key_path().to_path_buf();
        fs::create_dir_all(vk_path.parent().unwrap()).unwrap();
        fs::write(&vk_path, b"stale-bytes").unwrap();

        assert!(matches!(cache.validate().unwrap(), CacheState::Empty));
        assert!(!vk_path.exists());
        assert!(!pk_path.exists());
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn store_key_pair_writes_both_files_and_validates_as_pair() {
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
        assert!(matches!(
            cache.validate().unwrap(),
            CacheState::ValidKeyPair
        ));
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
    fn get_proving_key_returns_none_without_full_pair() {
        // VK present and matching but no PK → VerificationKeyOnly → get_proving_key returns None.
        // The type parameter is only a witness here; no deserialization runs.
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

        let proving_key: Option<MidnightVK> = cache.get_proving_key().unwrap();
        assert!(proving_key.is_none());
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
        assert!(
            matches!(isolated_cache.validate().unwrap(), CacheState::ValidKeyPair),
            "validate() should return ValidKeyPair when production VK bytes are on disk"
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
        assert!(
            matches!(isolated_cache.validate().unwrap(), CacheState::ValidKeyPair),
            "validate() should return ValidKeyPair when production VK bytes are on disk"
        );
        fs::remove_dir_all(&base_dir).ok();
    }
}
