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
};

use crate::StmResult;

use super::{
    MITHRIL_CIRCUIT_CACHE_FOLDER, halo2::NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
    halo2_ivc::RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
};

/// Outcome of [`CircuitKeyCache::validate`].
pub enum CacheState {
    /// On-disk VK matches the expected bytes and the PK file is also present.
    Valid,
    /// No VK on disk, or the cache was stale and has been cleared.
    Empty,
}

/// Owns the disk paths and expected VK bytes for one circuit's key cache.
///
/// See [`CircuitKeyCache::validate`] for the three-way decision logic.
pub struct CircuitKeyCache {
    /// Path to the on-disk verification key file.
    verification_key_path: PathBuf,
    /// Path to the on-disk proving key file.
    proving_key_path: PathBuf,
    /// Hard-coded expected VK bytes embedded in the binary, used for staleness detection.
    expected_verification_key_bytes: &'static [u8],
}

impl CircuitKeyCache {
    /// Build a cache rooted at `base_dir / MITHRIL_CIRCUIT_CACHE_FOLDER / circuit_name`.
    pub fn new(base_dir: PathBuf, circuit_name: &str, expected_vk_bytes: &'static [u8]) -> Self {
        let circuit_dir = base_dir.join(MITHRIL_CIRCUIT_CACHE_FOLDER).join(circuit_name);
        Self {
            verification_key_path: circuit_dir.join("verification-key"),
            proving_key_path: circuit_dir.join("proving-key"),
            expected_verification_key_bytes: expected_vk_bytes,
        }
    }

    /// Inspect the on-disk VK and decide whether the cache is usable.
    ///
    /// - **No VK file** → [`CacheState::Empty`] (first run or cache was cleared).
    /// - **VK matches expected bytes and PK is present** → [`CacheState::Valid`].
    /// - **VK matches but PK is missing** → [`CacheState::Empty`] (partial write; recompute).
    /// - **VK does not match** → both files are removed (stale cache after a key rotation)
    ///   and [`CacheState::Empty`] is returned. Both removals are idempotent.
    pub fn validate(&self) -> StmResult<CacheState> {
        let bytes = match fs::read(&self.verification_key_path) {
            Ok(b) => b,
            Err(e) if e.kind() == ErrorKind::NotFound => return Ok(CacheState::Empty),
            Err(e) => return Err(e.into()),
        };

        if bytes == self.expected_verification_key_bytes {
            return match fs::metadata(&self.proving_key_path) {
                Ok(_) => Ok(CacheState::Valid),
                Err(e) if e.kind() == ErrorKind::NotFound => Ok(CacheState::Empty),
                Err(e) => Err(e.into()),
            };
        }

        fs::remove_file(&self.verification_key_path).or_else(|e| {
            if e.kind() == ErrorKind::NotFound {
                Ok(())
            } else {
                Err(e)
            }
        })?;
        fs::remove_file(&self.proving_key_path).or_else(|e| {
            if e.kind() == ErrorKind::NotFound {
                Ok(())
            } else {
                Err(e)
            }
        })?;

        Ok(CacheState::Empty)
    }

    /// Returns the path to the on-disk verification key file.
    pub fn vk_path(&self) -> &Path {
        &self.verification_key_path
    }

    /// Returns the path to the on-disk proving key file.
    pub fn pk_path(&self) -> &Path {
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
        let (base_dir, cache) = make_test_cache("mithril-key-cache-test-absent");
        assert!(matches!(cache.validate().unwrap(), CacheState::Empty));
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn validate_returns_valid_when_vk_matches() {
        let (base_dir, cache) = make_test_cache("mithril-key-cache-test-match");
        fs::create_dir_all(cache.vk_path().parent().unwrap()).unwrap();
        fs::write(cache.vk_path(), b"expected-vk-bytes").unwrap();
        fs::write(cache.pk_path(), b"some-pk-bytes").unwrap();
        assert!(matches!(cache.validate().unwrap(), CacheState::Valid));
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn validate_returns_empty_when_vk_matches_but_pk_absent() {
        let (base_dir, cache) = make_test_cache("mithril-key-cache-test-pk-missing");
        fs::create_dir_all(cache.vk_path().parent().unwrap()).unwrap();
        fs::write(cache.vk_path(), b"expected-vk-bytes").unwrap();
        assert!(matches!(cache.validate().unwrap(), CacheState::Empty));
        fs::remove_dir_all(&base_dir).ok();
    }

    #[test]
    fn validate_removes_stale_files_and_returns_empty() {
        let (base_dir, cache) = make_test_cache("mithril-key-cache-test-stale");
        let vk_path = cache.vk_path().to_path_buf();
        let pk_path = cache.pk_path().to_path_buf();
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
        let (base_dir, cache) = make_test_cache("mithril-key-cache-test-pk-absent");
        let vk_path = cache.vk_path().to_path_buf();
        let pk_path = cache.pk_path().to_path_buf();
        fs::create_dir_all(vk_path.parent().unwrap()).unwrap();
        fs::write(&vk_path, b"stale-bytes").unwrap();

        assert!(matches!(cache.validate().unwrap(), CacheState::Empty));
        assert!(!vk_path.exists());
        assert!(!pk_path.exists());
        fs::remove_dir_all(&base_dir).ok();
    }

    // Both constructor tests below write to the real temp_dir() paths used by the production
    // factory methods. Parallel test runs that also exercise the production path could race
    // on the same directory. If these tests ever become flaky, switch to make_test_cache to
    // isolate each run into a unique subdirectory.
    #[test]
    fn for_non_recursive_circuit_has_correct_path_and_uses_production_vk() {
        let cache = CircuitKeyCache::for_non_recursive_circuit();

        assert!(
            cache
                .vk_path()
                .ends_with("mithril-circuit/non-recursive-keys/verification-key"),
            "vk_path should end with mithril-circuit/non-recursive-keys/verification-key"
        );
        assert!(
            cache
                .pk_path()
                .ends_with("mithril-circuit/non-recursive-keys/proving-key"),
            "pk_path should end with mithril-circuit/non-recursive-keys/proving-key"
        );

        let cleanup_dir = cache.vk_path().parent().unwrap().to_path_buf();
        fs::remove_dir_all(&cleanup_dir).ok();
        fs::create_dir_all(&cleanup_dir).unwrap();
        fs::write(
            cache.vk_path(),
            NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
        )
        .unwrap();
        fs::write(cache.pk_path(), b"any-pk-bytes").unwrap();
        assert!(
            matches!(cache.validate().unwrap(), CacheState::Valid),
            "validate() should return Valid when production VK bytes are on disk"
        );
        fs::remove_dir_all(&cleanup_dir).ok();
    }

    #[test]
    fn for_recursive_circuit_has_correct_path_and_uses_production_vk() {
        let cache = CircuitKeyCache::for_recursive_circuit();

        assert!(
            cache
                .vk_path()
                .ends_with("mithril-circuit/recursive-keys/verification-key"),
            "vk_path should end with mithril-circuit/recursive-keys/verification-key"
        );
        assert!(
            cache
                .pk_path()
                .ends_with("mithril-circuit/recursive-keys/proving-key"),
            "pk_path should end with mithril-circuit/recursive-keys/proving-key"
        );

        let cleanup_dir = cache.vk_path().parent().unwrap().to_path_buf();
        fs::remove_dir_all(&cleanup_dir).ok();
        fs::create_dir_all(&cleanup_dir).unwrap();
        fs::write(
            cache.vk_path(),
            RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
        )
        .unwrap();
        fs::write(cache.pk_path(), b"any-pk-bytes").unwrap();
        assert!(
            matches!(cache.validate().unwrap(), CacheState::Valid),
            "validate() should return Valid when production VK bytes are on disk"
        );
        fs::remove_dir_all(&cleanup_dir).ok();
    }
}
