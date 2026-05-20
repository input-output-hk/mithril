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

pub enum CacheState {
    Valid,
    Empty,
}

pub struct CircuitKeyCache {
    vk_path: PathBuf,
    pk_path: PathBuf,
    expected_vk_bytes: &'static [u8],
}

impl CircuitKeyCache {
    pub fn new(base_dir: PathBuf, circuit_name: &str, expected_vk_bytes: &'static [u8]) -> Self {
        let circuit_dir = base_dir.join(MITHRIL_CIRCUIT_CACHE_FOLDER).join(circuit_name);
        Self {
            vk_path: circuit_dir.join("verification-key"),
            pk_path: circuit_dir.join("proving-key"),
            expected_vk_bytes,
        }
    }

    pub fn validate(&self) -> StmResult<CacheState> {
        let bytes = match fs::read(&self.vk_path) {
            Ok(b) => b,
            Err(e) if e.kind() == ErrorKind::NotFound => return Ok(CacheState::Empty),
            Err(e) => return Err(e.into()),
        };

        if bytes == self.expected_vk_bytes {
            return Ok(CacheState::Valid);
        }

        fs::remove_file(&self.vk_path).or_else(|e| {
            if e.kind() == ErrorKind::NotFound {
                Ok(())
            } else {
                Err(e)
            }
        })?;
        fs::remove_file(&self.pk_path).or_else(|e| {
            if e.kind() == ErrorKind::NotFound {
                Ok(())
            } else {
                Err(e)
            }
        })?;

        Ok(CacheState::Empty)
    }

    pub fn vk_path(&self) -> &Path {
        &self.vk_path
    }

    pub fn pk_path(&self) -> &Path {
        &self.pk_path
    }

    pub fn for_non_recursive_circuit() -> Self {
        Self::new(
            std::env::temp_dir(),
            "non-recursive",
            NON_RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
        )
    }

    pub fn for_recursive_circuit() -> Self {
        Self::new(
            std::env::temp_dir(),
            "recursive",
            RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION,
        )
    }
}

#[cfg(test)]
mod tests {
    use std::{env, fs, path::PathBuf};

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
        assert!(matches!(cache.validate().unwrap(), CacheState::Valid));
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
}
