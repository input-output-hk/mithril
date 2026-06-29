//! Shared, content-keyed key cache for the slow tests.
//!
//! Keygen dominates the slow tier: each test that builds a prover or verifier pays it. This helper
//! lets the whole suite share one on-disk cache under the Cargo target directory, keyed by the
//! inputs that determine the keys. The first test to need a given configuration pays keygen; every
//! later test (and every later run) reuses the cached keys.
//!
//! Two properties make this safe alongside the empty-golden cache skip:
//! - The directory is keyed by the key-derivation inputs, so distinct configurations (for example
//!   forged protocol parameters) resolve to distinct directories and never reuse one another's keys.
//! - An exclusive lock per directory serializes cold-start keygen across parallel test processes, so
//!   a given key pair is computed once rather than races between processes.

use std::{
    fs::{self, File},
    path::{Path, PathBuf},
};

use blake2b_simd::Params as Blake2bParams;
use fs2::FileExt;

use crate::StmResult;

/// Bumped whenever the cache layout or the key-derivation inputs change, so a cache written by an
/// earlier scheme is never reused.
const CACHE_SCHEMA_VERSION: &[u8] = b"v1";

/// Root directory name under the Cargo target directory.
const SHARED_CACHE_ROOT: &str = "mithril-circuit-test-cache";

/// Locates the Cargo target directory. `CARGO_MANIFEST_DIR` points at the crate, not the workspace,
/// so the target directory is its parent's `target` unless `CARGO_TARGET_DIR` overrides it.
fn shared_cache_root() -> PathBuf {
    let target_directory = std::env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .expect("the crate manifest directory always has a workspace parent")
                .join("target")
        });
    target_directory.join(SHARED_CACHE_ROOT)
}

/// Maps `(label, fingerprint)` to a directory: a readable label prefix plus a hash of the
/// fingerprint inputs. The length prefix on each input keeps the concatenation unambiguous.
pub(crate) fn shared_cache_directory(label: &str, fingerprint: &[&[u8]]) -> PathBuf {
    let mut hasher = Blake2bParams::new().hash_length(16).to_state();
    hasher.update(CACHE_SCHEMA_VERSION);
    for input in fingerprint {
        hasher.update(&(input.len() as u64).to_le_bytes());
        hasher.update(input);
    }
    shared_cache_root().join(format!("{label}-{}", hasher.finalize().to_hex()))
}

/// Runs `derive` under an exclusive lock on the content-keyed directory for `(label, fingerprint)`,
/// passing that directory so the closure can root a `KeyProvider` there and run
/// the actual key derivation under the lock.
pub(crate) fn with_shared_key_cache<T>(
    label: &str,
    fingerprint: &[&[u8]],
    derive: impl FnOnce(&Path) -> StmResult<T>,
) -> StmResult<T> {
    let cache_directory = shared_cache_directory(label, fingerprint);
    fs::create_dir_all(&cache_directory)?;
    let lock = File::create(cache_directory.join(".lock"))?;
    lock.lock_exclusive()?;
    let result = derive(&cache_directory);
    let _ = FileExt::unlock(&lock);
    result
}

#[cfg(test)]
mod tests {
    use super::shared_cache_directory;

    #[test]
    fn same_inputs_map_to_the_same_directory() {
        let first = shared_cache_directory("non-recursive", &[b"parameters", b"depth"]);
        let second = shared_cache_directory("non-recursive", &[b"parameters", b"depth"]);
        assert_eq!(first, second);
    }

    #[test]
    fn a_change_in_any_input_maps_to_a_different_directory() {
        // The certificate-key cache keys on protocol parameters, Merkle-tree depth, and the unsafe
        // seed (no SRS degree); each must change the directory when it changes.
        let baseline =
            shared_cache_directory("non-recursive", &[b"parameters-a", b"depth-4", b"seed-42"]);
        let other_parameters =
            shared_cache_directory("non-recursive", &[b"parameters-b", b"depth-4", b"seed-42"]);
        let other_depth =
            shared_cache_directory("non-recursive", &[b"parameters-a", b"depth-5", b"seed-42"]);
        let other_seed =
            shared_cache_directory("non-recursive", &[b"parameters-a", b"depth-4", b"seed-7"]);
        assert_ne!(baseline, other_parameters);
        assert_ne!(baseline, other_depth);
        assert_ne!(baseline, other_seed);

        // The IVC setup cache additionally folds in the SRS degree and both production verifying keys
        // as a circuit-version salt; a change in either must also resolve to a different directory.
        let ivc_baseline = shared_cache_directory(
            "ivc-setup",
            &[b"parameters-a", b"depth-4", b"degree-19", b"circuit-vk-1"],
        );
        let other_degree = shared_cache_directory(
            "ivc-setup",
            &[b"parameters-a", b"depth-4", b"degree-20", b"circuit-vk-1"],
        );
        let other_circuit_version = shared_cache_directory(
            "ivc-setup",
            &[b"parameters-a", b"depth-4", b"degree-19", b"circuit-vk-2"],
        );
        assert_ne!(ivc_baseline, other_degree);
        assert_ne!(ivc_baseline, other_circuit_version);
    }
}
