//! Cross-process file lock used to serialize shared on-disk work across the parallel slow-test
//! processes — chiefly cold-start SNARK keygen into the shared key cache (`super::key_cache`).
//!
//! Mirrors [`std::sync::Mutex`]: build a [`FileMutex`] for a lock-file path, call [`FileMutex::lock`]
//! to block until this process holds the exclusive lock, and keep the returned [`FileMutexGuard`]
//! alive for as long as the protected work runs — it releases the lock when dropped.

use std::{
    fs::{self, File},
    path::PathBuf,
};

use fs2::FileExt;

use crate::StmResult;

/// A cross-process mutex backed by a single lock file: one file per lock id, exclusive across
/// processes.
pub(crate) struct FileMutex {
    lock_file_path: PathBuf,
}

impl FileMutex {
    /// Identifies the mutex by its lock-file path (one file per lock id).
    pub(crate) fn new(lock_file_path: PathBuf) -> Self {
        Self { lock_file_path }
    }

    /// Blocks until this process holds the exclusive lock, returning a guard that releases it on
    /// drop. Creates the lock file and its parent directory if they are absent.
    pub(crate) fn lock(&self) -> StmResult<FileMutexGuard> {
        if let Some(parent) = self.lock_file_path.parent() {
            fs::create_dir_all(parent)?;
        }
        let lock_file = File::create(&self.lock_file_path)?;
        lock_file.lock_exclusive()?;
        Ok(FileMutexGuard { lock_file })
    }
}

/// Holds the exclusive lock until dropped, like a [`std::sync::MutexGuard`].
#[must_use = "the lock is released as soon as the guard is dropped"]
pub(crate) struct FileMutexGuard {
    lock_file: File,
}

impl Drop for FileMutexGuard {
    fn drop(&mut self) {
        let _ = FileExt::unlock(&self.lock_file);
    }
}

#[cfg(test)]
mod tests {
    use super::FileMutex;

    #[test]
    fn lock_creates_the_file_and_is_reacquirable_after_the_guard_drops() {
        // Unique per test process so this never contends with another test's lock.
        let directory =
            std::env::temp_dir().join(format!("mithril-file-mutex-test-{}", std::process::id()));
        let lock_path = directory.join(".lock");
        std::fs::remove_dir_all(&directory).ok();

        {
            let guard = FileMutex::new(lock_path.clone())
                .lock()
                .expect("the first lock should be acquired");
            assert!(lock_path.exists(), "lock() must create the lock file");
            drop(guard);
        }

        // Guard dropped → lock released → a fresh lock on the same path succeeds.
        let reacquired = FileMutex::new(lock_path)
            .lock()
            .expect("the lock should be re-acquirable once the previous guard is dropped");
        drop(reacquired);
        std::fs::remove_dir_all(&directory).ok();
    }
}
