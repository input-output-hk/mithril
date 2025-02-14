use async_trait::async_trait;
use sha2::Sha256;
use slog::{info, Logger};
use std::{
    collections::BTreeMap,
    io,
    ops::RangeInclusive,
    path::{Path, PathBuf},
};
use thiserror::Error;

use crate::{
    crypto_helper::{MKTree, MKTreeStoreInMemory},
    digesters::ImmutableFileListingError,
    entities::{CardanoDbBeacon, HexEncodedDigest, ImmutableFileName, ImmutableFileNumber},
    StdError,
};

use super::ImmutableFile;

/// A digester than can compute the digest used for mithril signatures
///
/// If you want to mock it using mockall:
/// ```
/// mod test {
///     use async_trait::async_trait;
///     use mithril_common::digesters::{ComputedImmutablesDigests, ImmutableDigester, ImmutableDigesterError};
///     use mithril_common::entities::{CardanoDbBeacon, ImmutableFileNumber};
///     use mithril_common::crypto_helper::{MKTree, MKTreeStoreInMemory};
///     use anyhow::anyhow;
///     use mockall::mock;
///     use std::ops::RangeInclusive;
///     use std::path::Path;
///
///     mock! {
///         pub ImmutableDigesterImpl { }
///
///         #[async_trait]
///         impl ImmutableDigester for ImmutableDigesterImpl {
///             async fn compute_digest(
///               &self,
///               dirpath: &Path,
///               beacon: &CardanoDbBeacon,
///             ) -> Result<String, ImmutableDigesterError>;
///
///             async fn compute_digests_for_range(
///               &self,
///               dirpath: &Path,
///               range: &RangeInclusive<ImmutableFileNumber>,
///             ) -> Result<ComputedImmutablesDigests, ImmutableDigesterError>;
///
///            async fn compute_merkle_tree(
///               &self,
///              dirpath: &Path,
///              beacon: &CardanoDbBeacon,
///           ) -> Result<MKTree<MKTreeStoreInMemory>, ImmutableDigesterError>;
///         }
///     }
///
///     #[test]
///     fn test_mock() {
///         let mut mock = MockDigesterImpl::new();
///         mock.expect_compute_digest().return_once(|_, _| {
///             Err(ImmutableDigesterError::NotEnoughImmutable {
///                 expected_number: 3,
///                 found_number: None,
///                 db_dir: PathBuff::new(),
///             })
///         });
///         mock.expect_compute_merkle_tree().return_once(|_, _| {
///            Err(ImmutableDigesterError::MerkleTreeComputationError(anyhow!("Error")))
///         });
///     }
/// }
/// ```
#[async_trait]
pub trait ImmutableDigester: Sync + Send {
    /// Compute the digest
    async fn compute_digest(
        &self,
        dirpath: &Path,
        beacon: &CardanoDbBeacon,
    ) -> Result<String, ImmutableDigesterError>;

    /// Compute the digests for a range of immutable files
    async fn compute_digests_for_range(
        &self,
        dirpath: &Path,
        range: &RangeInclusive<ImmutableFileNumber>,
    ) -> Result<ComputedImmutablesDigests, ImmutableDigesterError>;

    /// Compute the digests merkle tree
    async fn compute_merkle_tree(
        &self,
        dirpath: &Path,
        beacon: &CardanoDbBeacon,
    ) -> Result<MKTree<MKTreeStoreInMemory>, ImmutableDigesterError>;
}

/// [ImmutableDigester] related Errors.
#[derive(Error, Debug)]
pub enum ImmutableDigesterError {
    /// Error raised when the files listing failed.
    #[error("Immutable files listing failed")]
    ListImmutablesError(#[from] ImmutableFileListingError),

    /// Error raised when there's less than the required number of completed immutables in
    /// the cardano database or even no immutable at all.
    #[error("At least two immutable chunks should exist in directory '{db_dir}': expected {expected_number} but found {found_number:?}.")]
    NotEnoughImmutable {
        /// Expected last [ImmutableFileNumber].
        expected_number: ImmutableFileNumber,
        /// Last [ImmutableFileNumber] found when listing [ImmutableFiles][crate::digesters::ImmutableFile].
        found_number: Option<ImmutableFileNumber>,
        /// A cardano node DB directory
        db_dir: PathBuf,
    },

    /// Error raised when the digest computation failed.
    #[error("Digest computation failed")]
    DigestComputationError(#[from] io::Error),

    /// Error raised when the Merkle tree computation failed.
    #[error("Merkle tree computation failed")]
    MerkleTreeComputationError(StdError),
}

/// Computed immutables digests
pub struct ComputedImmutablesDigests {
    /// A map of [ImmutableFile] to their respective digest.
    pub entries: BTreeMap<ImmutableFile, HexEncodedDigest>,
    pub(super) new_cached_entries: Vec<ImmutableFileName>,
}

impl ComputedImmutablesDigests {
    pub(super) fn compute_immutables_digests(
        entries: BTreeMap<ImmutableFile, Option<HexEncodedDigest>>,
        logger: Logger,
    ) -> Result<ComputedImmutablesDigests, io::Error> {
        let mut new_cached_entries = Vec::new();
        let mut progress = Progress {
            index: 0,
            total: entries.len(),
        };

        let mut digests = BTreeMap::new();

        for (ix, (entry, cache)) in entries.into_iter().enumerate() {
            let hash = match cache {
                None => {
                    new_cached_entries.push(entry.filename.clone());
                    hex::encode(entry.compute_raw_hash::<Sha256>()?)
                }
                Some(digest) => digest,
            };
            digests.insert(entry, hash);

            if progress.report(ix) {
                info!(logger, "Hashing: {progress}");
            }
        }

        Ok(ComputedImmutablesDigests {
            entries: digests,
            new_cached_entries,
        })
    }
}

pub(super) struct Progress {
    pub(super) index: usize,
    pub(super) total: usize,
}

impl Progress {
    pub(super) fn report(&mut self, ix: usize) -> bool {
        self.index = ix;
        (20 * ix) % self.total == 0
    }

    pub(super) fn percent(&self) -> f64 {
        (self.index as f64 * 100.0 / self.total as f64).ceil()
    }
}

impl std::fmt::Display for Progress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}/{} ({}%)", self.index, self.total, self.percent())
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn reports_progress_every_5_percent() {
        let mut progress = Progress {
            index: 0,
            total: 7000,
        };

        assert!(!progress.report(1));
        assert!(!progress.report(4));
        assert!(progress.report(350));
        assert!(!progress.report(351));
    }

    #[test]
    fn reports_progress_when_total_lower_than_20() {
        let mut progress = Progress {
            index: 0,
            total: 16,
        };

        assert!(progress.report(4));
        assert!(progress.report(12));
        assert!(!progress.report(3));
        assert!(!progress.report(15));
    }
}
