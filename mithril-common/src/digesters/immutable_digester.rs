use crate::{
    digesters::ImmutableFileListingError,
    entities::{Beacon, ImmutableFileNumber},
};
use async_trait::async_trait;
use std::{
    io,
    path::{Path, PathBuf},
};
use thiserror::Error;

/// A digester than can compute the digest used for mithril signatures
///
/// If you want to mock it using mockall:
/// ```
/// mod test {
///     use async_trait::async_trait;
///     use mithril_common::digesters::{ImmutableDigester, ImmutableDigesterError};
///     use mithril_common::entities::Beacon;
///     use mockall::mock;
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
///               beacon: &Beacon,
///             ) -> Result<String, ImmutableDigesterError>;
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
///     }
/// }
/// ```
#[async_trait]
pub trait ImmutableDigester: Sync + Send {
    /// Compute the digest
    async fn compute_digest(
        &self,
        dirpath: &Path,
        beacon: &Beacon,
    ) -> Result<String, ImmutableDigesterError>;
}

/// [ImmutableDigester] related Errors.
#[derive(Error, Debug)]
pub enum ImmutableDigesterError {
    /// Error raised when the files listing failed.
    #[error("Immutable files listing failed: {0}")]
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
    #[error("Digest computation failed: {0}")]
    DigestComputationError(#[from] io::Error),
}
