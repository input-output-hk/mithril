use crate::digesters::ImmutableFileListingError;
use crate::entities::ImmutableFileNumber;
use std::io;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub struct DigesterResult {
    /// The computed digest
    pub digest: String,

    /// The number of the last immutable file used to compute the digest
    pub last_immutable_file_number: ImmutableFileNumber,
}

#[derive(Error, Debug)]
pub enum DigesterError {
    #[error("Immutable files listing failed")]
    ListImmutablesError(#[from] ImmutableFileListingError),

    #[error("At least two immutables chunk should exists")]
    NotEnoughImmutable(),

    #[error("Digest computation failed:")]
    DigestComputationError(#[from] io::Error),
}

/// A digester than can compute the digest used for mithril signatures
///
/// If you want to mock it using mockall:
/// ```
/// mod test {
///     use mithril_common::digesters::{Digester, DigesterError, DigesterResult};
///     use mockall::mock;
///
///     mock! {
///         pub DigesterImpl { }
///         impl Digester for DigesterImpl {
///             fn compute_digest(&self) -> Result<DigesterResult, DigesterError>;
///         }
///     }
///
///     #[test]
///     fn test_mock() {
///         let mut mock = MockDigesterImpl::new();
///         mock.expect_compute_digest()
///             .return_once(|| Err(DigesterError::NotEnoughImmutable()));
///     }
/// }
/// ```
pub trait Digester: Sync + Send {
    fn compute_digest(&self) -> Result<DigesterResult, DigesterError>;
}
