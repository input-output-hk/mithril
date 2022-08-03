use crate::digesters::ImmutableFileListingError;
use crate::entities::ImmutableFileNumber;
use async_trait::async_trait;
use std::io;
use thiserror::Error;
use tokio::sync::RwLock;

/// Result a [Digester] computation.
#[derive(Debug, Clone, PartialEq)]
pub struct DigesterResult {
    /// The computed digest
    pub digest: String,

    /// The number of the last immutable file used to compute the digest
    pub last_immutable_file_number: ImmutableFileNumber,
}

/// [Digester] related Errors.
#[derive(Error, Debug)]
pub enum DigesterError {
    /// Error raised when the files listing failed.
    #[error("Immutable files listing failed: {0}")]
    ListImmutablesError(#[from] ImmutableFileListingError),

    /// Error raised when there's less than two immutables files available.
    #[error("At least two immutables chunk should exists")]
    NotEnoughImmutable(),

    /// Error raised when the digest computation failed.
    #[error("Digest computation failed: {0}")]
    DigestComputationError(#[from] io::Error),
}

/// A digester than can compute the digest used for mithril signatures
///
/// If you want to mock it using mockall:
/// ```
/// mod test {
///     use mithril_common::digesters::{Digester, DigesterError, DigesterResult};
///     use mockall::mock;
///     use async_trait::async_trait;
///
///
///     mock! {
///         pub DigesterImpl { }
///
///         #[async_trait]
///         impl Digester for DigesterImpl {
///             async fn compute_digest(&self) -> Result<DigesterResult, DigesterError>;
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
#[async_trait]
pub trait Digester: Sync + Send {
    /// Compute the digest
    async fn compute_digest(&self) -> Result<DigesterResult, DigesterError>;
}

/// A [Digester] returning configurable result for testing purpose.
pub struct DumbDigester {
    digest: String,
    last_immutable_number: RwLock<u64>,
    is_success: bool,
}

impl DumbDigester {
    /// DumbDigester factory
    pub fn new(digest: &str, last_immutable_number: u64, is_success: bool) -> Self {
        let digest = String::from(digest);

        Self {
            digest,
            last_immutable_number: RwLock::new(last_immutable_number),
            is_success,
        }
    }

    /// Set the stored immutable_file_number, changing the digest 'computation' result.
    pub async fn set_immutable_file_number(&self, immutable_file_number: u64) {
        let mut value = self.last_immutable_number.write().await;
        *value = immutable_file_number;
    }
}

impl Default for DumbDigester {
    fn default() -> Self {
        Self::new("1234", 119827, true)
    }
}

#[async_trait]
impl Digester for DumbDigester {
    async fn compute_digest(&self) -> Result<DigesterResult, DigesterError> {
        if self.is_success {
            Ok(DigesterResult {
                digest: self.digest.clone(),
                last_immutable_file_number: *self.last_immutable_number.read().await,
            })
        } else {
            Err(DigesterError::NotEnoughImmutable())
        }
    }
}
