//! A module used to retrieve the Certificate Chain created by an aggregator
//!
use async_trait::async_trait;

use crate::{entities::Certificate, StdError};

#[cfg(test)]
use mockall::automock;

/// [CertificateRetriever] related errors.
#[derive(Debug)]
pub struct CertificateRetrieverError(pub StdError);

impl From<StdError> for CertificateRetrieverError {
    fn from(error: StdError) -> Self {
        CertificateRetrieverError(error)
    }
}

/// CertificateRetriever is in charge of retrieving a [Certificate] given its hash
#[cfg_attr(test, automock)]
#[async_trait]
pub trait CertificateRetriever: Sync + Send {
    /// Get [Certificate] details
    async fn get_certificate_details(
        &self,
        certificate_hash: &str,
    ) -> Result<Certificate, CertificateRetrieverError>;
}
