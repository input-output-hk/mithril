//! A module used to retrieve the Certificate Chain created by an aggregator
//!
use async_trait::async_trait;
use thiserror::Error;

use crate::entities::Certificate;

#[cfg(test)]
use mockall::automock;
/// [CertificateRetriever] related errors.
#[derive(Error, Debug)]
pub enum CertificateRetrieverError {
    /// Error raised when a [CertificateRetriever] tries to retrieve a [certificate](https://mithril.network/mithril-common/doc/mithril_common/entities/struct.Certificate.html)
    #[error("general error: '{0}'")]
    General(String),
}

/// CertificateRetriever is in charge of retrieving a Certificate given its hash
#[cfg_attr(test, automock)]
#[async_trait]
pub trait CertificateRetriever: Sync + Send {
    /// Get certificate details
    async fn get_certificate_details(
        &self,
        certificate_hash: &str,
    ) -> Result<Certificate, CertificateRetrieverError>;
}
