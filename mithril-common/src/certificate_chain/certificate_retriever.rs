//! A module used to retrieve the Certificate Chain created by an aggregator
//!
use async_trait::async_trait;
use thiserror::Error;

use crate::{entities::Certificate, StdError};

/// [CertificateRetriever] related errors.
#[derive(Debug, Error)]
#[error("Error when retrieving certificate")]
pub struct CertificateRetrieverError(#[source] pub StdError);

/// CertificateRetriever is in charge of retrieving a [Certificate] given its hash
#[cfg_attr(test, mockall::automock)]
#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
pub trait CertificateRetriever: Sync + Send {
    /// Get [Certificate] details
    async fn get_certificate_details(
        &self,
        certificate_hash: &str,
    ) -> Result<Certificate, CertificateRetrieverError>;
}
