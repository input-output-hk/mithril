use async_trait::async_trait;
use mithril_common::logging::LoggerExtensions;
use std::sync::Arc;

use crate::aggregator_client::AggregatorClient;
use crate::certificate_client::fetch::InternalCertificateRetriever;
use crate::certificate_client::{fetch, verify};
use crate::{MithrilCertificate, MithrilCertificateListItem, MithrilResult};

/// Aggregator client for the Certificate
pub struct CertificateClient {
    pub(super) aggregator_client: Arc<dyn AggregatorClient>,
    pub(super) retriever: Arc<InternalCertificateRetriever>,
    pub(super) verifier: Arc<dyn CertificateVerifier>,
}

impl CertificateClient {
    /// Constructs a new `CertificateClient`.
    pub fn new(
        aggregator_client: Arc<dyn AggregatorClient>,
        verifier: Arc<dyn CertificateVerifier>,
        logger: slog::Logger,
    ) -> Self {
        let logger = logger.new_with_component_name::<Self>();
        let retriever = Arc::new(InternalCertificateRetriever::new(
            aggregator_client.clone(),
            logger,
        ));

        Self {
            aggregator_client,
            retriever,
            verifier,
        }
    }

    /// Fetch a list of certificates
    pub async fn list(&self) -> MithrilResult<Vec<MithrilCertificateListItem>> {
        fetch::list(self).await
    }

    /// Get a single certificate full information from the aggregator.
    pub async fn get(&self, certificate_hash: &str) -> MithrilResult<Option<MithrilCertificate>> {
        fetch::get(self, certificate_hash).await
    }

    /// Validate the chain starting with the certificate with given `certificate_hash`, return the certificate if
    /// the chain is valid.
    ///
    /// This method will fail if no certificate exists for the given `certificate_hash`.
    pub async fn verify_chain(&self, certificate_hash: &str) -> MithrilResult<MithrilCertificate> {
        verify::verify_chain(self, certificate_hash).await
    }
}

/// API that defines how to validate certificates.
#[cfg_attr(test, mockall::automock)]
#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
pub trait CertificateVerifier: Sync + Send {
    /// Validate the chain starting with the given certificate.
    async fn verify_chain(&self, certificate: &MithrilCertificate) -> MithrilResult<()>;
}

#[cfg(feature = "unstable")]
/// API that defines how to cache certificates validation results.
#[cfg_attr(test, mockall::automock)]
#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
pub trait CertificateVerifierCache: Sync + Send {}
