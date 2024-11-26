//! A client which retrieves and validates certificates from an Aggregator.
//!
//! In order to do so it defines a [CertificateClient] exposes the following features:
//!  - [get][CertificateClient::get]: get a certificate data from its hash
//!  - [list][CertificateClient::list]: get the list of available certificates
//!  - [verify_chain][CertificateClient::verify_chain]: verify a certificate chain
//!
//! # Get a certificate
//!
//! To get a certificate using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let certificate = client.certificate().get("CERTIFICATE_HASH").await?.unwrap();
//!
//! println!("Certificate hash={}, signed_message={}", certificate.hash, certificate.signed_message);
//! #    Ok(())
//! # }
//! ```
//!
//! # List available certificates
//!
//! To list available certificates using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = mithril_client::ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let certificates = client.certificate().list().await?;
//!
//! for certificate in certificates {
//!     println!("Certificate hash={}, signed_message={}", certificate.hash, certificate.signed_message);
//! }
//! #    Ok(())
//! # }
//! ```
//!
//! # Validate a certificate chain
//!
//! To validate a certificate using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let certificate = client.certificate().verify_chain("CERTIFICATE_HASH").await?;
//!
//! println!("Chain of Certificate (hash: {}) is valid", certificate.hash);
//! #    Ok(())
//! # }
//! ```

mod fetch;
mod verify;
#[cfg(feature = "unstable")]
mod verify_cache;

use fetch::*;
pub use verify::*;
#[cfg(feature = "unstable")]
pub use verify_cache::*;

use crate::aggregator_client::AggregatorClient;
use mithril_common::logging::LoggerExtensions;
use std::sync::Arc;

/// Aggregator client for the Certificate
pub struct CertificateClient {
    aggregator_client: Arc<dyn AggregatorClient>,
    retriever: Arc<InternalCertificateRetriever>,
    verifier: Arc<dyn CertificateVerifier>,
    #[cfg(feature = "unstable")]
    _verifier_cache: Option<Arc<dyn CertificateVerifierCache>>,
}

impl CertificateClient {
    /// Constructs a new `CertificateClient`.
    pub fn new(
        aggregator_client: Arc<dyn AggregatorClient>,
        verifier: Arc<dyn CertificateVerifier>,
        #[cfg(feature = "unstable")] verifier_cache: Option<Arc<dyn CertificateVerifierCache>>,
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
            #[cfg(feature = "unstable")]
            _verifier_cache: verifier_cache,
        }
    }
}

#[cfg(test)]
pub mod tests_utils {
    use mockall::predicate::eq;

    use mithril_common::entities::Certificate;
    use mithril_common::messages::CertificateMessage;

    use crate::aggregator_client::{AggregatorRequest, MockAggregatorHTTPClient};

    impl MockAggregatorHTTPClient {
        pub fn expect_certificate_chain(&mut self, certificate_chain: Vec<Certificate>) {
            for certificate in certificate_chain {
                let hash = certificate.hash.clone();
                let message = serde_json::to_string(
                    &TryInto::<CertificateMessage>::try_into(certificate).unwrap(),
                )
                .unwrap();
                self.expect_get_content()
                    .with(eq(AggregatorRequest::GetCertificate { hash }))
                    .returning(move |_| Ok(message.to_owned()));
            }
        }
    }
}
