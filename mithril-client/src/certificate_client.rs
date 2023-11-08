//! This module defines how to exchange and validate certificate information with an Aggregator.
//!
//! To do so it defines a [CertificateClient] exposes the following features:
//!  - [get][CertificateClient::get]: get a certificate data from its hash
//!  - [list][CertificateClient::list]: get the list of available certificates
//!  - [verify_chain][CertificateClient::verify_chain]: verify a certificate chain
//!
//! # Get a certificate
//!
//! To get a certificate using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # use mithril_client::client::ClientBuilder;
//! # use mithril_client::MithrilResult;
//! #
//! # #[tokio::main]
//! # async fn main() -> MithrilResult<()> {
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
//! # use mithril_client::client::ClientBuilder;
//! # use mithril_client::MithrilResult;
//! #
//! # #[tokio::main]
//! # async fn main() -> MithrilResult<()> {
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
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
//! # use mithril_client::client::ClientBuilder;
//! # use mithril_client::MithrilResult;
//! #
//! # #[tokio::main]
//! # async fn main() -> MithrilResult<()> {
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let certificate = client.certificate().verify_chain("CERTIFICATE_HASH").await?;
//!
//! println!("Chain of Certificate (hash: {}) is valid", certificate.hash);
//! #    Ok(())
//! # }
//! ```

use std::sync::Arc;

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use slog::{crit, debug, Logger};

use crate::aggregator_client::{AggregatorClient, AggregatorClientError, AggregatorRequest};
use crate::feedback::{FeedbackSender, MithrilEvent};
use crate::{MithrilCertificate, MithrilCertificateListItem, MithrilResult};
use mithril_common::crypto_helper::ProtocolGenesisVerificationKey;
use mithril_common::{
    certificate_chain::{
        CertificateRetriever, CertificateRetrieverError,
        CertificateVerifier as CommonCertificateVerifier,
        MithrilCertificateVerifier as CommonMithrilCertificateVerifier,
    },
    entities::Certificate,
    messages::CertificateMessage,
};

#[cfg(test)]
use mockall::automock;

/// Aggregator client for the Certificate
pub struct CertificateClient {
    aggregator_client: Arc<dyn AggregatorClient>,
    retriever: Arc<InternalCertificateRetriever>,
    verifier: Arc<dyn CertificateVerifier>,
}

#[cfg_attr(test, automock)]
#[async_trait]
pub trait CertificateVerifier: Sync + Send {
    /// Validate the chain starting with the certificate.
    async fn verify_chain(&self, certificate: &MithrilCertificate) -> MithrilResult<()>;
}

impl CertificateClient {
    /// Constructor
    pub fn new(
        aggregator_client: Arc<dyn AggregatorClient>,
        verifier: Arc<dyn CertificateVerifier>,
        logger: Logger,
    ) -> Self {
        let retriever = Arc::new(InternalCertificateRetriever {
            aggregator_client: aggregator_client.clone(),
            logger,
        });

        Self {
            aggregator_client,
            retriever,
            verifier,
        }
    }

    /// Fetch a list of signed certificates
    pub async fn list(&self) -> MithrilResult<Vec<MithrilCertificateListItem>> {
        let response = self
            .aggregator_client
            .get_content(AggregatorRequest::ListCertificates)
            .await
            .with_context(|| "CertificateClient can not get the certificate list")?;
        let items = serde_json::from_str::<Vec<MithrilCertificateListItem>>(&response)
            .with_context(|| "CertificateClient can not deserialize certificate list")?;

        Ok(items)
    }

    /// Get a single certificate full information from the aggregator.
    pub async fn get(&self, certificate_hash: &str) -> MithrilResult<Option<MithrilCertificate>> {
        self.retriever.get(certificate_hash).await
    }

    /// Validate the chain starting with the certificate with given `certificate_hash`, return the certificate if
    /// chain valid.
    ///
    /// This method will fails if no certicate exists with the given `certificate_hash`.
    pub async fn verify_chain(&self, certificate_hash: &str) -> MithrilResult<MithrilCertificate> {
        let certificate = self.retriever.get(certificate_hash).await?.ok_or(anyhow!(
            "No certificate exist for hash '{certificate_hash}'"
        ))?;

        self.verifier
            .verify_chain(&certificate)
            .await
            .with_context(|| {
                format!("Certicate chain of certificate '{certificate_hash}' is invalid")
            })?;

        Ok(certificate)
    }
}

/// Internal type to implement the [InternalCertificateRetriever] trait and avoid a circular
/// dependency between the [CertificateClient] and the [MithrilCertificateVerifier] that need
/// a [CertificateRetriever] as a dependency.
struct InternalCertificateRetriever {
    aggregator_client: Arc<dyn AggregatorClient>,
    logger: Logger,
}

impl InternalCertificateRetriever {
    async fn get(&self, certificate_hash: &str) -> MithrilResult<Option<MithrilCertificate>> {
        let response = self
            .aggregator_client
            .get_content(AggregatorRequest::GetCertificate {
                hash: certificate_hash.to_string(),
            })
            .await;

        match response {
            Err(AggregatorClientError::RemoteServerLogical(_)) => Ok(None),
            Err(e) => Err(e.into()),
            Ok(response) => {
                let message =
                    serde_json::from_str::<CertificateMessage>(&response).map_err(|e| {
                        crit!(
                            self.logger,
                            "Could not create certificate from API message: {e}."
                        );
                        debug!(self.logger, "Certificate message = {response}");
                        e
                    })?;

                Ok(Some(message))
            }
        }
    }
}

pub struct MithrilCertificateVerifier {
    internal_verifier: Arc<dyn CommonCertificateVerifier>,
    genesis_verification_key: ProtocolGenesisVerificationKey,
    feedback_sender: FeedbackSender,
}

impl MithrilCertificateVerifier {
    pub fn new(
        aggregator_client: Arc<dyn AggregatorClient>,
        genesis_verification_key: &str,
        feedback_sender: FeedbackSender,
        logger: Logger,
    ) -> MithrilResult<MithrilCertificateVerifier> {
        let retriever = Arc::new(InternalCertificateRetriever {
            aggregator_client: aggregator_client.clone(),
            logger: logger.clone(),
        });
        let internal_verifier = Arc::new(CommonMithrilCertificateVerifier::new(
            logger,
            retriever.clone(),
        ));
        let genesis_verification_key =
            ProtocolGenesisVerificationKey::try_from(genesis_verification_key)
                .with_context(|| "Invalid genesis verification key")?;

        Ok(Self {
            internal_verifier,
            genesis_verification_key,
            feedback_sender,
        })
    }
}

#[async_trait]
impl CertificateVerifier for MithrilCertificateVerifier {
    async fn verify_chain(&self, certificate: &MithrilCertificate) -> MithrilResult<()> {
        let certificate_chain_validation_id = MithrilEvent::new_certificate_chain_validation_id();
        self.feedback_sender
            .send_event(MithrilEvent::CertificateChainValidationStarted {
                certificate_chain_validation_id: certificate_chain_validation_id.clone(),
            })
            .await;

        let mut current_certificate = certificate.clone().try_into()?;
        loop {
            let previous_or_none = self
                .internal_verifier
                .verify_certificate(&current_certificate, &self.genesis_verification_key)
                .await?;

            self.feedback_sender
                .send_event(MithrilEvent::CertificateValidated {
                    certificate_hash: current_certificate.hash.clone(),
                    certificate_chain_validation_id: certificate_chain_validation_id.clone(),
                })
                .await;

            match previous_or_none {
                Some(previous_certificate) => current_certificate = previous_certificate,
                None => break,
            }
        }

        self.feedback_sender
            .send_event(MithrilEvent::CertificateChainValidated {
                certificate_chain_validation_id,
            })
            .await;

        Ok(())
    }
}

#[async_trait]
impl CertificateRetriever for InternalCertificateRetriever {
    async fn get_certificate_details(
        &self,
        certificate_hash: &str,
    ) -> Result<Certificate, CertificateRetrieverError> {
        self.get(certificate_hash)
            .await
            .map_err(CertificateRetrieverError)?
            .map(|message| message.try_into())
            .transpose()
            .map_err(CertificateRetrieverError)?
            .ok_or(CertificateRetrieverError(anyhow!(format!(
                "Certificate does not exist: '{}'",
                certificate_hash
            ))))
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::crypto_helper::tests_setup::setup_certificate_chain;
    use mithril_common::test_utils::fake_data;
    use mockall::predicate::eq;

    use crate::aggregator_client::MockAggregatorHTTPClient;
    use crate::feedback::StackFeedbackReceiver;
    use crate::test_utils;

    use super::*;

    fn build_client(
        aggregator_client: Arc<dyn AggregatorClient>,
        verifier: Option<Arc<dyn CertificateVerifier>>,
    ) -> CertificateClient {
        CertificateClient::new(
            aggregator_client,
            verifier.unwrap_or(Arc::new(MockCertificateVerifier::new())),
            test_utils::test_logger(),
        )
    }

    #[tokio::test]
    async fn get_certificate_list() {
        let expected = vec![
            MithrilCertificateListItem {
                hash: "cert-hash-123".to_string(),
                ..MithrilCertificateListItem::dummy()
            },
            MithrilCertificateListItem {
                hash: "cert-hash-456".to_string(),
                ..MithrilCertificateListItem::dummy()
            },
        ];
        let message = expected.clone();
        let mut aggregator_client = MockAggregatorHTTPClient::new();
        aggregator_client
            .expect_get_content()
            .return_once(move |_| Ok(serde_json::to_string(&message).unwrap()));
        let certificate_client = build_client(Arc::new(aggregator_client), None);
        let items = certificate_client.list().await.unwrap();

        assert_eq!(expected, items);
    }

    #[tokio::test]
    async fn get_certificate_empty_list() {
        let mut aggregator_client = MockAggregatorHTTPClient::new();
        aggregator_client
            .expect_get_content()
            .return_once(move |_| {
                Ok(serde_json::to_string::<Vec<MithrilCertificateListItem>>(&vec![]).unwrap())
            });
        let certificate_client = build_client(Arc::new(aggregator_client), None);
        let items = certificate_client.list().await.unwrap();

        assert!(items.is_empty());
    }

    #[tokio::test]
    async fn test_show_ok_some() {
        let mut aggregator_client = MockAggregatorHTTPClient::new();
        let certificate_hash = "cert-hash-123".to_string();
        let certificate = fake_data::certificate(certificate_hash.clone());
        let expected_certificate = certificate.clone();
        aggregator_client
            .expect_get_content()
            .return_once(move |_| {
                let message: CertificateMessage = certificate.try_into().unwrap();
                Ok(serde_json::to_string(&message).unwrap())
            })
            .times(1);

        let certificate_client = build_client(Arc::new(aggregator_client), None);
        let cert = certificate_client
            .get("cert-hash-123")
            .await
            .unwrap()
            .expect("The certificate should be found")
            .try_into()
            .unwrap();

        assert_eq!(expected_certificate, cert);
    }

    #[tokio::test]
    async fn test_show_ok_none() {
        let mut aggregator_client = MockAggregatorHTTPClient::new();
        aggregator_client
            .expect_get_content()
            .return_once(move |_| {
                Err(AggregatorClientError::RemoteServerLogical(anyhow!(
                    "an error"
                )))
            })
            .times(1);

        let certificate_client = build_client(Arc::new(aggregator_client), None);
        assert!(certificate_client
            .get("cert-hash-123")
            .await
            .unwrap()
            .is_none());
    }

    #[tokio::test]
    async fn test_show_ko() {
        let mut aggregator_client = MockAggregatorHTTPClient::new();
        aggregator_client
            .expect_get_content()
            .return_once(move |_| {
                Err(AggregatorClientError::RemoteServerTechnical(anyhow!(
                    "an error"
                )))
            })
            .times(1);

        let certificate_client = build_client(Arc::new(aggregator_client), None);
        certificate_client
            .get("cert-hash-123")
            .await
            .expect_err("The certificate client should fail here.");
    }

    #[tokio::test]
    async fn validating_chain_send_feedbacks() {
        let (chain, verifier) = setup_certificate_chain(3, 1);
        let verification_key: String = verifier.to_verification_key().try_into().unwrap();
        let mut aggregator_client = MockAggregatorHTTPClient::new();
        let last_certificate_hash = chain.first().unwrap().hash.clone();

        for certificate in chain.clone() {
            let hash = certificate.hash.clone();
            let message = serde_json::to_string(
                &TryInto::<CertificateMessage>::try_into(certificate).unwrap(),
            )
            .unwrap();
            aggregator_client
                .expect_get_content()
                .with(eq(AggregatorRequest::GetCertificate { hash }))
                .returning(move |_| Ok(message.to_owned()));
        }

        let aggregator_client = Arc::new(aggregator_client);
        let feedback_receiver = Arc::new(StackFeedbackReceiver::new());
        let certificate_client = build_client(
            aggregator_client.clone(),
            Some(Arc::new(
                MithrilCertificateVerifier::new(
                    aggregator_client,
                    &verification_key,
                    FeedbackSender::new(&[feedback_receiver.clone()]),
                    test_utils::test_logger(),
                )
                .unwrap(),
            )),
        );

        certificate_client
            .verify_chain(&last_certificate_hash)
            .await
            .expect("Chain validation should succeed");

        let actual = feedback_receiver.stacked_events();
        let id = actual[0].event_id();

        let expected = {
            let mut vec = vec![MithrilEvent::CertificateChainValidationStarted {
                certificate_chain_validation_id: id.to_string(),
            }];
            vec.extend(
                chain
                    .into_iter()
                    .map(|c| MithrilEvent::CertificateValidated {
                        certificate_chain_validation_id: id.to_string(),
                        certificate_hash: c.hash,
                    }),
            );
            vec.push(MithrilEvent::CertificateChainValidated {
                certificate_chain_validation_id: id.to_string(),
            });
            vec
        };

        assert_eq!(actual, expected);
    }

    #[tokio::test]
    async fn verify_chain_return_certificate_with_given_hash() {
        let (chain, verifier) = setup_certificate_chain(3, 1);
        let verification_key: String = verifier.to_verification_key().try_into().unwrap();
        let mut aggregator_client = MockAggregatorHTTPClient::new();
        let last_certificate_hash = chain.first().unwrap().hash.clone();

        for certificate in chain.clone() {
            let hash = certificate.hash.clone();
            let message = serde_json::to_string(
                &TryInto::<CertificateMessage>::try_into(certificate).unwrap(),
            )
            .unwrap();
            aggregator_client
                .expect_get_content()
                .with(eq(AggregatorRequest::GetCertificate { hash }))
                .returning(move |_| Ok(message.to_owned()));
        }

        let aggregator_client = Arc::new(aggregator_client);
        let certificate_client = build_client(
            aggregator_client.clone(),
            Some(Arc::new(
                MithrilCertificateVerifier::new(
                    aggregator_client,
                    &verification_key,
                    FeedbackSender::new(&[]),
                    test_utils::test_logger(),
                )
                .unwrap(),
            )),
        );

        let certificate = certificate_client
            .verify_chain(&last_certificate_hash)
            .await
            .expect("Chain validation should succeed");

        assert_eq!(certificate.hash, last_certificate_hash);
    }
}
