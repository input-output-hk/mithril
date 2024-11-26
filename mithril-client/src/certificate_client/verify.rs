use anyhow::{anyhow, Context};
use async_trait::async_trait;
use slog::Logger;
use std::sync::Arc;

use mithril_common::{
    certificate_chain::{
        CertificateVerifier as CommonCertificateVerifier,
        MithrilCertificateVerifier as CommonMithrilCertificateVerifier,
    },
    crypto_helper::ProtocolGenesisVerificationKey,
    logging::LoggerExtensions,
};

use crate::aggregator_client::AggregatorClient;
use crate::certificate_client::fetch::InternalCertificateRetriever;
use crate::certificate_client::CertificateClient;
use crate::feedback::{FeedbackSender, MithrilEvent};
use crate::{MithrilCertificate, MithrilResult};

/// API that defines how to validate certificates.
#[cfg_attr(test, mockall::automock)]
#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
pub trait CertificateVerifier: Sync + Send {
    /// Validate the chain starting with the given certificate.
    async fn verify_chain(&self, certificate: &MithrilCertificate) -> MithrilResult<()>;
}

impl CertificateClient {
    /// Validate the chain starting with the certificate with given `certificate_hash`, return the certificate if
    /// the chain is valid.
    ///
    /// This method will fail if no certificate exists for the given `certificate_hash`.
    pub async fn verify_chain(&self, certificate_hash: &str) -> MithrilResult<MithrilCertificate> {
        let certificate = self.retriever.get(certificate_hash).await?.ok_or(anyhow!(
            "No certificate exist for hash '{certificate_hash}'"
        ))?;

        self.verifier
            .verify_chain(&certificate)
            .await
            .with_context(|| {
                format!("Certificate chain of certificate '{certificate_hash}' is invalid")
            })?;

        Ok(certificate)
    }
}

/// Implementation of a [CertificateVerifier] that can send feedbacks using
/// the [feedback][crate::feedback] mechanism.
pub struct MithrilCertificateVerifier {
    internal_verifier: Arc<dyn CommonCertificateVerifier>,
    genesis_verification_key: ProtocolGenesisVerificationKey,
    feedback_sender: FeedbackSender,
}

impl MithrilCertificateVerifier {
    /// Constructs a new `MithrilCertificateVerifier`.
    pub fn new(
        aggregator_client: Arc<dyn AggregatorClient>,
        genesis_verification_key: &str,
        feedback_sender: FeedbackSender,
        logger: Logger,
    ) -> MithrilResult<MithrilCertificateVerifier> {
        let logger = logger.new_with_component_name::<Self>();
        let retriever = Arc::new(InternalCertificateRetriever::new(
            aggregator_client,
            logger.clone(),
        ));
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

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl CertificateVerifier for MithrilCertificateVerifier {
    async fn verify_chain(&self, certificate: &MithrilCertificate) -> MithrilResult<()> {
        // Todo: move most of this code in the `mithril_common` verifier by defining
        // a new `verify_chain` method that take a callback called when a certificate is
        // validated.
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

#[cfg(test)]
mod tests {
    use mithril_common::crypto_helper::tests_setup::setup_certificate_chain;

    use crate::aggregator_client::MockAggregatorHTTPClient;
    use crate::feedback::StackFeedbackReceiver;
    use crate::test_utils;

    use super::*;

    fn build_client(
        aggregator_client: Arc<dyn AggregatorClient>,
        verifier: Arc<dyn CertificateVerifier>,
    ) -> CertificateClient {
        CertificateClient::new(
            aggregator_client,
            verifier,
            #[cfg(feature = "unstable")]
            None,
            test_utils::test_logger(),
        )
    }

    #[tokio::test]
    async fn validating_chain_send_feedbacks() {
        let (chain, verifier) = setup_certificate_chain(3, 1);
        let verification_key: String = verifier.to_verification_key().try_into().unwrap();
        let mut aggregator_client = MockAggregatorHTTPClient::new();
        aggregator_client.expect_certificate_chain(chain.clone());
        let last_certificate_hash = chain.first().unwrap().hash.clone();

        let aggregator_client = Arc::new(aggregator_client);
        let feedback_receiver = Arc::new(StackFeedbackReceiver::new());
        let certificate_client = build_client(
            aggregator_client.clone(),
            Arc::new(
                MithrilCertificateVerifier::new(
                    aggregator_client,
                    &verification_key,
                    FeedbackSender::new(&[feedback_receiver.clone()]),
                    test_utils::test_logger(),
                )
                .unwrap(),
            ),
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
        aggregator_client.expect_certificate_chain(chain.clone());
        let last_certificate_hash = chain.first().unwrap().hash.clone();

        let aggregator_client = Arc::new(aggregator_client);
        let certificate_client = build_client(
            aggregator_client.clone(),
            Arc::new(
                MithrilCertificateVerifier::new(
                    aggregator_client,
                    &verification_key,
                    FeedbackSender::new(&[]),
                    test_utils::test_logger(),
                )
                .unwrap(),
            ),
        );

        let certificate = certificate_client
            .verify_chain(&last_certificate_hash)
            .await
            .expect("Chain validation should succeed");

        assert_eq!(certificate.hash, last_certificate_hash);
    }
}
