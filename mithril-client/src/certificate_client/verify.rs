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
#[cfg(feature = "unstable")]
use crate::certificate_client::CertificateVerifierCache;
use crate::certificate_client::{CertificateClient, CertificateVerifier};
use crate::feedback::{FeedbackSender, MithrilEvent};
use crate::{MithrilCertificate, MithrilResult};

#[inline]
pub(super) async fn verify_chain(
    client: &CertificateClient,
    certificate_hash: &str,
) -> MithrilResult<MithrilCertificate> {
    let certificate = client
        .retriever
        .get(certificate_hash)
        .await?
        .ok_or(anyhow!(
            "No certificate exist for hash '{certificate_hash}'"
        ))?;

    client
        .verifier
        .verify_chain(&certificate)
        .await
        .with_context(|| {
            format!("Certificate chain of certificate '{certificate_hash}' is invalid")
        })?;

    Ok(certificate)
}

/// Implementation of a [CertificateVerifier] that can send feedbacks using
/// the [feedback][crate::feedback] mechanism.
pub struct MithrilCertificateVerifier {
    internal_verifier: Arc<dyn CommonCertificateVerifier>,
    genesis_verification_key: ProtocolGenesisVerificationKey,
    feedback_sender: FeedbackSender,
    #[cfg(feature = "unstable")]
    _verifier_cache: Option<Arc<dyn CertificateVerifierCache>>,
}

impl MithrilCertificateVerifier {
    /// Constructs a new `MithrilCertificateVerifier`.
    pub fn new(
        aggregator_client: Arc<dyn AggregatorClient>,
        genesis_verification_key: &str,
        feedback_sender: FeedbackSender,
        #[cfg(feature = "unstable")] verifier_cache: Option<Arc<dyn CertificateVerifierCache>>,
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
            #[cfg(feature = "unstable")]
            _verifier_cache: verifier_cache,
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
    use mithril_common::test_utils::CertificateChainBuilder;

    use crate::certificate_client::tests_utils::CertificateClientTestBuilder;
    use crate::feedback::StackFeedbackReceiver;

    use super::*;

    #[tokio::test]
    async fn validating_chain_send_feedbacks() {
        let (chain, verifier) = CertificateChainBuilder::new()
            .with_total_certificates(3)
            .with_certificates_per_epoch(1)
            .build();
        let last_certificate_hash = chain.first().unwrap().hash.clone();

        let feedback_receiver = Arc::new(StackFeedbackReceiver::new());
        let certificate_client = CertificateClientTestBuilder::default()
            .config_aggregator_client_mock(|mock| mock.expect_certificate_chain(chain.clone()))
            .with_genesis_verification_key(verifier.to_verification_key())
            .add_feedback_receiver(feedback_receiver.clone())
            .build();

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
        let (chain, verifier) = CertificateChainBuilder::new()
            .with_total_certificates(3)
            .with_certificates_per_epoch(1)
            .build();
        let last_certificate_hash = chain.first().unwrap().hash.clone();

        let certificate_client = CertificateClientTestBuilder::default()
            .config_aggregator_client_mock(|mock| mock.expect_certificate_chain(chain.clone()))
            .with_genesis_verification_key(verifier.to_verification_key())
            .build();

        let certificate = certificate_client
            .verify_chain(&last_certificate_hash)
            .await
            .expect("Chain validation should succeed");

        assert_eq!(certificate.hash, last_certificate_hash);
    }
}
