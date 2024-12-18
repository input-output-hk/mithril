use anyhow::{anyhow, Context};
use async_trait::async_trait;
use slog::{trace, Logger};
use std::sync::Arc;

use mithril_common::{
    certificate_chain::{
        CertificateRetriever, CertificateVerifier as CommonCertificateVerifier,
        MithrilCertificateVerifier as CommonMithrilCertificateVerifier,
    },
    crypto_helper::ProtocolGenesisVerificationKey,
    entities::Certificate,
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
    retriever: Arc<InternalCertificateRetriever>,
    internal_verifier: Arc<dyn CommonCertificateVerifier>,
    genesis_verification_key: ProtocolGenesisVerificationKey,
    feedback_sender: FeedbackSender,
    #[cfg(feature = "unstable")]
    verifier_cache: Option<Arc<dyn CertificateVerifierCache>>,
    logger: Logger,
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
            logger.clone(),
            retriever.clone(),
        ));
        let genesis_verification_key =
            ProtocolGenesisVerificationKey::try_from(genesis_verification_key)
                .with_context(|| "Invalid genesis verification key")?;

        Ok(Self {
            retriever,
            internal_verifier,
            genesis_verification_key,
            feedback_sender,
            #[cfg(feature = "unstable")]
            verifier_cache,
            logger,
        })
    }

    #[cfg(feature = "unstable")]
    async fn fetch_cached_previous_hash(&self, hash: &str) -> MithrilResult<Option<String>> {
        if let Some(cache) = self.verifier_cache.as_ref() {
            Ok(cache.get_previous_hash(hash).await?)
        } else {
            Ok(None)
        }
    }

    #[cfg(not(feature = "unstable"))]
    async fn fetch_cached_previous_hash(&self, _hash: &str) -> MithrilResult<Option<String>> {
        Ok(None)
    }

    async fn verify_one(
        &self,
        certificate_chain_validation_id: &str,
        certificate: CertificateToVerify,
    ) -> MithrilResult<Option<CertificateToVerify>> {
        trace!(self.logger, "Validating certificate"; "hash" => certificate.hash(), "previous_hash" => certificate.hash());
        if let Some(previous_hash) = self.fetch_cached_previous_hash(certificate.hash()).await? {
            trace!(self.logger, "Certificate fetched from cache"; "hash" => certificate.hash(), "previous_hash" => &previous_hash);
            self.feedback_sender
                .send_event(MithrilEvent::CertificateFetchedFromCache {
                    certificate_hash: certificate.hash().to_owned(),
                    certificate_chain_validation_id: certificate_chain_validation_id.to_string(),
                })
                .await;

            Ok(Some(CertificateToVerify::ToDownload {
                hash: previous_hash,
            }))
        } else {
            self.verify_not_cached_certificate(certificate_chain_validation_id, certificate)
                .await
        }
    }

    async fn verify_not_cached_certificate(
        &self,
        certificate_chain_validation_id: &str,
        certificate: CertificateToVerify,
    ) -> MithrilResult<Option<CertificateToVerify>> {
        let certificate = match certificate {
            CertificateToVerify::Downloaded { certificate } => certificate,
            CertificateToVerify::ToDownload { hash } => {
                self.retriever.get_certificate_details(&hash).await?
            }
        };

        let previous_certificate = self
            .internal_verifier
            .verify_certificate(&certificate, &self.genesis_verification_key)
            .await?;

        #[cfg(feature = "unstable")]
        if let Some(cache) = self.verifier_cache.as_ref() {
            if !certificate.is_genesis() {
                cache
                    .store_validated_certificate(&certificate.hash, &certificate.previous_hash)
                    .await?;
            }
        }

        trace!(self.logger, "Certificate validated"; "hash" => &certificate.hash, "previous_hash" => &certificate.previous_hash);
        self.feedback_sender
            .send_event(MithrilEvent::CertificateValidated {
                certificate_hash: certificate.hash,
                certificate_chain_validation_id: certificate_chain_validation_id.to_string(),
            })
            .await;

        Ok(previous_certificate.map(|cert| CertificateToVerify::Downloaded { certificate: cert }))
    }
}

enum CertificateToVerify {
    /// The certificate is already downloaded.
    Downloaded { certificate: Certificate },
    /// The certificate is not downloaded yet (since its parent was cached).
    ToDownload { hash: String },
}

impl CertificateToVerify {
    fn hash(&self) -> &str {
        match self {
            CertificateToVerify::Downloaded { certificate } => &certificate.hash,
            CertificateToVerify::ToDownload { hash } => hash,
        }
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

        // always validate the given certificate even if it was cached
        let mut current_certificate = self
            .verify_not_cached_certificate(
                &certificate_chain_validation_id,
                CertificateToVerify::Downloaded {
                    certificate: certificate.clone().try_into()?,
                },
            )
            .await?;

        loop {
            match current_certificate {
                None => break,
                Some(next) => {
                    current_certificate = self
                        .verify_one(&certificate_chain_validation_id, next)
                        .await?
                }
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

    #[cfg(feature = "unstable")]
    mod cache {
        use chrono::TimeDelta;
        use mockall::predicate::eq;

        use crate::aggregator_client::MockAggregatorHTTPClient;
        use crate::certificate_client::verify_cache::MemoryCertificateVerifierCache;
        use crate::certificate_client::MockCertificateVerifierCache;
        use crate::test_utils;

        use super::*;

        fn build_verifier_with_cache(
            aggregator_client_mock_config: impl FnOnce(&mut MockAggregatorHTTPClient),
            genesis_verification_key: ProtocolGenesisVerificationKey,
            cache: Arc<dyn CertificateVerifierCache>,
        ) -> MithrilCertificateVerifier {
            let mut aggregator_client = MockAggregatorHTTPClient::new();
            aggregator_client_mock_config(&mut aggregator_client);
            let genesis_verification_key: String = genesis_verification_key.try_into().unwrap();

            MithrilCertificateVerifier::new(
                Arc::new(aggregator_client),
                &genesis_verification_key,
                FeedbackSender::new(&[]),
                Some(cache),
                test_utils::test_logger(),
            )
            .unwrap()
        }

        #[tokio::test]
        async fn genesis_certificates_verification_result_is_not_cached() {
            let (chain, verifier) = CertificateChainBuilder::new()
                .with_total_certificates(1)
                .with_certificates_per_epoch(1)
                .build();
            let genesis_certificate = chain.last().unwrap();
            assert!(genesis_certificate.is_genesis());

            let cache = Arc::new(MemoryCertificateVerifierCache::new(TimeDelta::hours(1)));
            let verifier = build_verifier_with_cache(
                |_mock| {},
                verifier.to_verification_key(),
                cache.clone(),
            );

            verifier
                .verify_one(
                    "certificate_chain_validation_id",
                    CertificateToVerify::Downloaded {
                        certificate: genesis_certificate.clone(),
                    },
                )
                .await
                .unwrap();

            assert_eq!(
                cache
                    .get_previous_hash(&genesis_certificate.hash)
                    .await
                    .unwrap(),
                None
            );
        }

        #[tokio::test]
        async fn non_genesis_certificates_verification_result_is_cached() {
            let (chain, verifier) = CertificateChainBuilder::new()
                .with_total_certificates(2)
                .with_certificates_per_epoch(1)
                .build();
            let certificate = chain.first().unwrap();
            let genesis_certificate = chain.last().unwrap();
            assert!(!certificate.is_genesis());

            let cache = Arc::new(MemoryCertificateVerifierCache::new(TimeDelta::hours(1)));
            let verifier = build_verifier_with_cache(
                |mock| mock.expect_certificate_chain(vec![genesis_certificate.clone()]),
                verifier.to_verification_key(),
                cache.clone(),
            );

            verifier
                .verify_one(
                    "certificate_chain_validation_id",
                    CertificateToVerify::Downloaded {
                        certificate: certificate.clone(),
                    },
                )
                .await
                .unwrap();

            assert_eq!(
                cache.get_previous_hash(&certificate.hash).await.unwrap(),
                Some(certificate.previous_hash.clone())
            );
        }

        #[tokio::test]
        async fn verification_of_first_certificate_of_a_chain_should_always_fetch_it_from_network()
        {
            let (chain, verifier) = CertificateChainBuilder::new()
                .with_total_certificates(2)
                .with_certificates_per_epoch(1)
                .build();
            let first_certificate = chain.first().unwrap();

            let cache = Arc::new(
                MemoryCertificateVerifierCache::new(TimeDelta::hours(3))
                    .with_items_from_chain(&vec![first_certificate.clone()]),
            );
            let certificate_client = CertificateClientTestBuilder::default()
                .config_aggregator_client_mock(|mock| {
                    // Expect to first certificate to be fetched from the network
                    mock.expect_certificate_chain(chain.clone());
                })
                .with_genesis_verification_key(verifier.to_verification_key())
                .with_verifier_cache(cache.clone())
                .build();

            certificate_client
                .verify_chain(&first_certificate.hash)
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn verification_of_first_certificate_of_a_chain_should_not_use_cache() {
            let (chain, verifier) = CertificateChainBuilder::new()
                .with_total_certificates(2)
                .with_certificates_per_epoch(1)
                .build();
            let first_certificate = chain.first().unwrap();
            let genesis_certificate = chain.last().unwrap();
            assert!(genesis_certificate.is_genesis());

            let mut cache = MockCertificateVerifierCache::new();
            cache.expect_get_previous_hash().returning(|_| Ok(None));
            cache
                .expect_get_previous_hash()
                .with(eq(first_certificate.hash.clone()))
                .never();
            cache
                .expect_store_validated_certificate()
                .returning(|_, _| Ok(()));

            let certificate_client = CertificateClientTestBuilder::default()
                .config_aggregator_client_mock(|mock| {
                    mock.expect_certificate_chain(chain.clone());
                })
                .with_genesis_verification_key(verifier.to_verification_key())
                .with_verifier_cache(Arc::new(cache))
                .build();

            certificate_client
                .verify_chain(&first_certificate.hash)
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn verify_chain_return_certificate_with_cache() {
            let (chain, verifier) = CertificateChainBuilder::new()
                .with_total_certificates(5)
                .with_certificates_per_epoch(1)
                .build();
            let last_certificate_hash = chain.first().unwrap().hash.clone();

            // All certificates are cached except the last and the genesis (we always fetch the both)
            let cache = MemoryCertificateVerifierCache::new(TimeDelta::hours(3))
                .with_items_from_chain(&chain[1..4]);

            let certificate_client = CertificateClientTestBuilder::default()
                .config_aggregator_client_mock(|mock| {
                    mock.expect_certificate_chain(
                        [chain[0..2].to_vec(), vec![chain.last().unwrap().clone()]].concat(),
                    )
                })
                .with_genesis_verification_key(verifier.to_verification_key())
                .with_verifier_cache(Arc::new(cache))
                .build();

            let certificate = certificate_client
                .verify_chain(&last_certificate_hash)
                .await
                .unwrap();

            assert_eq!(certificate.hash, last_certificate_hash);
        }
    }
}
