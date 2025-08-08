use anyhow::{Context, anyhow};
use async_trait::async_trait;
use slog::{Logger, trace};
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
#[cfg(feature = "unstable")]
use crate::certificate_client::CertificateVerifierCache;
use crate::certificate_client::fetch::InternalCertificateRetriever;
use crate::certificate_client::{CertificateClient, CertificateVerifier};
use crate::feedback::{FeedbackSender, MithrilEvent};
use crate::{MithrilCertificate, MithrilResult};

#[inline]
pub(super) async fn verify_chain(
    client: &CertificateClient,
    certificate_hash: &str,
) -> MithrilResult<MithrilCertificate> {
    let certificate = client.retriever.get(certificate_hash).await?.ok_or(anyhow!(
        "No certificate exist for hash '{certificate_hash}'"
    ))?;

    client.verifier.verify_chain(&certificate).await.with_context(|| {
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

    async fn verify_with_cache_enabled(
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
            let certificate = match certificate {
                CertificateToVerify::Downloaded { certificate } => *certificate,
                CertificateToVerify::ToDownload { hash } => {
                    self.retriever.get_certificate_details(&hash).await?
                }
            };

            let previous_certificate = self
                .verify_without_cache(certificate_chain_validation_id, certificate)
                .await?;
            Ok(previous_certificate.map(Into::into))
        }
    }

    async fn verify_without_cache(
        &self,
        certificate_chain_validation_id: &str,
        certificate: Certificate,
    ) -> MithrilResult<Option<Certificate>> {
        let previous_certificate = self
            .internal_verifier
            .verify_certificate(&certificate, &self.genesis_verification_key)
            .await?;

        #[cfg(feature = "unstable")]
        if let Some(cache) = self.verifier_cache.as_ref()
            && !certificate.is_genesis()
        {
            cache
                .store_validated_certificate(&certificate.hash, &certificate.previous_hash)
                .await?;
        }

        trace!(self.logger, "Certificate validated"; "hash" => &certificate.hash, "previous_hash" => &certificate.previous_hash);
        self.feedback_sender
            .send_event(MithrilEvent::CertificateValidated {
                certificate_hash: certificate.hash,
                certificate_chain_validation_id: certificate_chain_validation_id.to_string(),
            })
            .await;

        Ok(previous_certificate)
    }
}

enum CertificateToVerify {
    /// The certificate is already downloaded.
    Downloaded { certificate: Box<Certificate> },
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

impl From<Certificate> for CertificateToVerify {
    fn from(value: Certificate) -> Self {
        Self::Downloaded {
            certificate: Box::new(value),
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

        // Validate certificates without cache until we cross an epoch boundary
        // This is necessary to ensure that the AVK chaining is correct
        let start_epoch = certificate.epoch;
        let mut current_certificate: Option<Certificate> = Some(certificate.clone().try_into()?);
        loop {
            match current_certificate {
                None => break,
                Some(next) => {
                    current_certificate = self
                        .verify_without_cache(&certificate_chain_validation_id, next)
                        .await?;

                    let has_crossed_epoch_boundary =
                        current_certificate.as_ref().is_some_and(|c| c.epoch != start_epoch);
                    if has_crossed_epoch_boundary {
                        break;
                    }
                }
            }
        }

        let mut current_certificate: Option<CertificateToVerify> =
            current_certificate.map(Into::into);
        loop {
            match current_certificate {
                None => break,
                Some(next) => {
                    current_certificate = self
                        .verify_with_cache_enabled(&certificate_chain_validation_id, next)
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
    use mithril_common::test::builder::CertificateChainBuilder;

    use crate::certificate_client::tests_utils::CertificateClientTestBuilder;
    use crate::feedback::StackFeedbackReceiver;

    use super::*;

    #[tokio::test]
    async fn validating_chain_send_feedbacks() {
        let chain = CertificateChainBuilder::new()
            .with_total_certificates(3)
            .with_certificates_per_epoch(1)
            .build();
        let last_certificate_hash = chain.first().unwrap().hash.clone();

        let feedback_receiver = Arc::new(StackFeedbackReceiver::new());
        let certificate_client = CertificateClientTestBuilder::default()
            .config_aggregator_client_mock(|mock| {
                mock.expect_certificate_chain(chain.certificates_chained.clone())
            })
            .with_genesis_verification_key(chain.genesis_verifier.to_verification_key())
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
            vec.extend(chain.certificates_chained.into_iter().map(|c| {
                MithrilEvent::CertificateValidated {
                    certificate_chain_validation_id: id.to_string(),
                    certificate_hash: c.hash,
                }
            }));
            vec.push(MithrilEvent::CertificateChainValidated {
                certificate_chain_validation_id: id.to_string(),
            });
            vec
        };

        assert_eq!(actual, expected);
    }

    #[tokio::test]
    async fn verify_chain_return_certificate_with_given_hash() {
        let chain = CertificateChainBuilder::new()
            .with_total_certificates(3)
            .with_certificates_per_epoch(1)
            .build();
        let last_certificate_hash = chain.first().unwrap().hash.clone();

        let certificate_client = CertificateClientTestBuilder::default()
            .config_aggregator_client_mock(|mock| {
                mock.expect_certificate_chain(chain.certificates_chained.clone())
            })
            .with_genesis_verification_key(chain.genesis_verifier.to_verification_key())
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
        use mithril_common::test::builder::CertificateChainingMethod;
        use mockall::predicate::eq;

        use crate::aggregator_client::MockAggregatorClient;
        use crate::certificate_client::MockCertificateVerifierCache;
        use crate::certificate_client::verify_cache::MemoryCertificateVerifierCache;
        use crate::test_utils::TestLogger;

        use super::*;

        fn build_verifier_with_cache(
            aggregator_client_mock_config: impl FnOnce(&mut MockAggregatorClient),
            genesis_verification_key: ProtocolGenesisVerificationKey,
            cache: Arc<dyn CertificateVerifierCache>,
        ) -> MithrilCertificateVerifier {
            let mut aggregator_client = MockAggregatorClient::new();
            aggregator_client_mock_config(&mut aggregator_client);
            let genesis_verification_key: String = genesis_verification_key.try_into().unwrap();

            MithrilCertificateVerifier::new(
                Arc::new(aggregator_client),
                &genesis_verification_key,
                FeedbackSender::new(&[]),
                Some(cache),
                TestLogger::stdout(),
            )
            .unwrap()
        }

        #[tokio::test]
        async fn genesis_certificates_verification_result_is_not_cached() {
            let chain = CertificateChainBuilder::new()
                .with_total_certificates(1)
                .with_certificates_per_epoch(1)
                .build();
            let genesis_certificate = chain.last().unwrap();
            assert!(genesis_certificate.is_genesis());

            let cache = Arc::new(MemoryCertificateVerifierCache::new(TimeDelta::hours(1)));
            let verifier = build_verifier_with_cache(
                |_mock| {},
                chain.genesis_verifier.to_verification_key(),
                cache.clone(),
            );

            verifier
                .verify_with_cache_enabled(
                    "certificate_chain_validation_id",
                    CertificateToVerify::Downloaded {
                        certificate: Box::new(genesis_certificate.clone()),
                    },
                )
                .await
                .unwrap();

            assert_eq!(
                cache.get_previous_hash(&genesis_certificate.hash).await.unwrap(),
                None
            );
        }

        #[tokio::test]
        async fn non_genesis_certificates_verification_result_is_cached() {
            let chain = CertificateChainBuilder::new()
                .with_total_certificates(2)
                .with_certificates_per_epoch(1)
                .build();
            let certificate = chain.first().unwrap();
            let genesis_certificate = chain.last().unwrap();
            assert!(!certificate.is_genesis());

            let cache = Arc::new(MemoryCertificateVerifierCache::new(TimeDelta::hours(1)));
            let verifier = build_verifier_with_cache(
                |mock| mock.expect_certificate_chain(vec![genesis_certificate.clone()]),
                chain.genesis_verifier.to_verification_key(),
                cache.clone(),
            );

            verifier
                .verify_with_cache_enabled(
                    "certificate_chain_validation_id",
                    CertificateToVerify::Downloaded {
                        certificate: Box::new(certificate.clone()),
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
            let chain = CertificateChainBuilder::new()
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
                    // Expect to fetch the first certificate from the network
                    mock.expect_certificate_chain(chain.certificates_chained.clone());
                })
                .with_genesis_verification_key(chain.genesis_verifier.to_verification_key())
                .with_verifier_cache(cache.clone())
                .build();

            certificate_client
                .verify_chain(&first_certificate.hash)
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn verification_of_certificates_should_not_use_cache_until_crossing_an_epoch_boundary()
         {
            // Scenario:
            // | Certificate | epoch |         Parent | Can use cache to | Should be fully |
            // |             |       |                | get parent hash  | Verified        |
            // |------------:|------:|---------------:|------------------|-----------------|
            // |         n°6 |     3 |            n°5 | No               | Yes             |
            // |         n°5 |     3 |            n°4 | No               | Yes             |
            // |         n°4 |     2 |            n°3 | Yes              | Yes             |
            // |         n°3 |     2 |            n°2 | Yes              | No              |
            // |         n°2 |     2 |            n°1 | Yes              | No              |
            // |         n°1 |     1 | None (genesis) | Yes              | Yes             |
            let chain = CertificateChainBuilder::new()
                .with_total_certificates(6)
                .with_certificates_per_epoch(3)
                .with_certificate_chaining_method(CertificateChainingMethod::Sequential)
                .build();

            let first_certificate = chain.first().unwrap();
            let genesis_certificate = chain.last().unwrap();
            assert!(genesis_certificate.is_genesis());

            let certificates_that_must_be_fully_verified =
                [chain[..3].to_vec(), vec![genesis_certificate.clone()]].concat();
            let certificates_which_parents_can_be_fetched_from_cache = chain[2..5].to_vec();

            let cache = {
                let mut mock = MockCertificateVerifierCache::new();

                for certificate in certificates_which_parents_can_be_fetched_from_cache {
                    let previous_hash = certificate.previous_hash.clone();
                    mock.expect_get_previous_hash()
                        .with(eq(certificate.hash.clone()))
                        .return_once(|_| Ok(Some(previous_hash)))
                        .once();
                }
                mock.expect_get_previous_hash()
                    .with(eq(genesis_certificate.hash.clone()))
                    .returning(|_| Ok(None));
                mock.expect_store_validated_certificate().returning(|_, _| Ok(()));

                Arc::new(mock)
            };

            let certificate_client = CertificateClientTestBuilder::default()
                .config_aggregator_client_mock(|mock| {
                    mock.expect_certificate_chain(certificates_that_must_be_fully_verified);
                })
                .with_genesis_verification_key(chain.genesis_verifier.to_verification_key())
                .with_verifier_cache(cache)
                .build();

            certificate_client
                .verify_chain(&first_certificate.hash)
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn verify_chain_return_certificate_with_cache() {
            let chain = CertificateChainBuilder::new()
                .with_total_certificates(5)
                .with_certificates_per_epoch(1)
                .build();
            let last_certificate_hash = chain.first().unwrap().hash.clone();

            // All certificates are cached except the last two (to cross an epoch boundary) and the genesis
            let cache = MemoryCertificateVerifierCache::new(TimeDelta::hours(3))
                .with_items_from_chain(&chain[2..4]);

            let certificate_client = CertificateClientTestBuilder::default()
                .config_aggregator_client_mock(|mock| {
                    mock.expect_certificate_chain(
                        [chain[0..3].to_vec(), vec![chain.last().unwrap().clone()]].concat(),
                    )
                })
                .with_genesis_verification_key(chain.genesis_verifier.to_verification_key())
                .with_verifier_cache(Arc::new(cache))
                .build();

            let certificate =
                certificate_client.verify_chain(&last_certificate_hash).await.unwrap();

            assert_eq!(certificate.hash, last_certificate_hash);
        }
    }
}
