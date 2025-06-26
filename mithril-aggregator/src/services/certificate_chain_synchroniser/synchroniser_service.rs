//! # Certificate chain synchronizer
//!
//! Behavior:
//! 1. Check force:
//!    - If false, fetch the latest local genesis certificate in database
//!       - If it's found, fetch the remote Genesis certificate
//!          - If it's different from the local genesis, continue synchronization
//!          - If it's the same, abort with an `Ok`
//!       - If it's not found, continue synchronization
//!    - If true, skip the remote Genesis certificate check and synchronize
//! 2. Fetch then validate the latest remote certificate
//!    - if valid, store it in an in-memory FIFO list
//!    - if invalid, abort with an `Err`
//! 3. Repeat step 2. with each parent of the certificate until the genesis certificate is reached
//! 4. Store the fetched certificates in the database, for each certificate:
//!    - if it exists in the database, it is replaced
//!    - if it doesn't exist, it is inserted
//! 5. End
//!
use anyhow::anyhow;
use async_trait::async_trait;
use slog::Logger;
use std::sync::Arc;

use mithril_common::StdResult;
use mithril_common::certificate_chain::CertificateVerifier;
use mithril_common::crypto_helper::ProtocolGenesisVerifier;
use mithril_common::entities::Certificate;
use mithril_common::logging::LoggerExtensions;

use super::{
    CertificateChainSynchronizer, RemoteCertificateRetriever, SynchronizedCertificateStorer,
};

/// Service that synchronizes the certificate chain with a remote aggregator
pub struct MithrilCertificateChainSynchronizer {
    remote_certificate_retriever: Arc<dyn RemoteCertificateRetriever>,
    certificate_storer: Arc<dyn SynchronizedCertificateStorer>,
    certificate_verifier: Arc<dyn CertificateVerifier>,
    genesis_verifier: Arc<ProtocolGenesisVerifier>,
    logger: Logger,
}

impl MithrilCertificateChainSynchronizer {
    /// Create a new `MithrilCertificateChainSynchronizer` instance
    pub fn new(
        remote_certificate_retriever: Arc<dyn RemoteCertificateRetriever>,
        certificate_storer: Arc<dyn SynchronizedCertificateStorer>,
        certificate_verifier: Arc<dyn CertificateVerifier>,
        genesis_verifier: Arc<ProtocolGenesisVerifier>,
        logger: Logger,
    ) -> Self {
        Self {
            remote_certificate_retriever,
            certificate_storer,
            certificate_verifier,
            genesis_verifier,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    async fn should_sync(&self, force: bool) -> StdResult<bool> {
        if force {
            return Ok(true);
        }

        match self.certificate_storer.get_latest_genesis().await? {
            Some(local_genesis) => {
                match self
                    .remote_certificate_retriever
                    .get_genesis_certificate_details()
                    .await?
                {
                    Some(remote_genesis) => Ok(local_genesis != remote_genesis),
                    // The remote aggregator doesn't have a chain yet, we can't sync
                    None => Err(anyhow!("Remote aggregator doesn't have a chain yet")),
                }
            }
            // No local genesis certificate found, always sync
            None => Ok(true),
        }
    }

    async fn retrieve_and_validate_remote_certificate_chain(
        &self,
        starting_point: Certificate,
    ) -> StdResult<Vec<Certificate>> {
        let mut validated_certificates = Vec::new();
        let mut certificate = starting_point;

        loop {
            let parent_certificate = self
                .certificate_verifier
                .verify_certificate(&certificate, &self.genesis_verifier.to_verification_key())
                .await
                .with_context(
                    || format!("Failed to verify certificate: `{}`", certificate.hash,),
                )?;
            validated_certificates.push(certificate);

            match parent_certificate {
                None => break,
                Some(parent) => {
                    certificate = parent;
                }
            }
        }

        Ok(validated_certificates)
    }

    async fn store_certificate_chain(&self, certificate_chain: Vec<Certificate>) -> StdResult<()> {
        Ok(())
    }
}

#[async_trait]
impl CertificateChainSynchronizer for MithrilCertificateChainSynchronizer {
    async fn synchronize_certificate_chain(&self, force: bool) -> StdResult<()> {
        if !self.should_sync(force).await? {
            return Ok(());
        }

        let starting_point = self
            .remote_certificate_retriever
            .get_latest_certificate_details()
            .await?
            .ok_or(anyhow!("Remote aggregator doesn't have a chain yet"))?;
        let remote_certificate_chain = self
            .retrieve_and_validate_remote_certificate_chain(starting_point)
            .await?;
        self.store_certificate_chain(remote_certificate_chain).await?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;

    use mithril_common::crypto_helper::ProtocolGenesisVerificationKey;
    use mithril_common::test_utils::{fake_data, fake_keys, mock_extensions::MockBuilder};

    use crate::services::{MockRemoteCertificateRetriever, MockSynchronizedCertificateStorer};
    use crate::test_tools::TestLogger;

    use super::*;

    mockall::mock! {
        CertificateVerifier {}
        #[async_trait]
        impl CertificateVerifier for CertificateVerifier {
            async fn verify_genesis_certificate(
                &self,
                genesis_certificate: &Certificate,
                genesis_verification_key: &ProtocolGenesisVerificationKey,
            ) -> StdResult<()>;

            async fn verify_standard_certificate(
                &self,
                certificate: &Certificate,
                previous_certificate: &Certificate,
            ) -> StdResult<()>;

            async fn verify_certificate(
                &self,
                certificate: &Certificate,
                genesis_verification_key: &ProtocolGenesisVerificationKey,
            ) -> StdResult<Option<Certificate>>;

            async fn verify_certificate_chain(
                &self,
                certificate: Certificate,
                genesis_verification_key: &ProtocolGenesisVerificationKey,
            ) -> StdResult<()>;
        }
    }

    impl MithrilCertificateChainSynchronizer {
        fn default_for_test() -> Self {
            let genesis_verification_key =
                fake_keys::genesis_verification_key()[0].try_into().unwrap();
            Self::new(
                Arc::new(MockRemoteCertificateRetriever::new()),
                Arc::new(MockSynchronizedCertificateStorer::new()),
                Arc::new(MockCertificateVerifier::new()),
                Arc::new(ProtocolGenesisVerifier::from_verification_key(
                    genesis_verification_key,
                )),
                TestLogger::stdout(),
            )
        }
    }

    macro_rules! mocked_synchroniser {
        (with_remote_genesis: $remote_genesis_result:expr) => {
            MithrilCertificateChainSynchronizer {
                remote_certificate_retriever:
                    MockBuilder::<MockRemoteCertificateRetriever>::configure(|retriever| {
                        retriever
                            .expect_get_genesis_certificate_details()
                            .return_once(move || $remote_genesis_result);
                    }),
                ..MithrilCertificateChainSynchronizer::default_for_test()
            }
        };
        (with_local_genesis: $local_genesis_result:expr) => {
            MithrilCertificateChainSynchronizer {
                certificate_storer: MockBuilder::<MockSynchronizedCertificateStorer>::configure(
                    |storer| {
                        storer
                            .expect_get_latest_genesis()
                            .return_once(move || $local_genesis_result);
                    },
                ),
                ..MithrilCertificateChainSynchronizer::default_for_test()
            }
        };
        (with_remote_genesis: $remote_genesis_result:expr, with_local_genesis: $local_genesis_result:expr) => {
            MithrilCertificateChainSynchronizer {
                remote_certificate_retriever:
                    MockBuilder::<MockRemoteCertificateRetriever>::configure(|retriever| {
                        retriever
                            .expect_get_genesis_certificate_details()
                            .return_once(move || $remote_genesis_result);
                    }),
                certificate_storer: MockBuilder::<MockSynchronizedCertificateStorer>::configure(
                    |storer| {
                        storer
                            .expect_get_latest_genesis()
                            .return_once(move || $local_genesis_result);
                    },
                ),
                ..MithrilCertificateChainSynchronizer::default_for_test()
            }
        };
        (with_verify_certificate_result: $verify_certificate_result:expr) => {
            MithrilCertificateChainSynchronizer {
                certificate_verifier: MockBuilder::<MockCertificateVerifier>::configure(
                    |verifier| {
                        verifier
                            .expect_verify_certificate()
                            .return_once(move |_, _| $verify_certificate_result);
                    },
                ),
                ..MithrilCertificateChainSynchronizer::default_for_test()
            }
        };
    }

    mod should_sync {
        use super::*;

        #[tokio::test]
        async fn should_sync_if_force_true() {
            let synchroniser = MithrilCertificateChainSynchronizer::default_for_test();

            let should_sync = synchroniser.should_sync(true).await.unwrap();
            assert!(should_sync);
        }

        #[tokio::test]
        async fn should_sync_if_force_true_without_checking_genesis_certificate() {
            let synchroniser = mocked_synchroniser!(with_remote_genesis: Err(anyhow!(
                "should not fetch genesis"
            )));

            let should_sync = synchroniser.should_sync(true).await.unwrap();
            assert!(should_sync);
        }

        #[tokio::test]
        async fn should_sync_if_false_and_no_local_genesis_certificate_found() {
            let synchroniser = mocked_synchroniser!(with_local_genesis: Ok(None));

            let should_sync = synchroniser.should_sync(false).await.unwrap();
            assert!(should_sync);
        }

        #[tokio::test]
        async fn should_abort_with_error_if_force_false_and_fails_to_retrieve_local_genesis() {
            let synchroniser = mocked_synchroniser!(with_local_genesis: Err(anyhow!("failure")));
            synchroniser
                .should_sync(false)
                .await
                .expect_err("Expected an error but was:");
        }

        #[tokio::test]
        async fn should_abort_with_error_if_force_false_and_fails_to_retrieve_remote_genesis() {
            let synchroniser = mocked_synchroniser!(
                with_remote_genesis: Err(anyhow!("failure")),
                with_local_genesis: Ok(Some(fake_data::genesis_certificate("local_genesis")))
            );
            synchroniser
                .should_sync(false)
                .await
                .expect_err("Expected an error but was:");
        }

        #[tokio::test]
        async fn should_abort_with_error_if_force_false_and_remote_genesis_is_none() {
            let synchroniser = mocked_synchroniser!(
                with_remote_genesis: Ok(None),
                with_local_genesis: Ok(Some(fake_data::genesis_certificate("local_genesis")))
            );
            let error = synchroniser
                .should_sync(false)
                .await
                .expect_err("Expected an error but was:");

            assert!(
                error
                    .to_string()
                    .contains("Remote aggregator doesn't have a chain yet"),
                "Unexpected error:\n{error:?}"
            );
        }

        #[tokio::test]
        async fn should_sync_if_force_false_and_remote_genesis_dont_matches_local_genesis() {
            let synchroniser = mocked_synchroniser!(
                with_remote_genesis: Ok(Some(fake_data::genesis_certificate("remote_genesis"))),
                with_local_genesis: Ok(Some(fake_data::genesis_certificate("local_genesis")))
            );

            let should_sync = synchroniser.should_sync(false).await.unwrap();
            assert!(should_sync);
        }

        #[tokio::test]
        async fn should_not_sync_if_force_false_and_remote_genesis_matches_local_genesis() {
            let remote_genesis = fake_data::genesis_certificate("genesis");
            let local_genesis = remote_genesis.clone();
            let synchroniser = mocked_synchroniser!(
                with_remote_genesis: Ok(Some(remote_genesis)),
                with_local_genesis: Ok(Some(local_genesis))
            );

            let should_sync = synchroniser.should_sync(false).await.unwrap();
            assert!(!should_sync);
        }
    }

    mod retrieve_validate_remote_certificate_chain {
        use super::*;
        use mithril_common::certificate_chain::{
            FakeCertificaterRetriever, MithrilCertificateVerifier,
        };

        fn fake_verifier(remote_certificate_chain: &[Certificate]) -> Arc<dyn CertificateVerifier> {
            let verifier = MithrilCertificateVerifier::new(
                TestLogger::stdout(),
                Arc::new(FakeCertificaterRetriever::from_certificates(
                    remote_certificate_chain,
                )),
            );
            Arc::new(verifier)
        }

        #[tokio::test]
        async fn succeed_if_the_remote_chain_only_contains_a_genesis_certificate() {
            let chain = CertificateChainBuilder::new().with_total_certificates(1).build();
            let synchroniser = MithrilCertificateChainSynchronizer {
                certificate_verifier: fake_verifier(&chain),
                genesis_verifier: Arc::new(chain.genesis_verifier.clone()),
                ..MithrilCertificateChainSynchronizer::default_for_test()
            };

            let starting_point = chain[0].clone();
            let remote_certificate_chain = synchroniser
                .retrieve_and_validate_remote_certificate_chain(starting_point)
                .await
                .unwrap();

            assert_eq!(remote_certificate_chain, chain.certificates_chained);
        }

        #[tokio::test]
        async fn abort_with_error_if_a_certificate_is_invalid() {
            let synchroniser = mocked_synchroniser!(with_verify_certificate_result: Err(anyhow!("invalid certificate")));

            let starting_point = fake_data::certificate("certificate");
            synchroniser
                .retrieve_and_validate_remote_certificate_chain(starting_point)
                .await
                .expect_err("Expected an error but was:");
        }

        #[tokio::test]
        async fn succeed_with_a_valid_certificate_chain() {
            let chain = CertificateChainBuilder::new()
                .with_total_certificates(10)
                .with_certificates_per_epoch(3)
                .build();
            let synchroniser = MithrilCertificateChainSynchronizer {
                certificate_verifier: fake_verifier(&chain),
                genesis_verifier: Arc::new(chain.genesis_verifier.clone()),
                ..MithrilCertificateChainSynchronizer::default_for_test()
            };

            let starting_point = chain[0].clone();
            let remote_certificate_chain = synchroniser
                .retrieve_and_validate_remote_certificate_chain(starting_point.clone())
                .await
                .unwrap();

            let expected = chain.certificate_path_to_genesis(&starting_point.hash);
            assert_eq!(remote_certificate_chain, expected);
        }
    }
}
