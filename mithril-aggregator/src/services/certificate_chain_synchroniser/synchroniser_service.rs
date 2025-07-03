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
//! 4. Store the fetched certificates in the database, from genesis to latest, for each certificate:
//!    - if it exists in the database, it is replaced
//!    - if it doesn't exist, it is inserted
//! 5. End
//!
use anyhow::{Context, anyhow};
use async_trait::async_trait;
use slog::{Logger, debug, info};
use std::collections::VecDeque;
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum SyncStatus {
    Forced,
    NoLocalGenesis,
    RemoteGenesisMatchesLocalGenesis,
    RemoteGenesisDontMatchesLocalGenesis,
}

impl SyncStatus {
    fn should_sync(&self) -> bool {
        match self {
            SyncStatus::Forced => true,
            SyncStatus::NoLocalGenesis => true,
            SyncStatus::RemoteGenesisMatchesLocalGenesis => false,
            SyncStatus::RemoteGenesisDontMatchesLocalGenesis => true,
        }
    }
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

    async fn check_sync_state(&self, force: bool) -> StdResult<SyncStatus> {
        if force {
            return Ok(SyncStatus::Forced);
        }

        match self.certificate_storer.get_latest_genesis().await? {
            Some(local_genesis) => {
                match self
                    .remote_certificate_retriever
                    .get_genesis_certificate_details()
                    .await?
                {
                    Some(remote_genesis) if (local_genesis == remote_genesis) => {
                        Ok(SyncStatus::RemoteGenesisMatchesLocalGenesis)
                    }
                    Some(_) => Ok(SyncStatus::RemoteGenesisDontMatchesLocalGenesis),
                    // The remote aggregator doesn't have a chain yet, we can't sync
                    None => Err(anyhow!("Remote aggregator doesn't have a chain yet")),
                }
            }
            None => Ok(SyncStatus::NoLocalGenesis),
        }
    }

    async fn retrieve_and_validate_remote_certificate_chain(
        &self,
        starting_point: Certificate,
    ) -> StdResult<Vec<Certificate>> {
        // IMPORTANT: Order matters, certificates must be ordered from genesis to latest
        // (fetched database data is returned from last inserted to oldest)
        let mut validated_certificates = VecDeque::new();
        let mut certificate = starting_point;

        loop {
            let parent_certificate = self
                .certificate_verifier
                .verify_certificate(&certificate, &self.genesis_verifier.to_verification_key())
                .await
                .with_context(
                    || format!("Failed to verify certificate: `{}`", certificate.hash,),
                )?;
            validated_certificates.push_front(certificate);

            match parent_certificate {
                None => break,
                Some(parent) => {
                    certificate = parent;
                }
            }
        }

        Ok(validated_certificates.into())
    }

    async fn store_certificate_chain(&self, certificate_chain: Vec<Certificate>) -> StdResult<()> {
        self.certificate_storer
            .insert_or_replace_many(certificate_chain)
            .await?;
        Ok(())
    }
}

#[async_trait]
impl CertificateChainSynchronizer for MithrilCertificateChainSynchronizer {
    async fn synchronize_certificate_chain(&self, force: bool) -> StdResult<()> {
        debug!(self.logger, ">> synchronize_certificate_chain"; "force" => force);

        let sync_state = self.check_sync_state(force).await.with_context(|| {
            format!("Failed to check if certificate chain should be sync (force: `{force}`)")
        })?;
        if sync_state.should_sync() {
            info!(self.logger, "Start synchronizing certificate chain"; "sync_state" => ?sync_state);
        } else {
            info!(self.logger, "No need to synchronize certificate chain"; "sync_state" => ?sync_state);
            return Ok(());
        }

        let starting_point = self
            .remote_certificate_retriever
            .get_latest_certificate_details()
            .await?
            .ok_or(
                anyhow!("Remote aggregator doesn't have a chain yet")
                    .context("Failed to retrieve latest remote certificate details"),
            )?;
        let remote_certificate_chain = self
            .retrieve_and_validate_remote_certificate_chain(starting_point)
            .await
            .with_context(|| "Failed to retrieve and validate remote certificate chain")?;
        self.store_certificate_chain(remote_certificate_chain)
            .await
            .with_context(|| "Failed to store remote retrieved certificate chain")?;

        info!(
            self.logger,
            "Certificate chain synchronized with remote source"
        );
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use std::sync::RwLock;

    use mithril_common::certificate_chain::{
        FakeCertificaterRetriever, MithrilCertificateVerifier,
    };
    use mithril_common::crypto_helper::ProtocolGenesisVerificationKey;
    use mithril_common::test_utils::{
        CertificateChainBuilder, CertificateChainFixture, fake_data, fake_keys,
        mock_extensions::MockBuilder,
    };

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

    fn fake_verifier(remote_certificate_chain: &[Certificate]) -> Arc<dyn CertificateVerifier> {
        let verifier = MithrilCertificateVerifier::new(
            TestLogger::stdout(),
            Arc::new(FakeCertificaterRetriever::from_certificates(
                remote_certificate_chain,
            )),
        );
        Arc::new(verifier)
    }

    #[derive(Default)]
    struct DumbCertificateStorer {
        certificates: RwLock<Vec<Certificate>>,
        genesis_certificate: Option<Certificate>,
    }

    impl DumbCertificateStorer {
        fn new(genesis: Certificate, already_stored: Vec<Certificate>) -> Self {
            Self {
                certificates: RwLock::new(already_stored),
                genesis_certificate: Some(genesis),
            }
        }

        fn stored_certificates(&self) -> Vec<Certificate> {
            self.certificates.read().unwrap().clone()
        }
    }

    #[async_trait]
    impl SynchronizedCertificateStorer for DumbCertificateStorer {
        async fn insert_or_replace_many(
            &self,
            certificates_chain: Vec<Certificate>,
        ) -> StdResult<()> {
            let mut certificates = self.certificates.write().unwrap();
            *certificates = certificates_chain;
            Ok(())
        }

        async fn get_latest_genesis(&self) -> StdResult<Option<Certificate>> {
            Ok(self.genesis_certificate.clone())
        }
    }

    mod check_sync_state {
        use super::*;

        #[test]
        fn sync_state_should_sync() {
            assert!(SyncStatus::Forced.should_sync());
            assert!(!SyncStatus::RemoteGenesisMatchesLocalGenesis.should_sync());
            assert!(SyncStatus::RemoteGenesisDontMatchesLocalGenesis.should_sync());
            assert!(SyncStatus::NoLocalGenesis.should_sync());
        }

        #[tokio::test]
        async fn state_when_force_true() {
            let synchroniser = MithrilCertificateChainSynchronizer::default_for_test();

            let sync_state = synchroniser.check_sync_state(true).await.unwrap();
            assert_eq!(SyncStatus::Forced, sync_state);
        }

        #[tokio::test]
        async fn state_when_force_false_and_no_local_genesis_certificate_found() {
            let synchroniser = mocked_synchroniser!(with_local_genesis: Ok(None));

            let sync_state = synchroniser.check_sync_state(false).await.unwrap();
            assert_eq!(SyncStatus::NoLocalGenesis, sync_state);
        }

        #[tokio::test]
        async fn state_when_force_false_and_remote_genesis_dont_matches_local_genesis() {
            let synchroniser = mocked_synchroniser!(
                with_remote_genesis: Ok(Some(fake_data::genesis_certificate("remote_genesis"))),
                with_local_genesis: Ok(Some(fake_data::genesis_certificate("local_genesis")))
            );

            let sync_state = synchroniser.check_sync_state(false).await.unwrap();
            assert_eq!(SyncStatus::RemoteGenesisDontMatchesLocalGenesis, sync_state);
        }

        #[tokio::test]
        async fn state_when_force_false_and_remote_genesis_matches_local_genesis() {
            let remote_genesis = fake_data::genesis_certificate("genesis");
            let local_genesis = remote_genesis.clone();
            let synchroniser = mocked_synchroniser!(
                with_remote_genesis: Ok(Some(remote_genesis)),
                with_local_genesis: Ok(Some(local_genesis))
            );

            let sync_state = synchroniser.check_sync_state(false).await.unwrap();
            assert_eq!(SyncStatus::RemoteGenesisMatchesLocalGenesis, sync_state);
        }

        #[tokio::test]
        async fn if_force_true_it_should_not_fetch_remote_genesis_certificate() {
            let synchroniser = mocked_synchroniser!(with_remote_genesis: Err(anyhow!(
                "should not fetch genesis"
            )));

            synchroniser.check_sync_state(true).await.unwrap();
        }

        #[tokio::test]
        async fn should_abort_with_error_if_force_false_and_fails_to_retrieve_local_genesis() {
            let synchroniser = mocked_synchroniser!(with_local_genesis: Err(anyhow!("failure")));
            synchroniser
                .check_sync_state(false)
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
                .check_sync_state(false)
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
                .check_sync_state(false)
                .await
                .expect_err("Expected an error but was:");

            assert!(
                error
                    .to_string()
                    .contains("Remote aggregator doesn't have a chain yet"),
                "Unexpected error:\n{error:?}"
            );
        }
    }

    mod retrieve_validate_remote_certificate_chain {
        use mockall::predicate::{always, eq};

        use super::*;

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

            let mut expected = chain.certificate_path_to_genesis(&starting_point.hash);
            expected.reverse();
            assert_eq!(remote_certificate_chain, expected);
        }

        #[tokio::test]
        async fn return_chain_ordered_from_genesis_to_latest() {
            let base_certificate = fake_data::certificate("whatever");
            let chain = vec![
                fake_data::genesis_certificate("genesis"),
                Certificate {
                    hash: "hash1".to_string(),
                    previous_hash: "genesis".to_string(),
                    ..base_certificate.clone()
                },
                Certificate {
                    hash: "hash2".to_string(),
                    previous_hash: "hash1".to_string(),
                    ..base_certificate
                },
            ];
            let synchroniser = MithrilCertificateChainSynchronizer {
                certificate_verifier: MockBuilder::<MockCertificateVerifier>::configure(|mock| {
                    let cert_1 = chain[1].clone();
                    mock.expect_verify_certificate()
                        .with(eq(chain[2].clone()), always())
                        .return_once(move |_, _| Ok(Some(cert_1)));
                    let genesis = chain[0].clone();
                    mock.expect_verify_certificate()
                        .with(eq(chain[1].clone()), always())
                        .return_once(move |_, _| Ok(Some(genesis)));
                    mock.expect_verify_certificate()
                        .with(eq(chain[0].clone()), always())
                        .return_once(move |_, _| Ok(None));
                }),
                ..MithrilCertificateChainSynchronizer::default_for_test()
            };

            let starting_point = chain[2].clone();
            let remote_certificate_chain = synchroniser
                .retrieve_and_validate_remote_certificate_chain(starting_point.clone())
                .await
                .unwrap();

            assert_eq!(
                remote_certificate_chain
                    .into_iter()
                    .map(|c| c.hash)
                    .collect::<Vec<_>>(),
                vec!["genesis".to_string(), "hash1".to_string(), "hash2".to_string()]
            );
        }
    }

    mod store_remote_certificate_chain {
        use super::*;

        #[tokio::test]
        async fn do_store_given_certificates() {
            let certificates_chain = vec![
                fake_data::genesis_certificate("genesis"),
                fake_data::certificate("certificate1"),
                fake_data::certificate("certificate2"),
            ];
            let storer = Arc::new(DumbCertificateStorer::default());
            let synchroniser = MithrilCertificateChainSynchronizer {
                certificate_storer: storer.clone(),
                ..MithrilCertificateChainSynchronizer::default_for_test()
            };

            assert_eq!(Vec::<Certificate>::new(), storer.stored_certificates());

            synchroniser
                .store_certificate_chain(certificates_chain.clone())
                .await
                .unwrap();

            assert_eq!(certificates_chain, storer.stored_certificates());
        }

        #[tokio::test]
        async fn fail_on_storer_error() {
            let synchroniser = MithrilCertificateChainSynchronizer {
                certificate_storer: MockBuilder::<MockSynchronizedCertificateStorer>::configure(
                    |mock| {
                        mock.expect_insert_or_replace_many()
                            .return_once(move |_| Err(anyhow!("failure")));
                    },
                ),
                ..MithrilCertificateChainSynchronizer::default_for_test()
            };

            synchroniser
                .store_certificate_chain(vec![fake_data::certificate("certificate")])
                .await
                .unwrap_err();
        }
    }

    mod synchronize_certificate_chain {
        use super::*;

        fn build_synchroniser(
            remote_chain: &CertificateChainFixture,
            storer: Arc<dyn SynchronizedCertificateStorer>,
        ) -> MithrilCertificateChainSynchronizer {
            MithrilCertificateChainSynchronizer {
                certificate_storer: storer.clone(),
                remote_certificate_retriever:
                    MockBuilder::<MockRemoteCertificateRetriever>::configure(|mock| {
                        let genesis = remote_chain.genesis_certificate().clone();
                        mock.expect_get_genesis_certificate_details()
                            .return_once(move || Ok(Some(genesis)));
                        let latest = remote_chain.latest_certificate().clone();
                        mock.expect_get_latest_certificate_details()
                            .return_once(move || Ok(Some(latest)));
                    }),
                certificate_verifier: fake_verifier(remote_chain),
                ..MithrilCertificateChainSynchronizer::default_for_test()
            }
        }

        #[tokio::test]
        async fn store_all() {
            let remote_chain = CertificateChainBuilder::default()
                .with_certificates_per_epoch(3)
                .with_total_certificates(8)
                .build();
            let storer = Arc::new(DumbCertificateStorer::default());
            let synchroniser = build_synchroniser(&remote_chain, storer.clone());

            // Will sync even if force is false
            synchroniser.synchronize_certificate_chain(false).await.unwrap();

            let mut expected =
                remote_chain.certificate_path_to_genesis(&remote_chain.latest_certificate().hash);
            expected.reverse();
            assert_eq!(expected, storer.stored_certificates());
        }

        #[tokio::test]
        async fn store_partial() {
            let remote_chain = CertificateChainBuilder::default()
                .with_certificates_per_epoch(1)
                .with_total_certificates(8)
                .build();
            let existing_certificates =
                remote_chain.certificate_path_to_genesis(&remote_chain[5].hash);
            let storer = Arc::new(DumbCertificateStorer::new(
                remote_chain.genesis_certificate().clone(),
                existing_certificates.clone(),
            ));
            let synchroniser = build_synchroniser(&remote_chain, storer.clone());

            // Force false - won't sync
            synchroniser.synchronize_certificate_chain(false).await.unwrap();

            assert_eq!(&existing_certificates, &storer.stored_certificates());

            // Force true - will sync
            synchroniser.synchronize_certificate_chain(true).await.unwrap();

            let mut expected =
                remote_chain.certificate_path_to_genesis(&remote_chain.latest_certificate().hash);
            expected.reverse();
            assert_eq!(expected, storer.stored_certificates());
        }
    }
}
