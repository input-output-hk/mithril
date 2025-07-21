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
//! 5. Create a certified open message in the database, based on the latest certificate (the first
//!    of the last epoch synchronized)
//! 6. End
//!
use anyhow::{Context, anyhow};
use async_trait::async_trait;
use chrono::Utc;
use slog::{Logger, debug, info};
use std::collections::VecDeque;
use std::sync::Arc;

use mithril_common::StdResult;
use mithril_common::certificate_chain::CertificateVerifier;
use mithril_common::crypto_helper::ProtocolGenesisVerifier;
use mithril_common::entities::{Certificate, SignedEntityType};
use mithril_common::logging::LoggerExtensions;

use crate::entities::OpenMessage;

use super::{
    CertificateChainSynchronizer, OpenMessageStorer, RemoteCertificateRetriever,
    SynchronizedCertificateStorer,
};

/// Service that synchronizes the certificate chain with a remote aggregator
pub struct MithrilCertificateChainSynchronizer {
    remote_certificate_retriever: Arc<dyn RemoteCertificateRetriever>,
    certificate_storer: Arc<dyn SynchronizedCertificateStorer>,
    certificate_verifier: Arc<dyn CertificateVerifier>,
    genesis_verifier: Arc<ProtocolGenesisVerifier>,
    open_message_storer: Arc<dyn OpenMessageStorer>,
    logger: Logger,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum SyncStatus {
    Forced,
    NoLocalGenesis,
    RemoteGenesisMatchesLocalGenesis,
    RemoteGenesisDoesntMatchLocalGenesis,
}

impl SyncStatus {
    fn should_sync(&self) -> bool {
        match self {
            SyncStatus::Forced => true,
            SyncStatus::NoLocalGenesis => true,
            SyncStatus::RemoteGenesisMatchesLocalGenesis => false,
            SyncStatus::RemoteGenesisDoesntMatchLocalGenesis => true,
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
        open_message_storer: Arc<dyn OpenMessageStorer>,
        logger: Logger,
    ) -> Self {
        Self {
            remote_certificate_retriever,
            certificate_storer,
            certificate_verifier,
            genesis_verifier,
            open_message_storer,
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
                    Some(_) => Ok(SyncStatus::RemoteGenesisDoesntMatchLocalGenesis),
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
        // IMPORTANT: Order matters, returned certificates must be ordered from genesis to latest
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

            match parent_certificate {
                None => {
                    validated_certificates.push_front(certificate);
                    break;
                }
                Some(parent) => {
                    // At the start of the retrieval the first certificate may not be the first of
                    // its epoch, filter them out since we only need one certificate per epoch
                    if !validated_certificates.is_empty() || parent.epoch != certificate.epoch {
                        validated_certificates.push_front(certificate);
                    }

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
        let open_message = prepare_open_message_to_store(
            remote_certificate_chain
                .last()
                .ok_or(anyhow!("Retrieved certificate chain is empty"))?,
        );
        self.store_certificate_chain(remote_certificate_chain)
            .await
            .with_context(|| "Failed to store remote retrieved certificate chain")?;
        self.open_message_storer
            .insert_or_replace_open_message(open_message)
            .await
            .with_context(|| "Failed to store open message when synchronizing certificate chain")?;

        info!(
            self.logger,
            "Certificate chain synchronized with remote source"
        );
        Ok(())
    }
}

fn prepare_open_message_to_store(latest_certificate: &Certificate) -> OpenMessage {
    OpenMessage {
        epoch: latest_certificate.epoch,
        signed_entity_type: SignedEntityType::MithrilStakeDistribution(latest_certificate.epoch),
        protocol_message: latest_certificate.protocol_message.clone(),
        is_certified: true,
        is_expired: false,
        single_signatures: Vec::new(),
        created_at: Utc::now(),
        expires_at: None,
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use std::sync::RwLock;

    use mithril_common::certificate_chain::{
        FakeCertificaterRetriever, MithrilCertificateVerifier,
    };
    use mithril_common::test_utils::{
        CertificateChainBuilder, CertificateChainFixture, fake_data, fake_keys,
        mock_extensions::MockBuilder,
    };

    use crate::services::{
        MockOpenMessageStorer, MockRemoteCertificateRetriever, MockSynchronizedCertificateStorer,
    };
    use crate::test::mocks::MockCertificateVerifier;
    use crate::test_tools::TestLogger;

    use super::*;

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
                Arc::new(MockOpenMessageStorer::new()),
                TestLogger::stdout(),
            )
        }
    }

    macro_rules! mocked_synchronizer {
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
            assert!(SyncStatus::RemoteGenesisDoesntMatchLocalGenesis.should_sync());
            assert!(SyncStatus::NoLocalGenesis.should_sync());
        }

        #[tokio::test]
        async fn state_when_force_true() {
            let synchronizer = MithrilCertificateChainSynchronizer::default_for_test();

            let sync_state = synchronizer.check_sync_state(true).await.unwrap();
            assert_eq!(SyncStatus::Forced, sync_state);
        }

        #[tokio::test]
        async fn state_when_force_false_and_no_local_genesis_certificate_found() {
            let synchronizer = mocked_synchronizer!(with_local_genesis: Ok(None));

            let sync_state = synchronizer.check_sync_state(false).await.unwrap();
            assert_eq!(SyncStatus::NoLocalGenesis, sync_state);
        }

        #[tokio::test]
        async fn state_when_force_false_and_remote_genesis_dont_matches_local_genesis() {
            let synchronizer = mocked_synchronizer!(
                with_remote_genesis: Ok(Some(fake_data::genesis_certificate("remote_genesis"))),
                with_local_genesis: Ok(Some(fake_data::genesis_certificate("local_genesis")))
            );

            let sync_state = synchronizer.check_sync_state(false).await.unwrap();
            assert_eq!(SyncStatus::RemoteGenesisDoesntMatchLocalGenesis, sync_state);
        }

        #[tokio::test]
        async fn state_when_force_false_and_remote_genesis_matches_local_genesis() {
            let remote_genesis = fake_data::genesis_certificate("genesis");
            let local_genesis = remote_genesis.clone();
            let synchronizer = mocked_synchronizer!(
                with_remote_genesis: Ok(Some(remote_genesis)),
                with_local_genesis: Ok(Some(local_genesis))
            );

            let sync_state = synchronizer.check_sync_state(false).await.unwrap();
            assert_eq!(SyncStatus::RemoteGenesisMatchesLocalGenesis, sync_state);
        }

        #[tokio::test]
        async fn if_force_true_it_should_not_fetch_remote_genesis_certificate() {
            let synchronizer = mocked_synchronizer!(with_remote_genesis: Err(anyhow!(
                "should not fetch genesis"
            )));

            synchronizer.check_sync_state(true).await.unwrap();
        }

        #[tokio::test]
        async fn should_abort_with_error_if_force_false_and_fails_to_retrieve_local_genesis() {
            let synchronizer = mocked_synchronizer!(with_local_genesis: Err(anyhow!("failure")));
            synchronizer
                .check_sync_state(false)
                .await
                .expect_err("Expected an error but was:");
        }

        #[tokio::test]
        async fn should_abort_with_error_if_force_false_and_fails_to_retrieve_remote_genesis() {
            let synchronizer = mocked_synchronizer!(
                with_remote_genesis: Err(anyhow!("failure")),
                with_local_genesis: Ok(Some(fake_data::genesis_certificate("local_genesis")))
            );
            synchronizer
                .check_sync_state(false)
                .await
                .expect_err("Expected an error but was:");
        }

        #[tokio::test]
        async fn should_abort_with_error_if_force_false_and_remote_genesis_is_none() {
            let synchronizer = mocked_synchronizer!(
                with_remote_genesis: Ok(None),
                with_local_genesis: Ok(Some(fake_data::genesis_certificate("local_genesis")))
            );
            let error = synchronizer
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

        use mithril_common::entities::Epoch;

        use super::*;

        #[tokio::test]
        async fn succeed_if_the_remote_chain_only_contains_a_genesis_certificate() {
            let chain = CertificateChainBuilder::new().with_total_certificates(1).build();
            let synchronizer = MithrilCertificateChainSynchronizer {
                certificate_verifier: fake_verifier(&chain),
                genesis_verifier: Arc::new(chain.genesis_verifier.clone()),
                ..MithrilCertificateChainSynchronizer::default_for_test()
            };

            let starting_point = chain[0].clone();
            let remote_certificate_chain = synchronizer
                .retrieve_and_validate_remote_certificate_chain(starting_point)
                .await
                .unwrap();

            assert_eq!(remote_certificate_chain, chain.certificates_chained);
        }

        #[tokio::test]
        async fn abort_with_error_if_a_certificate_is_invalid() {
            let synchronizer = mocked_synchronizer!(with_verify_certificate_result: Err(anyhow!("invalid certificate")));

            let starting_point = fake_data::certificate("certificate");
            synchronizer
                .retrieve_and_validate_remote_certificate_chain(starting_point)
                .await
                .expect_err("Expected an error but was:");
        }

        #[tokio::test]
        async fn succeed_with_a_valid_certificate_chain_and_only_get_first_certificate_of_each_epoch_plus_genesis()
         {
            // Note: the `CertificateChainBuilder` use one epoch for the genesis only, so in order
            // for the last epoch to have two certificates when `certificates_per_epoch` is an *even*
            // number, we need to set `total_certificates` to an *odd* number
            let chain = CertificateChainBuilder::new()
                .with_total_certificates(9)
                .with_certificates_per_epoch(2)
                .build();
            let synchronizer = MithrilCertificateChainSynchronizer {
                certificate_verifier: fake_verifier(&chain),
                genesis_verifier: Arc::new(chain.genesis_verifier.clone()),
                ..MithrilCertificateChainSynchronizer::default_for_test()
            };

            let starting_point = chain[0].clone();
            let remote_certificate_chain = synchronizer
                .retrieve_and_validate_remote_certificate_chain(starting_point.clone())
                .await
                .unwrap();

            let mut expected = chain.certificate_path_to_genesis(&starting_point.hash);
            // Remote certificate chain is returned ordered from genesis to latest
            expected.reverse();
            // Remove the latest certificate has it's not the first of its epoch
            expected.pop();
            assert_eq!(remote_certificate_chain, expected);
        }

        #[tokio::test]
        async fn return_chain_ordered_from_genesis_to_latest() {
            let base_certificate = fake_data::certificate("whatever");
            let chain = vec![
                Certificate {
                    epoch: Epoch(2),
                    ..fake_data::genesis_certificate("genesis")
                },
                Certificate {
                    epoch: Epoch(3),
                    hash: "hash1".to_string(),
                    previous_hash: "genesis".to_string(),
                    ..base_certificate.clone()
                },
                Certificate {
                    epoch: Epoch(4),
                    hash: "hash2".to_string(),
                    previous_hash: "hash1".to_string(),
                    ..base_certificate
                },
            ];
            let synchronizer = MithrilCertificateChainSynchronizer {
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
            let remote_certificate_chain = synchronizer
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
            let synchronizer = MithrilCertificateChainSynchronizer {
                certificate_storer: storer.clone(),
                ..MithrilCertificateChainSynchronizer::default_for_test()
            };

            assert_eq!(Vec::<Certificate>::new(), storer.stored_certificates());

            synchronizer
                .store_certificate_chain(certificates_chain.clone())
                .await
                .unwrap();

            assert_eq!(certificates_chain, storer.stored_certificates());
        }

        #[tokio::test]
        async fn fail_on_storer_error() {
            let synchronizer = MithrilCertificateChainSynchronizer {
                certificate_storer: MockBuilder::<MockSynchronizedCertificateStorer>::configure(
                    |mock| {
                        mock.expect_insert_or_replace_many()
                            .return_once(move |_| Err(anyhow!("failure")));
                    },
                ),
                ..MithrilCertificateChainSynchronizer::default_for_test()
            };

            synchronizer
                .store_certificate_chain(vec![fake_data::certificate("certificate")])
                .await
                .unwrap_err();
        }
    }

    mod synchronize_certificate_chain {
        use mockall::predicate::function;

        use super::*;

        fn build_synchronizer(
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
                open_message_storer: MockBuilder::<MockOpenMessageStorer>::configure(|mock| {
                    // Ensure that `store_open_message` is called
                    let expected_msd_epoch = remote_chain.latest_certificate().epoch;
                    mock.expect_insert_or_replace_open_message()
                        .with(function(move |open_message: &OpenMessage| {
                            open_message.signed_entity_type
                                == SignedEntityType::MithrilStakeDistribution(expected_msd_epoch)
                        }))
                        .times(1..)
                        .returning(|_| Ok(()));
                }),
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
            let synchronizer = build_synchronizer(&remote_chain, storer.clone());

            // Will sync even if force is false
            synchronizer.synchronize_certificate_chain(false).await.unwrap();

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
            let synchronizer = build_synchronizer(&remote_chain, storer.clone());

            // Force false - won't sync
            synchronizer.synchronize_certificate_chain(false).await.unwrap();

            assert_eq!(&existing_certificates, &storer.stored_certificates());

            // Force true - will sync
            synchronizer.synchronize_certificate_chain(true).await.unwrap();

            let mut expected =
                remote_chain.certificate_path_to_genesis(&remote_chain.latest_certificate().hash);
            expected.reverse();
            assert_eq!(expected, storer.stored_certificates());
        }
    }
}
