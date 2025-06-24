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
use mithril_common::entities::Certificate;
use mithril_common::logging::LoggerExtensions;

use super::{
    CertificateChainSynchronizer, RemoteCertificateRetriever, SynchronizedCertificateStorer,
};

/// Service that synchronizes the certificate chain with a remote aggregator
pub struct MithrilCertificateChainSynchronizer {
    remote_certificate_retriever: Arc<dyn RemoteCertificateRetriever>,
    certificate_storer: Arc<dyn SynchronizedCertificateStorer>,
    logger: Logger,
}

impl MithrilCertificateChainSynchronizer {
    /// Create a new `MithrilCertificateChainSynchronizer` instance
    pub fn new(
        remote_certificate_retriever: Arc<dyn RemoteCertificateRetriever>,
        certificate_storer: Arc<dyn SynchronizedCertificateStorer>,
        logger: Logger,
    ) -> Self {
        Self {
            remote_certificate_retriever,
            certificate_storer,
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

    async fn retrieve_remote_certificate_chain(&self) -> StdResult<Vec<Certificate>> {
        Ok(Vec::new())
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

        let remote_certificate_chain = self.retrieve_remote_certificate_chain().await?;
        self.store_certificate_chain(remote_certificate_chain).await?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;

    use mithril_common::test_utils::fake_data;

    use crate::services::{MockRemoteCertificateRetriever, MockSynchronizedCertificateStorer};
    use crate::test_tools::TestLogger;

    use super::*;

    impl MithrilCertificateChainSynchronizer {
        fn new_for_test(
            remote_certificate_retriever_mock_config: impl FnOnce(&mut MockRemoteCertificateRetriever),
            certificate_storer_mock_config: impl FnOnce(&mut MockSynchronizedCertificateStorer),
        ) -> Self {
            let mut remote_certificate_retriever_mock = MockRemoteCertificateRetriever::new();
            remote_certificate_retriever_mock_config(&mut remote_certificate_retriever_mock);

            let mut certificate_storer_mock = MockSynchronizedCertificateStorer::new();
            certificate_storer_mock_config(&mut certificate_storer_mock);

            Self {
                remote_certificate_retriever: Arc::new(remote_certificate_retriever_mock),
                certificate_storer: Arc::new(certificate_storer_mock),
                logger: TestLogger::stdout(),
            }
        }
    }

    macro_rules! mocked_synchroniser {
        () => {
            MithrilCertificateChainSynchronizer::new_for_test(|_| {}, |_| {})
        };
        (with_remote_genesis: $remote_genesis_result:expr) => {
            MithrilCertificateChainSynchronizer::new_for_test(
                move |retriever| {
                    retriever
                        .expect_get_genesis_certificate_details()
                        .return_once(move || $remote_genesis_result);
                },
                |_| {},
            )
        };
        (with_local_genesis: $local_genesis_result:expr) => {
            MithrilCertificateChainSynchronizer::new_for_test(
                |_| {},
                move |storer| {
                    storer
                        .expect_get_latest_genesis()
                        .return_once(move || $local_genesis_result);
                },
            )
        };
        (with_remote_genesis: $remote_genesis_result:expr, with_local_genesis: $local_genesis_result:expr) => {
            MithrilCertificateChainSynchronizer::new_for_test(
                move |retriever| {
                    retriever
                        .expect_get_genesis_certificate_details()
                        .return_once(move || $remote_genesis_result);
                },
                move |storer| {
                    storer
                        .expect_get_latest_genesis()
                        .return_once(move || $local_genesis_result);
                },
            )
        };
    }

    mod should_sync {
        use super::*;

        #[tokio::test]
        async fn should_sync_if_force_true() {
            let synchroniser = mocked_synchroniser!();

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
}
