#[cfg(feature = "fs")]
use std::path::Path;
use std::sync::Arc;

#[cfg(feature = "fs")]
use slog::Logger;

#[cfg(feature = "fs")]
use mithril_common::{
    crypto_helper::MKProof,
    messages::{CardanoDatabaseSnapshotMessage, CertificateMessage},
};

use crate::aggregator_client::AggregatorClient;
#[cfg(feature = "fs")]
use crate::feedback::FeedbackSender;
#[cfg(feature = "fs")]
use crate::file_downloader::FileDownloader;
use crate::{CardanoDatabaseSnapshot, CardanoDatabaseSnapshotListItem, MithrilResult};

use super::fetch::InternalArtifactRetriever;
#[cfg(feature = "fs")]
use super::{
    download_unpack::InternalArtifactDownloader, proving::InternalArtifactProver,
    DownloadUnpackOptions, ImmutableFileRange,
};

/// HTTP client for CardanoDatabase API from the Aggregator
pub struct CardanoDatabaseClient {
    pub(super) artifact_retriever: InternalArtifactRetriever,
    #[cfg(feature = "fs")]
    pub(super) artifact_downloader: InternalArtifactDownloader,
    #[cfg(feature = "fs")]
    pub(super) artifact_prover: InternalArtifactProver,
}

impl CardanoDatabaseClient {
    /// Constructs a new `CardanoDatabase`.
    pub fn new(
        aggregator_client: Arc<dyn AggregatorClient>,
        #[cfg(feature = "fs")] http_file_downloader: Arc<dyn FileDownloader>,
        #[cfg(feature = "fs")] feedback_sender: FeedbackSender,
        #[cfg(feature = "fs")] logger: Logger,
    ) -> Self {
        #[cfg(feature = "fs")]
        let logger =
            mithril_common::logging::LoggerExtensions::new_with_component_name::<Self>(&logger);
        Self {
            artifact_retriever: InternalArtifactRetriever::new(aggregator_client.clone()),
            #[cfg(feature = "fs")]
            artifact_downloader: InternalArtifactDownloader::new(
                http_file_downloader.clone(),
                feedback_sender.clone(),
                logger.clone(),
            ),
            #[cfg(feature = "fs")]
            artifact_prover: InternalArtifactProver::new(
                http_file_downloader.clone(),
                feedback_sender.clone(),
                logger.clone(),
            ),
        }
    }

    /// Fetch a list of signed CardanoDatabase
    pub async fn list(&self) -> MithrilResult<Vec<CardanoDatabaseSnapshotListItem>> {
        self.artifact_retriever.list().await
    }

    /// Get the given Cardano database data by hash
    pub async fn get(&self, hash: &str) -> MithrilResult<Option<CardanoDatabaseSnapshot>> {
        self.artifact_retriever.get(hash).await
    }

    /// Download and unpack the given Cardano database parts data by hash.
    #[cfg(feature = "fs")]
    pub async fn download_unpack(
        &self,
        cardano_database_snapshot: &CardanoDatabaseSnapshotMessage,
        immutable_file_range: &ImmutableFileRange,
        target_dir: &Path,
        download_unpack_options: DownloadUnpackOptions,
    ) -> MithrilResult<()> {
        self.artifact_downloader
            .download_unpack(
                cardano_database_snapshot,
                immutable_file_range,
                target_dir,
                download_unpack_options,
            )
            .await
    }

    /// Compute the Merkle proof of membership for the given immutable file range.
    #[cfg(feature = "fs")]
    pub async fn compute_merkle_proof(
        &self,
        certificate: &CertificateMessage,
        cardano_database_snapshot: &CardanoDatabaseSnapshotMessage,
        immutable_file_range: &ImmutableFileRange,
        database_dir: &Path,
    ) -> MithrilResult<MKProof> {
        self.artifact_prover
            .compute_merkle_proof(
                certificate,
                cardano_database_snapshot,
                immutable_file_range,
                database_dir,
            )
            .await
    }
}

#[cfg(test)]
pub(crate) mod test_dependency_injector {
    use super::*;

    use crate::aggregator_client::MockAggregatorClient;
    #[cfg(feature = "fs")]
    use crate::file_downloader::{FileDownloader, MockFileDownloaderBuilder};
    #[cfg(feature = "fs")]
    use crate::{feedback::FeedbackReceiver, test_utils};

    /// Dependency injector for `CardanoDatabaseClient` for testing purposes.
    pub(crate) struct CardanoDatabaseClientDependencyInjector {
        aggregator_client: MockAggregatorClient,
        #[cfg(feature = "fs")]
        http_file_downloader: Arc<dyn FileDownloader>,
        #[cfg(feature = "fs")]
        feedback_receivers: Vec<Arc<dyn FeedbackReceiver>>,
    }

    impl CardanoDatabaseClientDependencyInjector {
        pub(crate) fn new() -> Self {
            Self {
                aggregator_client: MockAggregatorClient::new(),
                #[cfg(feature = "fs")]
                http_file_downloader: Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_compression(None)
                        .with_success()
                        .with_times(0)
                        .build(),
                ),
                #[cfg(feature = "fs")]
                feedback_receivers: vec![],
            }
        }

        pub(crate) fn with_aggregator_client_mock_config<F>(mut self, config: F) -> Self
        where
            F: FnOnce(&mut MockAggregatorClient),
        {
            config(&mut self.aggregator_client);

            self
        }

        #[cfg(feature = "fs")]
        pub(crate) fn with_http_file_downloader(
            self,
            http_file_downloader: Arc<dyn FileDownloader>,
        ) -> Self {
            Self {
                http_file_downloader,
                ..self
            }
        }

        #[cfg(feature = "fs")]
        pub(crate) fn with_feedback_receivers(
            self,
            feedback_receivers: &[Arc<dyn FeedbackReceiver>],
        ) -> Self {
            Self {
                feedback_receivers: feedback_receivers.to_vec(),
                ..self
            }
        }

        #[cfg(feature = "fs")]
        pub(crate) fn build_cardano_database_client(self) -> CardanoDatabaseClient {
            CardanoDatabaseClient::new(
                Arc::new(self.aggregator_client),
                self.http_file_downloader,
                FeedbackSender::new(&self.feedback_receivers),
                test_utils::test_logger(),
            )
        }

        #[cfg(not(feature = "fs"))]
        pub(crate) fn build_cardano_database_client(self) -> CardanoDatabaseClient {
            CardanoDatabaseClient::new(Arc::new(self.aggregator_client))
        }
    }

    mod tests {
        use mockall::predicate;

        use crate::aggregator_client::AggregatorRequest;
        #[cfg(feature = "fs")]
        use crate::feedback::StackFeedbackReceiver;

        use super::*;

        #[cfg(feature = "fs")]
        #[test]
        fn test_cardano_database_client_dependency_injector_builds() {
            let _ = CardanoDatabaseClientDependencyInjector::new()
                .with_aggregator_client_mock_config(|http_client| {
                    let message = vec![CardanoDatabaseSnapshotListItem {
                        hash: "hash-123".to_string(),
                        ..CardanoDatabaseSnapshotListItem::dummy()
                    }];
                    http_client
                        .expect_get_content()
                        .with(predicate::eq(
                            AggregatorRequest::ListCardanoDatabaseSnapshots,
                        ))
                        .return_once(move |_| Ok(serde_json::to_string(&message).unwrap()));
                })
                .with_http_file_downloader(Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_success()
                        .with_times(0)
                        .build(),
                ))
                .with_feedback_receivers(&[Arc::new(StackFeedbackReceiver::new())])
                .build_cardano_database_client();
        }

        #[cfg(not(feature = "fs"))]
        #[test]
        fn test_cardano_database_client_dependency_injector_builds() {
            let _ = CardanoDatabaseClientDependencyInjector::new()
                .with_aggregator_client_mock_config(|http_client| {
                    let message = vec![CardanoDatabaseSnapshotListItem {
                        hash: "hash-123".to_string(),
                        ..CardanoDatabaseSnapshotListItem::dummy()
                    }];
                    http_client
                        .expect_get_content()
                        .with(predicate::eq(
                            AggregatorRequest::ListCardanoDatabaseSnapshots,
                        ))
                        .return_once(move |_| Ok(serde_json::to_string(&message).unwrap()));
                })
                .build_cardano_database_client();
        }
    }
}
