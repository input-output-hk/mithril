use std::sync::Arc;

#[cfg(feature = "fs")]
use slog::Logger;

#[cfg(feature = "fs")]
use mithril_common::entities::{AncillaryLocation, ImmutablesLocation};

use crate::aggregator_client::AggregatorClient;
#[cfg(feature = "fs")]
use crate::feedback::FeedbackSender;
#[cfg(feature = "fs")]
use crate::file_downloader::{FileDownloader, FileDownloaderResolver};

/// HTTP client for CardanoDatabase API from the Aggregator
pub struct CardanoDatabaseClient {
    pub(super) aggregator_client: Arc<dyn AggregatorClient>,
    #[cfg(feature = "fs")]
    pub(super) immutable_file_downloader_resolver:
        Arc<dyn FileDownloaderResolver<ImmutablesLocation>>,
    #[cfg(feature = "fs")]
    pub(super) ancillary_file_downloader_resolver:
        Arc<dyn FileDownloaderResolver<AncillaryLocation>>,
    #[cfg(feature = "fs")]
    pub(super) http_file_downloader: Arc<dyn FileDownloader>,
    #[cfg(feature = "fs")]
    pub(super) feedback_sender: FeedbackSender,
    #[cfg(feature = "fs")]
    pub(super) logger: Logger,
}

impl CardanoDatabaseClient {
    /// Constructs a new `CardanoDatabase`.
    pub fn new(
        aggregator_client: Arc<dyn AggregatorClient>,
        #[cfg(feature = "fs")] immutable_file_downloader_resolver: Arc<
            dyn FileDownloaderResolver<ImmutablesLocation>,
        >,
        #[cfg(feature = "fs")] ancillary_file_downloader_resolver: Arc<
            dyn FileDownloaderResolver<AncillaryLocation>,
        >,
        #[cfg(feature = "fs")] http_file_downloader: Arc<dyn FileDownloader>,
        #[cfg(feature = "fs")] feedback_sender: FeedbackSender,
        #[cfg(feature = "fs")] logger: Logger,
    ) -> Self {
        Self {
            aggregator_client,
            #[cfg(feature = "fs")]
            immutable_file_downloader_resolver,
            #[cfg(feature = "fs")]
            ancillary_file_downloader_resolver,
            #[cfg(feature = "fs")]
            http_file_downloader,
            #[cfg(feature = "fs")]
            feedback_sender,
            #[cfg(feature = "fs")]
            logger: mithril_common::logging::LoggerExtensions::new_with_component_name::<Self>(
                &logger,
            ),
        }
    }
}

#[cfg(test)]
pub(crate) mod test_dependency_injector {
    use super::*;

    use mithril_common::entities::{
        AncillaryLocationDiscriminants, ImmutablesLocationDiscriminants,
    };

    use crate::{
        aggregator_client::MockAggregatorHTTPClient,
        feedback::FeedbackReceiver,
        file_downloader::{
            AncillaryFileDownloaderResolver, FileDownloader, ImmutablesFileDownloaderResolver,
            MockFileDownloaderBuilder,
        },
        test_utils,
    };

    /// Dependency injector for `CardanoDatabaseClient` for testing purposes.
    pub(crate) struct CardanoDatabaseClientDependencyInjector {
        http_client: MockAggregatorHTTPClient,
        immutable_file_downloader_resolver: ImmutablesFileDownloaderResolver,
        ancillary_file_downloader_resolver: AncillaryFileDownloaderResolver,
        http_file_downloader: Arc<dyn FileDownloader>,
        feedback_receivers: Vec<Arc<dyn FeedbackReceiver>>,
    }

    impl CardanoDatabaseClientDependencyInjector {
        pub(crate) fn new() -> Self {
            Self {
                http_client: MockAggregatorHTTPClient::new(),
                immutable_file_downloader_resolver: ImmutablesFileDownloaderResolver::new(vec![]),
                ancillary_file_downloader_resolver: AncillaryFileDownloaderResolver::new(vec![]),
                http_file_downloader: Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_compression(None)
                        .with_success()
                        .with_times(0)
                        .build(),
                ),
                feedback_receivers: vec![],
            }
        }

        pub(crate) fn with_http_client_mock_config<F>(mut self, config: F) -> Self
        where
            F: FnOnce(&mut MockAggregatorHTTPClient),
        {
            config(&mut self.http_client);

            self
        }

        pub(crate) fn with_immutable_file_downloaders(
            self,
            file_downloaders: Vec<(ImmutablesLocationDiscriminants, Arc<dyn FileDownloader>)>,
        ) -> Self {
            let immutable_file_downloader_resolver =
                ImmutablesFileDownloaderResolver::new(file_downloaders);

            Self {
                immutable_file_downloader_resolver,
                ..self
            }
        }

        pub(crate) fn with_ancillary_file_downloaders(
            self,
            file_downloaders: Vec<(AncillaryLocationDiscriminants, Arc<dyn FileDownloader>)>,
        ) -> Self {
            let ancillary_file_downloader_resolver =
                AncillaryFileDownloaderResolver::new(file_downloaders);

            Self {
                ancillary_file_downloader_resolver,
                ..self
            }
        }

        pub(crate) fn with_http_file_downloader(
            self,
            http_file_downloader: Arc<dyn FileDownloader>,
        ) -> Self {
            Self {
                http_file_downloader,
                ..self
            }
        }

        pub(crate) fn with_feedback_receivers(
            self,
            feedback_receivers: &[Arc<dyn FeedbackReceiver>],
        ) -> Self {
            Self {
                feedback_receivers: feedback_receivers.to_vec(),
                ..self
            }
        }

        pub(crate) fn build_cardano_database_client(self) -> CardanoDatabaseClient {
            CardanoDatabaseClient::new(
                Arc::new(self.http_client),
                Arc::new(self.immutable_file_downloader_resolver),
                Arc::new(self.ancillary_file_downloader_resolver),
                self.http_file_downloader,
                FeedbackSender::new(&self.feedback_receivers),
                test_utils::test_logger(),
            )
        }
    }
}
