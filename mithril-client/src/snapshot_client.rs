//! A client to retrieve snapshots data from an Aggregator.
//!
//! In order to do so it defines a [SnapshotClient] which exposes the following features:
//!  - [get][SnapshotClient::get]: get a single snapshot data from its digest
//!  - [list][SnapshotClient::list]: get the list of available snapshots
//!  - [download_unpack][SnapshotClient::download_unpack]: download and unpack the tarball of a snapshot to a directory
//!
//! # Get a single snapshot
//!
//! To get a single snapshot using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let snapshot = client.snapshot().get("SNAPSHOT_DIGEST").await?.unwrap();
//!
//! println!("Snapshot digest={}, size={}", snapshot.digest, snapshot.size);
//! #    Ok(())
//! # }
//! ```
//!
//! # List available snapshots
//!
//! To list available snapshots using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let snapshots = client.snapshot().list().await?;
//!
//! for snapshot in snapshots {
//!     println!("Snapshot digest={}, size={}", snapshot.digest, snapshot.size);
//! }
//! #    Ok(())
//! # }
//! ```
//!
//! # Download a snapshot
//! **Note:** _Available on crate feature_ **fs** _only._
//!
//! To download and simultaneously unpack the tarball of a snapshots using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # #[cfg(feature = "fs")]
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//! use std::path::Path;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let snapshot = client.snapshot().get("SNAPSHOT_DIGEST").await?.unwrap();
//!
//! // Note: the directory must already exist, and the user running the binary must have read/write access to it.
//! let target_directory = Path::new("/home/user/download/");
//! client
//!    .snapshot()
//!    .download_unpack(&snapshot, target_directory)
//!    .await?;
//! #
//! #    Ok(())
//! # }
//! ```
//!
//! # Add statistics
//! **Note:** _Available on crate feature_ **fs** _only._
//!
//! Increments the aggregator snapshot download statistics using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # #[cfg(feature = "fs")]
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//! use std::path::Path;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let snapshot = client.snapshot().get("SNAPSHOT_DIGEST").await?.unwrap();
//!
//! // Note: the directory must already exist, and the user running the binary must have read/write access to it.
//! let target_directory = Path::new("/home/user/download/");
//! client
//!    .snapshot()
//!    .download_unpack(&snapshot, target_directory)
//!    .await?;
//!
//! client.snapshot().add_statistics(&snapshot).await.unwrap();
//! #
//! #    Ok(())
//! # }
//! ```

use anyhow::Context;
#[cfg(feature = "fs")]
use slog::Logger;
use std::sync::Arc;
use thiserror::Error;

#[cfg(feature = "fs")]
use mithril_common::entities::CompressionAlgorithm;

use crate::aggregator_client::{AggregatorClient, AggregatorClientError, AggregatorRequest};
#[cfg(feature = "fs")]
use crate::feedback::FeedbackSender;
#[cfg(feature = "fs")]
use crate::file_downloader::{DownloadEvent, FileDownloader};
use crate::{MithrilResult, Snapshot, SnapshotListItem};

/// Error for the Snapshot client
#[derive(Error, Debug)]
pub enum SnapshotClientError {
    /// Download location does not work
    #[error("Could not find a working download location for the snapshot digest '{digest}', tried location: {{'{locations}'}}.")]
    NoWorkingLocation {
        /// given digest
        digest: String,

        /// list of locations tried
        locations: String,
    },
}

/// Aggregator client for the snapshot artifact
pub struct SnapshotClient {
    aggregator_client: Arc<dyn AggregatorClient>,
    #[cfg(feature = "fs")]
    http_file_downloader: Arc<dyn FileDownloader>,
    #[cfg(feature = "fs")]
    _feedback_sender: FeedbackSender,
    #[cfg(feature = "fs")]
    logger: Logger,
}

impl SnapshotClient {
    /// Constructs a new `SnapshotClient`.
    pub fn new(
        aggregator_client: Arc<dyn AggregatorClient>,
        #[cfg(feature = "fs")] http_file_downloader: Arc<dyn FileDownloader>,
        #[cfg(feature = "fs")] feedback_sender: FeedbackSender,
        #[cfg(feature = "fs")] logger: Logger,
    ) -> Self {
        Self {
            aggregator_client,
            #[cfg(feature = "fs")]
            http_file_downloader,
            // The underscore prefix prevents breaking the `SnapshotClient` API compatibility.
            #[cfg(feature = "fs")]
            _feedback_sender: feedback_sender,
            #[cfg(feature = "fs")]
            logger: mithril_common::logging::LoggerExtensions::new_with_component_name::<Self>(
                &logger,
            ),
        }
    }

    /// Return a list of available snapshots
    pub async fn list(&self) -> MithrilResult<Vec<SnapshotListItem>> {
        let response = self
            .aggregator_client
            .get_content(AggregatorRequest::ListSnapshots)
            .await
            .with_context(|| "Snapshot Client can not get the artifact list")?;
        let items = serde_json::from_str::<Vec<SnapshotListItem>>(&response)
            .with_context(|| "Snapshot Client can not deserialize artifact list")?;

        Ok(items)
    }

    /// Get the given snapshot data. If it cannot be found, a None is returned.
    pub async fn get(&self, digest: &str) -> MithrilResult<Option<Snapshot>> {
        match self
            .aggregator_client
            .get_content(AggregatorRequest::GetSnapshot {
                digest: digest.to_string(),
            })
            .await
        {
            Ok(content) => {
                let snapshot: Snapshot = serde_json::from_str(&content)
                    .with_context(|| "Snapshot Client can not deserialize artifact")?;

                Ok(Some(snapshot))
            }
            Err(AggregatorClientError::RemoteServerLogical(_)) => Ok(None),
            Err(e) => Err(e.into()),
        }
    }

    cfg_fs! {
        /// Download and unpack the given snapshot to the given directory
        ///
        /// **NOTE**: The directory should already exist, and the user running the binary
        /// must have read/write access to it.
        pub async fn download_unpack(
            &self,
            snapshot: &Snapshot,
            target_dir: &std::path::Path,
        ) -> MithrilResult<()> {
            use crate::feedback::MithrilEvent;
            let download_id = MithrilEvent::new_snapshot_download_id();

            self.download_unpack_file
                (
                    &snapshot.digest,
                    &snapshot.locations,
                    snapshot.size,
                    target_dir,
                    snapshot.compression_algorithm,
                    DownloadEvent::Full {
                        download_id: download_id.clone(),
                        digest: snapshot.digest.clone(),
                    },
                )
                .await?;

            if let Some(ancillary_locations) = &snapshot.ancillary_locations {
                self.download_unpack_file(
                    &snapshot.digest,
                    ancillary_locations,
                    snapshot.ancillary_size.unwrap_or(0),
                    target_dir,
                    snapshot.compression_algorithm,
                    DownloadEvent::FullAncillary {
                        download_id: download_id.clone(),
                    },
                )
                .await?;
            }

            Ok(())
        }

        async fn download_unpack_file(&self,
            digest: &str,
            locations: &[String],
            size: u64,
            target_dir: &std::path::Path,
            compression_algorithm: CompressionAlgorithm,
            download_event: DownloadEvent
        ) -> MithrilResult<()> {
            for location in locations {
                let file_downloader_uri = location.to_owned().into();

                if let Err(error) = self
                    .http_file_downloader
                    .download_unpack(
                        &file_downloader_uri,
                        size,
                        target_dir,
                        Some(compression_algorithm),
                        download_event.clone(),
                    )
                    .await
                {
                    slog::warn!(self.logger, "Failed downloading snapshot from '{location}'"; "error" => ?error);
                } else {
                    return Ok(());
                }
            }

            let locations = locations.join(", ");

            Err(SnapshotClientError::NoWorkingLocation {
                digest: digest.to_string(),
                locations,
            }
            .into())
        }
    }

    /// Increments the aggregator snapshot download statistics
    pub async fn add_statistics(&self, snapshot: &Snapshot) -> MithrilResult<()> {
        let _response = self
            .aggregator_client
            .post_content(AggregatorRequest::IncrementSnapshotStatistic {
                snapshot: serde_json::to_string(snapshot)?,
            })
            .await?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(feature = "fs")]
    mod download_unpack_file {
        use std::path::PathBuf;

        use mithril_common::temp_dir_create;

        use crate::aggregator_client::MockAggregatorClient;
        use crate::common::CompressionAlgorithm;
        use crate::feedback::MithrilEvent;
        use crate::file_downloader::{MockFileDownloader, MockFileDownloaderBuilder};
        use crate::test_utils::TestLogger;

        use super::*;

        fn setup_snapshot_client(file_downloader: Arc<dyn FileDownloader>) -> SnapshotClient {
            let aggregator_client = Arc::new(MockAggregatorClient::new());
            let logger = TestLogger::stdout();

            SnapshotClient::new(
                aggregator_client,
                file_downloader,
                FeedbackSender::new(&[]),
                logger.clone(),
            )
        }

        fn dummy_download_event() -> DownloadEvent {
            DownloadEvent::Full {
                download_id: MithrilEvent::new_snapshot_download_id(),
                digest: "test-digest".to_string(),
            }
        }

        #[tokio::test]
        async fn log_warning_if_location_fails() {
            let log_path = temp_dir_create!().join("test.log");
            let mock_downloader = MockFileDownloaderBuilder::default()
                .with_file_uri("http://whatever.co/snapshot")
                .with_failure()
                .build();

            {
                let client = SnapshotClient {
                    logger: TestLogger::file(&log_path),
                    ..setup_snapshot_client(Arc::new(mock_downloader))
                };

                let _result = client
                    .download_unpack_file(
                        "test-digest",
                        &["http://whatever.co/snapshot".to_string()],
                        19,
                        &PathBuf::from("/whatever"),
                        CompressionAlgorithm::Gzip,
                        dummy_download_event(),
                    )
                    .await;
            }

            let logs = std::fs::read_to_string(&log_path).unwrap();
            assert!(logs.contains("Failed downloading snapshot"));
        }

        #[tokio::test]
        async fn error_contains_joined_locations_list_when_all_locations_fail() {
            let test_locations = vec![
                "http://example.com/snapshot1".to_string(),
                "http://example.com/snapshot2".to_string(),
            ];
            let mock_downloader = MockFileDownloaderBuilder::default()
                .with_file_uri("http://example.com/snapshot1")
                .with_failure()
                .next_call()
                .with_file_uri("http://example.com/snapshot2")
                .with_failure()
                .build();
            let client = setup_snapshot_client(Arc::new(mock_downloader));

            let error = client
                .download_unpack_file(
                    "test-digest",
                    &test_locations,
                    19,
                    &PathBuf::from("/whatever"),
                    CompressionAlgorithm::Gzip,
                    dummy_download_event(),
                )
                .await
                .expect_err("Should fail when all locations fail");

            if let Some(SnapshotClientError::NoWorkingLocation { digest, locations }) =
                error.downcast_ref::<SnapshotClientError>()
            {
                assert_eq!(digest, "test-digest");
                assert_eq!(
                    locations,
                    "http://example.com/snapshot1, http://example.com/snapshot2"
                );
            } else {
                panic!("Expected SnapshotClientError::NoWorkingLocation, but got: {error:?}");
            }
        }

        #[tokio::test]
        async fn fallback_to_another_location() {
            let mock_downloader = MockFileDownloaderBuilder::default()
                .with_file_uri("http://example.com/snapshot1")
                .with_failure()
                .next_call()
                .with_file_uri("http://example.com/snapshot2")
                .with_success()
                .build();
            let client = setup_snapshot_client(Arc::new(mock_downloader));

            client
                .download_unpack_file(
                    "test-digest",
                    &[
                        "http://example.com/snapshot1".to_string(),
                        "http://example.com/snapshot2".to_string(),
                    ],
                    19,
                    &PathBuf::from("/whatever"),
                    CompressionAlgorithm::Gzip,
                    dummy_download_event(),
                )
                .await
                .expect("Should succeed when fallbacking to another location");
        }

        #[tokio::test]
        async fn fail_if_location_list_is_empty() {
            let client = setup_snapshot_client(Arc::new(MockFileDownloader::new()));

            let error = client
                .download_unpack_file(
                    "test-digest",
                    &Vec::new(),
                    19,
                    &PathBuf::from("/whatever"),
                    CompressionAlgorithm::Gzip,
                    dummy_download_event(),
                )
                .await
                .expect_err("Should fail with empty location list");

            if let Some(SnapshotClientError::NoWorkingLocation { locations, .. }) =
                error.downcast_ref::<SnapshotClientError>()
            {
                assert_eq!(locations, "");
            } else {
                panic!("Expected SnapshotClientError::NoWorkingLocation, but got: {error:?}");
            }
        }
    }
}
