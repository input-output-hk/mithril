//! A client to retrieve snapshots data from an Aggregator.
//!
//! In order to do so it defines a [SnapshotClient] which exposes the following features:
//!  - [get][SnapshotClient::get]: get a single snapshot data from its digest
//!  - [list][SnapshotClient::list]: get the list of available snapshots
//!  - [download_unpack_full][SnapshotClient::download_unpack_full]: download and unpack the tarball
//!    of a snapshot and its ancillary files to a directory, use this function if you want to fast bootstrap
//!    a Cardano node
//!  - [download_unpack][SnapshotClient::download_unpack]: download and unpack the tarball of a snapshot
//!    to a directory (immutable files only)
//!
//! **Note:** Ancillary files are files that are not part of the snapshot but are needed to enable fast
//! bootstrapping of the Cardano node.
//! They include the ledger files and the latest unfinished immutable files.
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
//! let snapshot = client.cardano_database().get("SNAPSHOT_DIGEST").await?.unwrap();
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
//! let snapshots = client.cardano_database().list().await?;
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
//! To download and simultaneously unpack the tarball of a snapshot using the [ClientBuilder][crate::client::ClientBuilder]
//! , including its ancillary files, to a directory.
//!
//! ```no_run
//! # #[cfg(feature = "fs")]
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//! use std::path::Path;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY")
//!     .set_ancillary_verification_key("YOUR_ANCILLARY_VERIFICATION_KEY".to_string())
//!     .build()?;
//! let snapshot = client.cardano_database().get("SNAPSHOT_DIGEST").await?.unwrap();
//!
//! // Note: the directory must already exist, and the user running the binary must have read/write access to it.
//! let target_directory = Path::new("/home/user/download/");
//! client
//!    .cardano_database()
//!    .download_unpack_full(&snapshot, target_directory)
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
//! let snapshot = client.cardano_database().get("SNAPSHOT_DIGEST").await?.unwrap();
//!
//! // Note: the directory must already exist, and the user running the binary must have read/write access to it.
//! let target_directory = Path::new("/home/user/download/");
//! client
//!    .cardano_database()
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
#[cfg(feature = "fs")]
use crate::utils::AncillaryVerifier;
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
    /// Missing ancillary verifier
    #[error("Ancillary verifier is not set, please use `set_ancillary_verification_key` when creating the client")]
    MissingAncillaryVerifier,
}

/// Aggregator client for the snapshot artifact
pub struct SnapshotClient {
    aggregator_client: Arc<dyn AggregatorClient>,
    #[cfg(feature = "fs")]
    http_file_downloader: Arc<dyn FileDownloader>,
    #[cfg(feature = "fs")]
    ancillary_verifier: Option<Arc<AncillaryVerifier>>,
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
        #[cfg(feature = "fs")] ancillary_verifier: Option<Arc<AncillaryVerifier>>,
        #[cfg(feature = "fs")] feedback_sender: FeedbackSender,
        #[cfg(feature = "fs")] logger: Logger,
    ) -> Self {
        Self {
            aggregator_client,
            #[cfg(feature = "fs")]
            http_file_downloader,
            #[cfg(feature = "fs")]
            ancillary_verifier,
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
        /// Download and unpack the given snapshot, including its ancillary files, to the given directory
        ///
        /// Ancillary files are files that are not part of the snapshot but are needed to enable fast
        /// bootstrapping of the Cardano node.
        /// They include the ledger files and the latest unfinished immutable files.
        ///
        /// **NOTE**: The target directory should already exist, and the user running the binary
        /// must have read/write access to it.
        pub async fn download_unpack_full(
            &self,
            snapshot: &Snapshot,
            target_dir: &std::path::Path,
        ) -> MithrilResult<()> {
            use crate::feedback::MithrilEvent;
            if self.ancillary_verifier.is_none() {
                return Err(SnapshotClientError::MissingAncillaryVerifier.into());
            }

            let download_id = MithrilEvent::new_snapshot_download_id();
            self.download_unpack_immutables_files(snapshot, target_dir, &download_id)
                .await?;
            self.download_unpack_ancillary(snapshot, target_dir, &download_id)
                .await?;

            Ok(())
        }

        /// Download and unpack the given immutable files of the snapshot to the given directory
        ///
        /// Ancillary files are not included in this operation, if they are needed, use
        /// [download_unpack_full][Self::download_unpack_full] instead.
        ///
        /// **NOTE**: The target directory should already exist, and the user running the binary
        /// must have read/write access to it.
        pub async fn download_unpack(
            &self,
            snapshot: &Snapshot,
            target_dir: &std::path::Path,
        ) -> MithrilResult<()> {
            use crate::feedback::MithrilEvent;
            let download_id = MithrilEvent::new_snapshot_download_id();
            self.download_unpack_immutables_files(snapshot, target_dir, &download_id)
                .await?;

            Ok(())
        }

        async fn download_unpack_immutables_files(
            &self,
            snapshot: &Snapshot,
            target_dir: &std::path::Path,
            download_id: &str,
        ) -> MithrilResult<()> {
            self.download_unpack_file(
                &snapshot.digest,
                &snapshot.locations,
                snapshot.size,
                target_dir,
                snapshot.compression_algorithm,
                DownloadEvent::Full {
                    download_id: download_id.to_string(),
                    digest: snapshot.digest.clone(),
                },
            )
            .await?;

            Ok(())
        }

        async fn download_unpack_ancillary(
            &self,
            snapshot: &Snapshot,
            target_dir: &std::path::Path,
            download_id: &str,
        ) -> MithrilResult<()> {
            slog::info!(self.logger, "Ancillary verification don't use Mithril certification");

            match &snapshot.ancillary_locations {
                None => Ok(()),
                Some(ancillary_locations) => {
                    let temp_ancillary_unpack_dir = Self::ancillary_subdir(target_dir, download_id);
                    tokio::fs::create_dir(&temp_ancillary_unpack_dir)
                        .await
                        .with_context(|| {
                            format!(
                                "Snapshot Client can not create ancillary unpack directory '{}'",
                                temp_ancillary_unpack_dir.display()
                            )
                        })?;

                    let result = self
                        .download_unpack_verify_ancillary(
                            snapshot,
                            ancillary_locations,
                            snapshot.ancillary_size.unwrap_or(0),
                            target_dir,
                            &temp_ancillary_unpack_dir,
                            download_id,
                        )
                        .await;

                    if let Err(e) = std::fs::remove_dir_all(&temp_ancillary_unpack_dir) {
                        slog::warn!(
                            self.logger, "Failed to remove ancillary unpack directory '{}'", temp_ancillary_unpack_dir.display();
                            "error" => ?e
                        );
                    }

                    result
                }
            }
        }

        async fn download_unpack_verify_ancillary(
            &self,
            snapshot: &Snapshot,
            ancillary_locations: &[String],
            ancillary_size: u64,
            target_dir: &std::path::Path,
            temp_ancillary_unpack_dir: &std::path::Path,
            download_id: &str,
        ) -> MithrilResult<()> {
            self.download_unpack_file(
                &snapshot.digest,
                ancillary_locations,
                ancillary_size,
                temp_ancillary_unpack_dir,
                snapshot.compression_algorithm,
                DownloadEvent::FullAncillary {
                    download_id: download_id.to_string(),
                },
            )
            .await?;

            let ancillary_verifier = self
                .ancillary_verifier
                .as_ref()
                .ok_or(SnapshotClientError::MissingAncillaryVerifier)?;

            let validated_manifest = ancillary_verifier.verify(temp_ancillary_unpack_dir).await?;
            validated_manifest
                .move_to_final_location(target_dir)
                .await?;

            Ok(())
        }

        async fn download_unpack_file(
            &self,
            digest: &str,
            locations: &[String],
            size: u64,
            target_dir: &std::path::Path,
            compression_algorithm: CompressionAlgorithm,
            download_event: DownloadEvent,
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

        fn ancillary_subdir(target_dir: &std::path::Path, download_id: &str) -> std::path::PathBuf {
            target_dir.join(format!("ancillary-{download_id}"))
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
    #[cfg(feature = "fs")]
    use std::path::PathBuf;

    #[cfg(feature = "fs")]
    use crate::{
        aggregator_client::MockAggregatorClient,
        common::CompressionAlgorithm,
        feedback::MithrilEvent,
        file_downloader::{MockFileDownloader, MockFileDownloaderBuilder},
        test_utils::TestLogger,
    };

    use super::*;

    #[cfg(feature = "fs")]
    use mithril_common::temp_dir_create;

    #[cfg(feature = "fs")]
    fn dummy_download_event() -> DownloadEvent {
        DownloadEvent::Full {
            download_id: MithrilEvent::new_snapshot_download_id(),
            digest: "test-digest".to_string(),
        }
    }

    #[cfg(feature = "fs")]
    fn setup_snapshot_client(
        file_downloader: Arc<dyn FileDownloader>,
        ancillary_verifier: Option<Arc<AncillaryVerifier>>,
    ) -> SnapshotClient {
        let aggregator_client = Arc::new(MockAggregatorClient::new());
        let logger = TestLogger::stdout();

        SnapshotClient::new(
            aggregator_client,
            file_downloader,
            ancillary_verifier,
            FeedbackSender::new(&[]),
            logger.clone(),
        )
    }

    #[cfg(feature = "fs")]
    mod download_unpack_file {
        use super::*;

        fn setup_snapshot_client(file_downloader: Arc<dyn FileDownloader>) -> SnapshotClient {
            super::setup_snapshot_client(file_downloader, None)
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

    #[cfg(feature = "fs")]
    mod download_unpack_full {
        use super::*;

        #[tokio::test]
        async fn fail_if_ancillary_verifier_is_not_set() {
            let snapshot = Snapshot {
                ancillary_locations: Some(vec!["http://example.com/ancillary".to_string()]),
                ancillary_size: Some(123),
                ..Snapshot::dummy()
            };

            let client = setup_snapshot_client(Arc::new(MockFileDownloader::new()), None);

            let error = client
                .download_unpack_full(&snapshot, &PathBuf::from("/whatever"))
                .await
                .expect_err("Should fail when ancillary verifier is not set");

            assert!(
                matches!(
                    error.downcast_ref::<SnapshotClientError>(),
                    Some(SnapshotClientError::MissingAncillaryVerifier)
                ),
                "Expected SnapshotClientError::MissingAncillaryVerifier, but got: {error:#?}"
            );
        }
    }

    #[cfg(feature = "fs")]
    mod download_unpack_ancillary {
        use mithril_common::crypto_helper::ManifestSigner;
        use mithril_common::test_utils::fake_keys;

        use crate::file_downloader::FakeAncillaryFileBuilder;

        use super::*;

        #[tokio::test]
        async fn log_a_info_message_telling_that_the_feature_does_not_use_mithril_certification() {
            let log_path = temp_dir_create!().join("test.log");
            let verification_key = fake_keys::manifest_verification_key()[0]
                .try_into()
                .unwrap();
            let snapshot = Snapshot {
                ancillary_locations: None,
                ancillary_size: None,
                ..Snapshot::dummy()
            };

            {
                let client = SnapshotClient {
                    logger: TestLogger::file(&log_path),
                    ..setup_snapshot_client(
                        Arc::new(MockFileDownloader::new()),
                        Some(Arc::new(AncillaryVerifier::new(verification_key))),
                    )
                };

                client
                    .download_unpack_ancillary(
                        &snapshot,
                        &PathBuf::from("/whatever"),
                        "test-download-id",
                    )
                    .await
                    .unwrap()
            }

            let logs = std::fs::read_to_string(&log_path).unwrap();
            assert!(logs.contains("Ancillary verification don't use Mithril certification"));
        }

        #[tokio::test]
        async fn do_nothing_if_no_ancillary_locations_available_in_snapshot() {
            let verification_key = fake_keys::manifest_verification_key()[0]
                .try_into()
                .unwrap();
            let snapshot = Snapshot {
                ancillary_locations: None,
                ancillary_size: None,
                ..Snapshot::dummy()
            };

            let client = setup_snapshot_client(
                Arc::new(MockFileDownloader::new()),
                Some(Arc::new(AncillaryVerifier::new(verification_key))),
            );

            client
                .download_unpack_ancillary(
                    &snapshot,
                    &PathBuf::from("/whatever"),
                    "test-download-id",
                )
                .await
                .expect("Should succeed when no ancillary locations are available");
        }

        #[tokio::test]
        async fn delete_temporary_unpack_subfolder_if_download_fail() {
            let test_dir = temp_dir_create!();
            let mock_downloader = MockFileDownloaderBuilder::default()
                .with_file_uri("http://example.com/ancillary")
                .with_failure()
                .build();
            let verification_key = fake_keys::manifest_verification_key()[0]
                .try_into()
                .unwrap();

            let client = setup_snapshot_client(
                Arc::new(mock_downloader),
                Some(Arc::new(AncillaryVerifier::new(verification_key))),
            );
            let snapshot = Snapshot {
                ancillary_locations: Some(vec!["http://example.com/ancillary".to_string()]),
                ancillary_size: Some(123),
                ..Snapshot::dummy()
            };

            client
                .download_unpack_ancillary(&snapshot, &test_dir, "test-download-id")
                .await
                .unwrap_err();

            assert!(!SnapshotClient::ancillary_subdir(&test_dir, "test-download-id").exists());
        }

        #[tokio::test]
        async fn delete_temporary_unpack_subfolder_if_verify_fail() {
            let test_dir = temp_dir_create!();
            let mock_downloader = MockFileDownloaderBuilder::default()
                .with_file_uri("http://example.com/ancillary")
                .with_success()
                .build();
            let verification_key = fake_keys::manifest_verification_key()[0]
                .try_into()
                .unwrap();

            let client = setup_snapshot_client(
                Arc::new(mock_downloader),
                Some(Arc::new(AncillaryVerifier::new(verification_key))),
            );
            let snapshot = Snapshot {
                ancillary_locations: Some(vec!["http://example.com/ancillary".to_string()]),
                ancillary_size: Some(123),
                ..Snapshot::dummy()
            };

            client
                .download_unpack_ancillary(&snapshot, &test_dir, "test-download-id")
                .await
                .unwrap_err();

            assert!(!SnapshotClient::ancillary_subdir(&test_dir, "test-download-id").exists());
        }

        #[tokio::test]
        async fn move_file_in_manifest_then_delete_temporary_unpack_subfolder_if_verify_succeed() {
            let test_dir = temp_dir_create!();
            let ancillary_signer = ManifestSigner::create_deterministic_signer();
            let verification_key = ancillary_signer.verification_key();
            let mock_downloader = MockFileDownloaderBuilder::default()
                .with_file_uri("http://example.com/ancillary")
                .with_success_and_create_fake_ancillary_files(
                    FakeAncillaryFileBuilder::builder()
                        .files_in_manifest_to_create(vec!["dummy_ledger".to_string()])
                        .files_not_in_manifest_to_create(vec!["not_in_ancillary".to_string()])
                        .sign_manifest(ancillary_signer)
                        .build(),
                )
                .build();

            let client = setup_snapshot_client(
                Arc::new(mock_downloader),
                Some(Arc::new(AncillaryVerifier::new(verification_key))),
            );

            let snapshot = Snapshot {
                ancillary_locations: Some(vec!["http://example.com/ancillary".to_string()]),
                ancillary_size: Some(123),
                ..Snapshot::dummy()
            };
            client
                .download_unpack_ancillary(&snapshot, &test_dir, "test-download-id")
                .await
                .expect("Should succeed when ancillary verification is successful");

            assert!(test_dir.join("dummy_ledger").exists());
            assert!(!test_dir.join("not_in_ancillary").exists());
            assert!(!SnapshotClient::ancillary_subdir(&test_dir, "test-download-id").exists());
        }
    }
}
