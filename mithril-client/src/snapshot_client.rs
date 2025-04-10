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
