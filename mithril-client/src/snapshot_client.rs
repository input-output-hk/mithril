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
//!
//! To download and simultaneously unpack the tarball of a snapshots using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
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

use anyhow::Context;
use slog::{warn, Logger};
use std::{path::Path, sync::Arc};
use thiserror::Error;

use crate::aggregator_client::{AggregatorClient, AggregatorClientError, AggregatorRequest};
use crate::feedback::{FeedbackSender, MithrilEvent};
use crate::snapshot_downloader::SnapshotDownloader;
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
    snapshot_downloader: Arc<dyn SnapshotDownloader>,
    feedback_sender: FeedbackSender,
    logger: Logger,
}

impl SnapshotClient {
    /// Constructs a new `SnapshotClient`.
    pub fn new(
        aggregator_client: Arc<dyn AggregatorClient>,
        snapshot_downloader: Arc<dyn SnapshotDownloader>,
        feedback_sender: FeedbackSender,
        logger: Logger,
    ) -> Self {
        Self {
            aggregator_client,
            snapshot_downloader,
            feedback_sender,
            logger,
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

    /// Download and unpack the given snapshot to the given directory
    ///
    /// **NOTE**: The directory should already exist, and the user running the binary
    /// must have read/write access to it.
    pub async fn download_unpack(
        &self,
        snapshot: &Snapshot,
        target_dir: &Path,
    ) -> MithrilResult<()> {
        for location in snapshot.locations.as_slice() {
            if self.snapshot_downloader.probe(location).await.is_ok() {
                let download_id = MithrilEvent::new_snapshot_download_id();
                self.feedback_sender
                    .send_event(MithrilEvent::SnapshotDownloadStarted {
                        digest: snapshot.digest.clone(),
                        download_id: download_id.clone(),
                        size: snapshot.size,
                    })
                    .await;
                return match self
                    .snapshot_downloader
                    .download_unpack(
                        location,
                        target_dir,
                        snapshot.compression_algorithm.unwrap_or_default(),
                        &download_id,
                        snapshot.size,
                    )
                    .await
                {
                    Ok(()) => {
                        // todo: add snapshot statistics to cli (it was previously done here)
                        // note: the snapshot download does not fail if the statistic call fails.
                        self.feedback_sender
                            .send_event(MithrilEvent::SnapshotDownloadComplete { download_id })
                            .await;
                        Ok(())
                    }
                    Err(e) => {
                        warn!(
                            self.logger,
                            "Failed downloading snapshot from '{location}' Error: {e}."
                        );
                        Err(e)
                    }
                };
            }
        }

        let locations = snapshot.locations.join(", ");

        Err(SnapshotClientError::NoWorkingLocation {
            digest: snapshot.digest.clone(),
            locations,
        }
        .into())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        aggregator_client::MockAggregatorHTTPClient, feedback::StackFeedbackReceiver,
        snapshot_downloader::MockHttpSnapshotDownloader, test_utils,
    };

    use super::*;

    #[tokio::test]
    async fn download_unpack_send_feedbacks() {
        let mut snapshot_downloader = MockHttpSnapshotDownloader::new();
        snapshot_downloader.expect_probe().returning(|_| Ok(()));
        snapshot_downloader
            .expect_download_unpack()
            .returning(|_, _, _, _, _| Ok(()));
        let feedback_receiver = Arc::new(StackFeedbackReceiver::new());
        let client = SnapshotClient::new(
            Arc::new(MockAggregatorHTTPClient::new()),
            Arc::new(snapshot_downloader),
            FeedbackSender::new(&[feedback_receiver.clone()]),
            test_utils::test_logger(),
        );
        let snapshot = Snapshot::dummy();

        client
            .download_unpack(&snapshot, Path::new(""))
            .await
            .expect("download should succeed");

        let actual = feedback_receiver.stacked_events();
        let id = actual[0].event_id();
        let expected = vec![
            MithrilEvent::SnapshotDownloadStarted {
                digest: snapshot.digest,
                download_id: id.to_string(),
                size: snapshot.size,
            },
            MithrilEvent::SnapshotDownloadComplete {
                download_id: id.to_string(),
            },
        ];

        assert_eq!(actual, expected);
    }
}
