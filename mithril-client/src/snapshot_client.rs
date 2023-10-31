//! This module contains a struct to exchange snapshot information with the Aggregator

use anyhow::Context;
use slog_scope::warn;
use std::{path::Path, sync::Arc};
use thiserror::Error;

use crate::aggregator_client::{AggregatorClient, AggregatorClientError, AggregatorRequest};
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
}

impl SnapshotClient {
    /// constructor
    pub fn new(
        aggregator_client: Arc<dyn AggregatorClient>,
        snapshot_downloader: Arc<dyn SnapshotDownloader>,
    ) -> Self {
        Self {
            aggregator_client,
            snapshot_downloader,
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
    /// (the directory should already exist)
    pub async fn download_unpack(
        &self,
        snapshot: &Snapshot,
        target_dir: &Path,
    ) -> MithrilResult<()> {
        for location in snapshot.locations.as_slice() {
            if self.snapshot_downloader.probe(location).await.is_ok() {
                return match self
                    .snapshot_downloader
                    .download_unpack(
                        location,
                        target_dir,
                        snapshot.compression_algorithm.unwrap_or_default(),
                    )
                    .await
                {
                    Ok(()) => {
                        // todo: add snapshot statistics to cli (it was previously done here)
                        // note: the snapshot download does not fail if the statistic call fails.

                        Ok(())
                    }
                    Err(e) => {
                        warn!("Failed downloading snapshot from '{location}' Error: {e}.");
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
