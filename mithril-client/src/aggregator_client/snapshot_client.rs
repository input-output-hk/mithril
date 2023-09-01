//! This module contains a struct to exchange snapshot information with the Aggregator

use slog_scope::warn;
use std::{path::Path, sync::Arc};
use thiserror::Error;

use mithril_common::{
    entities::Snapshot,
    messages::{SnapshotListItemMessage, SnapshotListMessage, SnapshotMessage},
    StdResult,
};

use crate::aggregator_client::AggregatorClient;
use crate::utils::DownloadProgressReporter;

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
    http_client: Arc<dyn AggregatorClient>,
}

impl SnapshotClient {
    /// constructor
    pub fn new(http_client: Arc<dyn AggregatorClient>) -> Self {
        Self { http_client }
    }

    /// Return a list of available snapshots
    pub async fn list(&self) -> StdResult<Vec<SnapshotListItemMessage>> {
        let url = "artifact/snapshots";
        let response = self.http_client.get_content(url).await?;
        let items = serde_json::from_str::<SnapshotListMessage>(&response)?;

        Ok(items)
    }

    /// Return a snapshot based on the given digest (list to get the digests)
    pub async fn show(&self, digest: &str) -> StdResult<SnapshotMessage> {
        let url = format!("artifact/snapshot/{}", digest);
        let response = self.http_client.get_content(&url).await?;
        let message = serde_json::from_str::<SnapshotMessage>(&response)?;

        Ok(message)
    }

    /// Download and unpack the given snapshot to the given directory
    pub async fn download_unpack(
        &self,
        snapshot: &Snapshot,
        target_dir: &Path,
        progress_reporter: DownloadProgressReporter,
    ) -> StdResult<()> {
        for url in snapshot.locations.as_slice() {
            if self.http_client.probe(url).await.is_ok() {
                return match self
                    .http_client
                    .download_unpack(
                        url,
                        target_dir,
                        snapshot.compression_algorithm,
                        progress_reporter,
                    )
                    .await
                {
                    Ok(()) => Ok(()),
                    Err(e) => {
                        warn!("Failed downloading snapshot from '{url}' Error: {e}.");
                        Err(e.into())
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
