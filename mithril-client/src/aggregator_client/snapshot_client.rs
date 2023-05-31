//! This module contains a struct to exchange snapshot information with the Aggregator

use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use mithril_common::{
    entities::Snapshot,
    messages::{SnapshotListMessage, SnapshotMessage},
    StdResult,
};
use slog_scope::warn;
use thiserror::Error;

use crate::{FromSnapshotListMessageAdapter, FromSnapshotMessageAdapter};

use super::AggregatorClient;

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
    pub async fn list(&self) -> StdResult<Vec<Snapshot>> {
        let url = "artifact/snapshots";
        let response = self.http_client.get_content(url).await?;
        let message = serde_json::from_str::<SnapshotListMessage>(&response)?;
        let snapshots = FromSnapshotListMessageAdapter::adapt(message);

        Ok(snapshots)
    }

    /// Return a snapshot based on the given digest (list to get the digests)
    pub async fn show(&self, digest: &str) -> StdResult<Snapshot> {
        let url = format!("artifact/snapshot/{}", digest);
        let response = self.http_client.get_content(&url).await?;
        let message = serde_json::from_str::<SnapshotMessage>(&response)?;
        let snapshot = FromSnapshotMessageAdapter::adapt(message);

        Ok(snapshot)
    }

    /// Download the snapshot identified by the given snapshot in the given directory
    pub async fn download(&self, snapshot: &Snapshot, download_dir: &Path) -> StdResult<PathBuf> {
        let filepath = PathBuf::new()
            .join(download_dir)
            .join(format!("snapshot-{}", snapshot.digest));

        for url in snapshot.locations.as_slice() {
            match self.http_client.download(url, &filepath).await {
                Ok(()) => return Ok(filepath),
                Err(e) => {
                    warn!("Failed downloading snapshot from '{url}' Error: {e}.");
                }
            };
        }

        let locations = snapshot.locations.join(", ");

        Err(SnapshotClientError::NoWorkingLocation {
            digest: snapshot.digest.clone(),
            locations,
        }
        .into())
    }
}
