//! This module contains a struct to exchange snapshot information with the Aggregator

use anyhow::Context;
use slog_scope::warn;
use std::{path::Path, sync::Arc};
use thiserror::Error;

use crate::aggregator_client::AggregatorClient;
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
}

impl SnapshotClient {
    /// constructor
    pub fn new(aggregator_client: Arc<dyn AggregatorClient>) -> Self {
        Self { aggregator_client }
    }

    /// Return a list of available snapshots
    pub async fn list(&self) -> MithrilResult<Vec<SnapshotListItem>> {
        let url = "artifact/snapshots";
        let response = self
            .aggregator_client
            .get_content(url)
            .await
            .with_context(|| format!("Snapshot Client can not get the artifact list at '{url}'"))?;
        let items = serde_json::from_str::<Vec<SnapshotListItem>>(&response)
            .with_context(|| "Snapshot Client can not deserialize artifact list")?;

        Ok(items)
    }

    /// Return a snapshot based on the given digest (list to get the digests)
    pub async fn show(&self, digest: &str) -> MithrilResult<Snapshot> {
        let url = format!("artifact/snapshot/{}", digest);
        let response = self
            .aggregator_client
            .get_content(&url)
            .await
            .with_context(|| format!("Snapshot Client can not get the artifact at '{url}'"))?;
        let message = serde_json::from_str::<Snapshot>(&response).with_context(|| {
            format!("Snapshot Client can not deserialize artifact for digest '{digest}'")
        })?;

        Ok(message)
    }

    /// Download and unpack the given snapshot to the given directory
    pub async fn download_unpack(
        &self,
        snapshot: &Snapshot,
        target_dir: &Path,
    ) -> MithrilResult<()> {
        for url in snapshot.locations.as_slice() {
            if self.aggregator_client.probe(url).await.is_ok() {
                return match self
                    .aggregator_client
                    .download_unpack(
                        url,
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
