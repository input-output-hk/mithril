//! This module contains a struct to exchange snapshot information with the Aggregator

use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use mithril_common::{
    entities::Snapshot,
    messages::{SnapshotListMessage, SnapshotMessage},
    StdError, StdResult,
};
use slog_scope::warn;
use thiserror::Error;

use crate::{FromSnapshotListMessageAdapter, FromSnapshotMessageAdapter};

use super::AggregatorClient;

#[derive(Error, Debug)]
pub enum SnapshotClientError {
    #[error("Could not find a working download location for the snapshot digest '{digest}', tried location: {{'{locations}'}}.")]
    NoWorkingLocation { digest: String, locations: String },

    #[error("subsystem error: '{message}'; nested error: {error}")]
    SubsystemError { message: String, error: StdError },
}

pub struct SnapshotClient {
    http_client: Arc<dyn AggregatorClient>,
    download_dir: PathBuf,
}

impl SnapshotClient {
    pub fn new(http_client: Arc<dyn AggregatorClient>, download_dir: &Path) -> Self {
        Self {
            http_client,
            download_dir: download_dir.to_owned(),
        }
    }

    pub async fn list(&self) -> StdResult<Vec<Snapshot>> {
        let url = "/artifact/snapshots";
        let response = self.http_client.get_json(url).await?;
        let message = serde_json::from_str::<SnapshotListMessage>(&response)?;
        let snapshots = FromSnapshotListMessageAdapter::adapt(message);

        Ok(snapshots)
    }

    pub async fn show(&self, digest: &str) -> StdResult<Snapshot> {
        let url = format!("/artifact/snapshot/{}", digest);
        let response = self.http_client.get_json(&url).await?;
        let message = serde_json::from_str::<SnapshotMessage>(&response)?;
        let snapshot = FromSnapshotMessageAdapter::adapt(message);

        Ok(snapshot)
    }

    pub async fn download(&self, snapshot: &Snapshot) -> StdResult<String> {
        let filepath = PathBuf::new()
            .join(&self.download_dir)
            .join(format!("snapshot-{}", snapshot.digest));

        while let Some(url) = snapshot.locations.iter().next() {
            match self.http_client.download(url, &filepath).await {
                Ok(()) => return Ok(filepath.display().to_string()),
                Err(e) => {
                    warn!("Failed downloading snapshot from '{url}'.");
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
