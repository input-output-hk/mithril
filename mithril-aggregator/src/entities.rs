use crate::dependency::SnapshotStoreWrapper;
use crate::snapshot_stores::LocalSnapshotStore;
use crate::snapshot_uploaders::SnapshotUploader;
use crate::tools::GcpFileUploader;
use crate::{LocalSnapshotUploader, RemoteSnapshotStore, RemoteSnapshotUploader};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;

/// Aggregator configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    /// Cardano network
    pub network: String,

    /// Snapshots manifest location
    pub url_snapshot_manifest: String,

    /// Type of snapshot store to use
    pub snapshot_store_type: SnapshotStoreType,

    /// Type of snapshot uploader to use
    pub snapshot_uploader_type: SnapshotUploaderType,

    /// Server listening IP
    pub server_url: String,

    /// Directory to snapshot
    pub db_directory: PathBuf,

    /// Directory to store snapshot
    pub snapshot_directory: PathBuf,

    /// Directory to store pending certificates
    pub pending_certificate_store_directory: PathBuf,

    /// Directory to store certificates
    pub certificate_store_directory: PathBuf,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum SnapshotStoreType {
    Gcp,
    Local,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum SnapshotUploaderType {
    Gcp,
    Local,
}

impl Config {
    pub fn build_snapshot_store(&self) -> SnapshotStoreWrapper {
        match self.snapshot_store_type {
            SnapshotStoreType::Gcp => Arc::new(RwLock::new(RemoteSnapshotStore::new(
                Box::new(GcpFileUploader::default()),
                self.url_snapshot_manifest.clone(),
            ))),
            SnapshotStoreType::Local => Arc::new(RwLock::new(LocalSnapshotStore::new())),
        }
    }

    pub fn build_snapshot_uploader(&self) -> Box<dyn SnapshotUploader> {
        match self.snapshot_store_type {
            SnapshotStoreType::Gcp => Box::new(RemoteSnapshotUploader::new(Box::new(
                GcpFileUploader::default(),
            ))),
            SnapshotStoreType::Local => {
                Box::new(LocalSnapshotUploader::new(self.server_url.clone()))
            }
        }
    }
}
