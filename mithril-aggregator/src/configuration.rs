use config::ConfigError;
use mithril_common::entities::ProtocolParameters;
use serde::{Deserialize, Serialize};
use std::error::Error;
use std::path::PathBuf;
use std::sync::Arc;

use mithril_common::{store::adapter::JsonFileStoreAdapter, CardanoNetwork};

use crate::dependency::{SnapshotStoreWrapper, SnapshotUploaderWrapper};
use crate::snapshot_stores::LocalSnapshotStore;
use crate::tools::GcpFileUploader;
use crate::{LocalSnapshotUploader, RemoteSnapshotStore, RemoteSnapshotUploader};

// TODO: 'LIST_SNAPSHOTS_MAX_ITEMS' keep as const or in config, or add a parameter to `list_snapshots`?
const LIST_SNAPSHOTS_MAX_ITEMS: usize = 20;

/// Aggregator configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Configuration {
    /// Cardano CLI tool path
    pub cardano_cli_path: PathBuf,

    /// Path of the socket used by the Cardano CLI tool
    /// to communicate with the Cardano node
    pub cardano_node_socket_path: PathBuf,

    /// Cardano Network Magic number
    ///
    /// useful for TestNet & DevNet
    pub network_magic: Option<u64>,

    /// Cardano network
    pub network: String,

    /// Protocol parameters
    pub protocol_parameters: ProtocolParameters,

    /// Snapshots manifest location
    pub url_snapshot_manifest: String,

    /// Type of snapshot store to use
    pub snapshot_store_type: SnapshotStoreType,

    /// Type of snapshot uploader to use
    pub snapshot_uploader_type: SnapshotUploaderType,

    /// Server listening IP
    pub server_url: String,

    /// Run Interval is the interval between two runtime cycles in ms
    pub run_interval: u64,

    /// Directory to snapshot
    pub db_directory: PathBuf,

    /// Directory to store snapshot
    pub snapshot_directory: PathBuf,

    /// Directory to store aggregator data (Certificates, Snapshots, Protocol Parameters, ...)
    pub data_stores_directory: PathBuf,

    /// Genesis verification key
    pub genesis_verification_key: String,
}

/// Snapshot store type enumerates the different kinds of snapshot stores.
/// Local storage is mainly used by development and test environements while GCP
/// is intended for production use.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum SnapshotStoreType {
    /// Google storage.
    Gcp,
    /// Local hard drive storage.
    Local,
}

/// Uploader needed to copy the snapshot once computed.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum SnapshotUploaderType {
    /// Uploader to GCP storage.
    Gcp,
    /// Uploader to local storage.
    Local,
}

impl Configuration {
    /// Create a snapshot store from the configuration settings.
    pub fn build_snapshot_store(&self) -> Result<SnapshotStoreWrapper, Box<dyn Error>> {
        match self.snapshot_store_type {
            SnapshotStoreType::Gcp => Ok(Arc::new(RemoteSnapshotStore::new(
                Box::new(GcpFileUploader::default()),
                self.url_snapshot_manifest.clone(),
            ))),
            SnapshotStoreType::Local => Ok(Arc::new(LocalSnapshotStore::new(
                Box::new(JsonFileStoreAdapter::new(
                    self.data_stores_directory.join("snapshot_db"),
                )?),
                LIST_SNAPSHOTS_MAX_ITEMS,
            ))),
        }
    }

    /// Create a snapshot uploader from configuration settings.
    pub fn build_snapshot_uploader(&self) -> SnapshotUploaderWrapper {
        match self.snapshot_uploader_type {
            SnapshotUploaderType::Gcp => Arc::new(RemoteSnapshotUploader::new(Box::new(
                GcpFileUploader::default(),
            ))),
            SnapshotUploaderType::Local => Arc::new(LocalSnapshotUploader::new(
                self.server_url.clone(),
                &self.snapshot_directory,
            )),
        }
    }

    /// Check configuration and return a representation of the Cardano network.
    pub fn get_network(&self) -> Result<CardanoNetwork, ConfigError> {
        CardanoNetwork::from_code(self.network.clone(), self.network_magic)
            .map_err(|e| ConfigError::Message(e.to_string()))
    }
}
