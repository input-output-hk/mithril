use config::ConfigError;
use serde::{Deserialize, Serialize};
use std::error::Error;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;

use mithril_common::store::adapter::JsonFileStoreAdapter;
use mithril_common::{CardanoNetwork, MagicId};

use crate::dependency::{SnapshotStoreWrapper, SnapshotUploaderWrapper};
use crate::snapshot_stores::LocalSnapshotStore;
use crate::tools::GcpFileUploader;
use crate::{LocalSnapshotUploader, RemoteSnapshotStore, RemoteSnapshotUploader};

// TODO: 'LIST_SNAPSHOTS_MAX_ITEMS' keep as const or in config, or add a parameter to `list_snapshots`?
const LIST_SNAPSHOTS_MAX_ITEMS: usize = 5;

/// Aggregator configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    /// Cardano Network Magic number
    ///
    /// useful for TestNet & DevNet
    pub network_magic: Option<u64>,

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

    /// Run Interval is the interval between two runtime cycles in ms
    pub run_interval: u64,

    /// Directory to snapshot
    pub db_directory: PathBuf,

    /// Directory to store snapshot
    pub snapshot_directory: PathBuf,

    /// Directory to store snapshot records
    pub snapshot_store_directory: PathBuf,

    /// Directory to store pending certificates
    pub pending_certificate_store_directory: PathBuf,

    /// Directory to store certificates
    pub certificate_store_directory: PathBuf,

    /// Directory to store verification keys
    pub verification_key_store_directory: PathBuf,

    /// Directory to store stakes
    pub stake_store_directory: PathBuf,

    /// Directory to single signatures
    pub single_signature_store_directory: PathBuf,
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
    pub fn build_snapshot_store(&self) -> Result<SnapshotStoreWrapper, Box<dyn Error>> {
        match self.snapshot_store_type {
            SnapshotStoreType::Gcp => Ok(Arc::new(RwLock::new(RemoteSnapshotStore::new(
                Box::new(GcpFileUploader::default()),
                self.url_snapshot_manifest.clone(),
            )))),
            SnapshotStoreType::Local => Ok(Arc::new(RwLock::new(LocalSnapshotStore::new(
                Box::new(JsonFileStoreAdapter::new(
                    self.snapshot_store_directory.clone(),
                )?),
                LIST_SNAPSHOTS_MAX_ITEMS,
            )))),
        }
    }

    pub fn build_snapshot_uploader(&self) -> SnapshotUploaderWrapper {
        match self.snapshot_uploader_type {
            SnapshotUploaderType::Gcp => Arc::new(RwLock::new(RemoteSnapshotUploader::new(
                Box::new(GcpFileUploader::default()),
            ))),
            SnapshotUploaderType::Local => Arc::new(RwLock::new(LocalSnapshotUploader::new(
                self.server_url.clone(),
            ))),
        }
    }

    pub fn get_network(&self) -> Result<CardanoNetwork, ConfigError> {
        match self.network.to_lowercase().as_str() {
            "mainnet" => Ok(CardanoNetwork::MainNet),
            "testnet" => {
                if let Some(magic) = self.network_magic {
                    Ok(CardanoNetwork::TestNet(magic))
                } else {
                    Err(ConfigError::Message(
                        "no NETWORK MAGIC number given for testnet network".to_string(),
                    ))
                }
            }
            "devnet" => {
                if let Some(magic) = self.network_magic {
                    Ok(CardanoNetwork::DevNet(magic))
                } else {
                    Err(ConfigError::Message(
                        "no NETWORK MAGIC number given for devnet network".to_string(),
                    ))
                }
            }
            what => Err(ConfigError::Message(format!(
                "could not parse network '{}', the only recognized networks are: mainnet, devnet, testnet",
                what
            ))),
        }
    }
}
