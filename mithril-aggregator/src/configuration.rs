use config::{ConfigError, Map, Source, Value, ValueKind};
use serde::{Deserialize, Serialize};
use std::error::Error;
use std::path::PathBuf;
use std::sync::Arc;

use mithril_common::entities::ProtocolParameters;
use mithril_common::store::adapter::SQLiteAdapter;
use mithril_common::CardanoNetwork;

use crate::snapshot_stores::LocalSnapshotStore;
use crate::tools::GcpFileUploader;
use crate::{
    LocalSnapshotUploader, RemoteSnapshotStore, RemoteSnapshotUploader, SnapshotStore,
    SnapshotUploader,
};

// TODO: 'LIST_SNAPSHOTS_MAX_ITEMS' keep as const or in config, or add a parameter to `list_snapshots`?
const LIST_SNAPSHOTS_MAX_ITEMS: usize = 20;
const SQLITE_FILE: &str = "aggregator.sqlite3";

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
    pub server_ip: String,

    /// Server listening port
    pub server_port: u16,

    /// Run Interval is the interval between two runtime cycles in ms
    pub run_interval: u64,

    /// Directory of the node immutable files
    pub db_directory: PathBuf,

    /// Directory to store snapshot
    pub snapshot_directory: PathBuf,

    /// Directory to store aggregator data (Certificates, Snapshots, Protocol Parameters, ...)
    pub data_stores_directory: PathBuf,

    /// Genesis verification key
    pub genesis_verification_key: String,

    /// Max number of records in stores.
    /// When new records are added, oldest records are automatically deleted so
    /// there can always be at max the number of records specified by this
    /// setting.
    pub store_retention_limit: Option<usize>,
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
    /// Build the server URL from configuration.
    pub fn get_server_url(&self) -> String {
        format!("http://{}:{}/", self.server_ip, self.server_port)
    }

    /// Create a snapshot store from the configuration settings.
    pub fn build_snapshot_store(&self) -> Result<Arc<dyn SnapshotStore>, Box<dyn Error>> {
        match self.snapshot_store_type {
            SnapshotStoreType::Gcp => Ok(Arc::new(RemoteSnapshotStore::new(
                Box::new(GcpFileUploader::default()),
                self.url_snapshot_manifest.clone(),
            ))),
            SnapshotStoreType::Local => Ok(Arc::new(LocalSnapshotStore::new(
                Box::new(SQLiteAdapter::new(
                    "snapshot",
                    Some(self.get_sqlite_file()),
                )?),
                LIST_SNAPSHOTS_MAX_ITEMS,
            ))),
        }
    }

    /// Create a snapshot uploader from configuration settings.
    pub fn build_snapshot_uploader(&self) -> Arc<dyn SnapshotUploader> {
        match self.snapshot_uploader_type {
            SnapshotUploaderType::Gcp => Arc::new(RemoteSnapshotUploader::new(Box::new(
                GcpFileUploader::default(),
            ))),
            SnapshotUploaderType::Local => Arc::new(LocalSnapshotUploader::new(
                self.get_server_url(),
                &self.snapshot_directory,
            )),
        }
    }

    /// Check configuration and return a representation of the Cardano network.
    pub fn get_network(&self) -> Result<CardanoNetwork, ConfigError> {
        CardanoNetwork::from_code(self.network.clone(), self.network_magic)
            .map_err(|e| ConfigError::Message(e.to_string()))
    }

    /// Return the file of the SQLite stores. If the directory does not exist, it is created.
    pub fn get_sqlite_file(&self) -> PathBuf {
        let store_dir = &self.data_stores_directory;

        if !store_dir.exists() {
            std::fs::create_dir_all(store_dir).unwrap();
        }

        self.data_stores_directory.join(SQLITE_FILE)
    }
}

/// Configuration expected for Genesis commands.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenesisConfiguration {
    /// Cardano CLI tool path
    pub cardano_cli_path: PathBuf,

    /// Path of the socket used by the Cardano CLI tool
    /// to communicate with the Cardano node
    pub cardano_node_socket_path: PathBuf,

    /// Directory of the node immutable files
    pub db_directory: PathBuf,

    /// Cardano Network Magic number
    ///
    /// useful for TestNet & DevNet
    pub network_magic: Option<u64>,

    /// Cardano network
    pub network: String,

    /// Protocol parameters
    pub protocol_parameters: ProtocolParameters,

    /// Directory to store aggregator data (Certificates, Snapshots, Protocol Parameters, ...)
    pub data_stores_directory: PathBuf,

    /// Genesis verification key
    pub genesis_verification_key: String,

    /// Max number of records in stores.
    /// When new records are added, oldest records are automatically deleted so
    /// there can always be at max the number of records specified by this
    /// setting.
    pub store_retention_limit: Option<usize>,
}

impl GenesisConfiguration {
    /// Check configuration and return a representation of the Cardano network.
    pub fn get_network(&self) -> Result<CardanoNetwork, ConfigError> {
        CardanoNetwork::from_code(self.network.clone(), self.network_magic)
            .map_err(|e| ConfigError::Message(e.to_string()))
    }

    /// Return the file of the SQLite stores. If the directory does not exist, it is created.
    pub fn get_sqlite_file(&self) -> PathBuf {
        let store_dir = &self.data_stores_directory;

        if !store_dir.exists() {
            std::fs::create_dir_all(store_dir).unwrap();
        }

        self.data_stores_directory.join(SQLITE_FILE)
    }
}

/// Default configuration with all the default values for configurations.
#[derive(Debug, Clone)]
pub struct DefaultConfiguration {
    /// Server listening IP
    pub server_ip: String,

    /// Server listening port
    pub server_port: String,

    /// Directory of the node immutable files
    pub db_directory: String,

    /// Directory to store snapshot
    pub snapshot_directory: String,

    /// Type of snapshot store to use
    pub snapshot_store_type: String,

    /// Type of snapshot uploader to use
    pub snapshot_uploader_type: String,
}

impl Default for DefaultConfiguration {
    fn default() -> Self {
        Self {
            server_ip: "0.0.0.0".to_string(),
            server_port: "8080".to_string(),
            db_directory: "/db".to_string(),
            snapshot_directory: ".".to_string(),
            snapshot_store_type: "local".to_string(),
            snapshot_uploader_type: "gcp".to_string(),
        }
    }
}

impl Source for DefaultConfiguration {
    fn clone_into_box(&self) -> Box<dyn Source + Send + Sync> {
        Box::new(self.clone())
    }

    fn collect(&self) -> Result<Map<String, Value>, config::ConfigError> {
        let mut result = Map::new();
        let namespace = "default configuration".to_string();
        let myself = self.clone();
        result.insert(
            "server_ip".to_string(),
            Value::new(Some(&namespace), ValueKind::from(myself.server_ip)),
        );
        result.insert(
            "server_port".to_string(),
            Value::new(Some(&namespace), ValueKind::from(myself.server_port)),
        );
        result.insert(
            "db_directory".to_string(),
            Value::new(Some(&namespace), ValueKind::from(myself.db_directory)),
        );
        result.insert(
            "snapshot_directory".to_string(),
            Value::new(Some(&namespace), ValueKind::from(myself.snapshot_directory)),
        );
        result.insert(
            "snapshot_store_type".to_string(),
            Value::new(
                Some(&namespace),
                ValueKind::from(myself.snapshot_store_type),
            ),
        );
        result.insert(
            "snapshot_uploader_type".to_string(),
            Value::new(
                Some(&namespace),
                ValueKind::from(myself.snapshot_uploader_type),
            ),
        );

        Ok(result)
    }
}
