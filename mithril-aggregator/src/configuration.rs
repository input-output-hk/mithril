use anyhow::anyhow;
use config::{ConfigError, Map, Source, Value, ValueKind};
use mithril_common::crypto_helper::ProtocolGenesisSigner;
use mithril_common::era::adapters::EraReaderAdapterType;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::str::FromStr;

use mithril_common::entities::{
    CompressionAlgorithm, HexEncodedGenesisVerificationKey, ProtocolParameters,
};
use mithril_common::{CardanoNetwork, StdResult};

/// Different kinds of execution environments
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub enum ExecutionEnvironment {
    /// Test environment, maximum logging, memory stores etc.
    Test,

    /// Production environment, minimum logging, maximum performances,
    /// persistent stores etc.
    Production,
}

impl FromStr for ExecutionEnvironment {
    type Err = ConfigError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "production" => Ok(Self::Production),
            "test" => Ok(Self::Test),
            _ => Err(ConfigError::Message(format!(
                "Unknown execution environment {s}"
            ))),
        }
    }
}

/// Aggregator configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Configuration {
    /// What kind of runtime environment the configuration is meant to.
    pub environment: ExecutionEnvironment,

    /// Cardano CLI tool path
    pub cardano_cli_path: PathBuf,

    /// Path of the socket used by the Cardano CLI tool
    /// to communicate with the Cardano node
    pub cardano_node_socket_path: PathBuf,

    /// Cardano node version.
    ///
    /// **NOTE**: This cannot be verified for now (see [this
    /// issue](https://github.com/input-output-hk/cardano-cli/issues/224)). This
    /// is why it has to be manually given to the Aggregator
    pub cardano_node_version: String,

    /// Cardano Network Magic number
    ///
    /// useful for TestNet & DevNet
    pub network_magic: Option<u64>,

    /// Cardano network
    pub network: String,

    /// Protocol parameters
    pub protocol_parameters: ProtocolParameters,

    /// Type of snapshot uploader to use
    pub snapshot_uploader_type: SnapshotUploaderType,

    /// Bucket name where the snapshots are stored if snapshot_uploader_type is Gcp
    pub snapshot_bucket_name: Option<String>,

    /// Use CDN domain to construct snapshot urls if snapshot_uploader_type is Gcp
    pub snapshot_use_cdn_domain: bool,

    /// Server listening IP
    pub server_ip: String,

    /// Server listening port
    pub server_port: u16,

    /// Run Interval is the interval between two runtime cycles in ms
    pub run_interval: u64,

    /// Directory of the Cardano node store.
    pub db_directory: PathBuf,

    /// Directory to store snapshot
    pub snapshot_directory: PathBuf,

    /// Directory to store aggregator data (Certificates, Snapshots, Protocol Parameters, ...)
    pub data_stores_directory: PathBuf,

    /// Genesis verification key
    pub genesis_verification_key: HexEncodedGenesisVerificationKey,

    /// Should the immutable cache be reset or not
    pub reset_digests_cache: bool,

    /// Use the digest caching strategy
    pub disable_digests_cache: bool,

    /// Max number of records in stores.
    /// When new records are added, oldest records are automatically deleted so
    /// there can always be at max the number of records specified by this
    /// setting.
    pub store_retention_limit: Option<usize>,

    /// Era reader adapter type
    pub era_reader_adapter_type: EraReaderAdapterType,

    /// Era reader adapter parameters
    pub era_reader_adapter_params: Option<String>,

    /// Compression algorithm used for the snapshot archive artifacts.
    pub snapshot_compression_algorithm: CompressionAlgorithm,

    /// Specific parameters when [snapshot_compression_algorithm][Self::snapshot_compression_algorithm]
    /// is set to [zstandard][CompressionAlgorithm::Zstandard].
    pub zstandard_parameters: Option<ZstandardCompressionParameters>,
}

/// Uploader needed to copy the snapshot once computed.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum SnapshotUploaderType {
    /// Uploader to GCP storage.
    Gcp,
    /// Uploader to local storage.
    Local,
}

/// [Zstandard][CompressionAlgorithm::Zstandard] specific parameters
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub struct ZstandardCompressionParameters {
    /// Level of compression, default to 9.
    pub level: i32,

    /// Number of workers when compressing, 0 will disable multithreading, default to 4.
    pub number_of_workers: u32,
}

impl Default for ZstandardCompressionParameters {
    fn default() -> Self {
        Self {
            level: 9,
            number_of_workers: 4,
        }
    }
}

impl Configuration {
    /// Create a sample configuration mainly for tests
    pub fn new_sample() -> Self {
        let genesis_verification_key = ProtocolGenesisSigner::create_deterministic_genesis_signer()
            .create_genesis_verifier()
            .to_verification_key();

        Self {
            environment: ExecutionEnvironment::Test,
            cardano_cli_path: PathBuf::new(),
            cardano_node_socket_path: PathBuf::new(),
            cardano_node_version: "0.0.1".to_string(),
            network_magic: Some(42),
            network: "devnet".to_string(),
            protocol_parameters: ProtocolParameters {
                k: 5,
                m: 100,
                phi_f: 0.95,
            },
            snapshot_uploader_type: SnapshotUploaderType::Local,
            snapshot_bucket_name: None,
            snapshot_use_cdn_domain: false,
            server_ip: "0.0.0.0".to_string(),
            server_port: 8000,
            run_interval: 5000,
            db_directory: PathBuf::new(),
            snapshot_directory: PathBuf::new(),
            data_stores_directory: PathBuf::from(":memory:"),
            genesis_verification_key: genesis_verification_key.to_json_hex().unwrap(),
            reset_digests_cache: false,
            disable_digests_cache: false,
            store_retention_limit: None,
            era_reader_adapter_type: EraReaderAdapterType::Bootstrap,
            era_reader_adapter_params: None,
            snapshot_compression_algorithm: CompressionAlgorithm::Zstandard,
            zstandard_parameters: Some(ZstandardCompressionParameters::default()),
        }
    }

    /// Build the server URL from configuration.
    pub fn get_server_url(&self) -> String {
        format!("http://{}:{}/", self.server_ip, self.server_port)
    }

    /// Check configuration and return a representation of the Cardano network.
    pub fn get_network(&self) -> StdResult<CardanoNetwork> {
        CardanoNetwork::from_code(self.network.clone(), self.network_magic)
            .map_err(|e| anyhow!(ConfigError::Message(e.to_string())))
    }

    /// Return the file of the SQLite stores. If the directory does not exist, it is created.
    pub fn get_sqlite_dir(&self) -> PathBuf {
        let store_dir = &self.data_stores_directory;

        if !store_dir.exists() {
            std::fs::create_dir_all(store_dir).unwrap();
        }

        self.data_stores_directory.clone()
    }

    /// Same as the [store retention limit][Configuration::store_retention_limit] but will never
    /// yield a value lower than 3.
    ///
    /// This is in order to avoid pruning data that will be used in future epochs (like the protocol
    /// parameters).
    pub fn safe_epoch_retention_limit(&self) -> Option<u64> {
        self.store_retention_limit
            .map(|limit| if limit > 3 { limit as u64 } else { 3 })
    }
}

/// Default configuration with all the default values for configurations.
#[derive(Debug, Clone)]
pub struct DefaultConfiguration {
    /// Execution environment
    pub environment: ExecutionEnvironment,

    /// Server listening IP
    pub server_ip: String,

    /// Server listening port
    pub server_port: String,

    /// Directory of the Cardano node database
    pub db_directory: String,

    /// Directory to store snapshot
    pub snapshot_directory: String,

    /// Type of snapshot store to use
    pub snapshot_store_type: String,

    /// Type of snapshot uploader to use
    pub snapshot_uploader_type: String,

    /// Era reader adapter type
    pub era_reader_adapter_type: String,

    /// ImmutableDigesterCacheProvider default setting
    pub reset_digests_cache: String,

    /// ImmutableDigesterCacheProvider default setting
    pub disable_digests_cache: String,

    /// Snapshot compression algorithm default setting
    pub snapshot_compression_algorithm: String,

    /// Use CDN domain to construct snapshot urls default setting (if snapshot_uploader_type is Gcp)
    pub snapshot_use_cdn_domain: String,
}

impl Default for DefaultConfiguration {
    fn default() -> Self {
        Self {
            environment: ExecutionEnvironment::Production,
            server_ip: "0.0.0.0".to_string(),
            server_port: "8080".to_string(),
            db_directory: "/db".to_string(),
            snapshot_directory: ".".to_string(),
            snapshot_store_type: "local".to_string(),
            snapshot_uploader_type: "gcp".to_string(),
            era_reader_adapter_type: "bootstrap".to_string(),
            reset_digests_cache: "false".to_string(),
            disable_digests_cache: "false".to_string(),
            snapshot_compression_algorithm: "zstandard".to_string(),
            snapshot_use_cdn_domain: "false".to_string(),
        }
    }
}

impl From<ExecutionEnvironment> for ValueKind {
    fn from(value: ExecutionEnvironment) -> Self {
        match value {
            ExecutionEnvironment::Production => ValueKind::String("Production".to_string()),
            ExecutionEnvironment::Test => ValueKind::String("Test".to_string()),
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
            "environment".to_string(),
            Value::new(Some(&namespace), ValueKind::from(myself.environment)),
        );
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
        result.insert(
            "era_reader_adapter_type".to_string(),
            Value::new(
                Some(&namespace),
                ValueKind::from(myself.era_reader_adapter_type),
            ),
        );
        result.insert(
            "reset_digests_cache".to_string(),
            Value::new(
                Some(&namespace),
                ValueKind::from(myself.reset_digests_cache),
            ),
        );
        result.insert(
            "disable_digests_cache".to_string(),
            Value::new(
                Some(&namespace),
                ValueKind::from(myself.disable_digests_cache),
            ),
        );
        result.insert(
            "snapshot_compression_algorithm".to_string(),
            Value::new(
                Some(&namespace),
                ValueKind::from(myself.snapshot_compression_algorithm),
            ),
        );
        result.insert(
            "snapshot_use_cdn_domain".to_string(),
            Value::new(
                Some(&namespace),
                ValueKind::from(myself.snapshot_use_cdn_domain),
            ),
        );

        Ok(result)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn safe_epoch_retention_limit_wont_change_a_value_higher_than_three() {
        for limit in 4..=10u64 {
            let configuration = Configuration {
                store_retention_limit: Some(limit as usize),
                ..Configuration::new_sample()
            };
            assert_eq!(configuration.safe_epoch_retention_limit(), Some(limit));
        }
    }

    #[test]
    fn safe_epoch_retention_limit_wont_change_a_none_value() {
        let configuration = Configuration {
            store_retention_limit: None,
            ..Configuration::new_sample()
        };
        assert_eq!(configuration.safe_epoch_retention_limit(), None);
    }

    #[test]
    fn safe_epoch_retention_limit_wont_yield_a_value_lower_than_three() {
        for limit in 0..=3 {
            let configuration = Configuration {
                store_retention_limit: Some(limit),
                ..Configuration::new_sample()
            };
            assert_eq!(configuration.safe_epoch_retention_limit(), Some(3));
        }
    }
}
