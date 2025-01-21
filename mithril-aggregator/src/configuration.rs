use anyhow::{anyhow, Context};
use config::{ConfigError, Map, Source, Value, ValueKind};
use mithril_common::chain_observer::ChainObserverType;
use mithril_common::crypto_helper::ProtocolGenesisSigner;
use mithril_common::era::adapters::EraReaderAdapterType;
use mithril_doc::{Documenter, DocumenterDefault, StructDoc};
use reqwest::Url;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeSet, HashMap};
use std::path::PathBuf;
use std::str::FromStr;

use mithril_common::entities::{
    BlockNumber, CardanoTransactionsSigningConfig, CompressionAlgorithm,
    HexEncodedGenesisVerificationKey, ProtocolParameters, SignedEntityConfig,
    SignedEntityTypeDiscriminants,
};
use mithril_common::{CardanoNetwork, StdResult};

use crate::tools::url_sanitizer;

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
#[derive(Debug, Clone, Serialize, Deserialize, Documenter)]
pub struct Configuration {
    /// What kind of runtime environment the configuration is meant to.
    pub environment: ExecutionEnvironment,

    /// Cardano CLI tool path
    #[example = "`cardano-cli`"]
    pub cardano_cli_path: PathBuf,

    /// Path of the socket used by the Cardano CLI tool
    /// to communicate with the Cardano node
    #[example = "`/tmp/cardano.sock`"]
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
    #[example = "`1097911063` or `42`"]
    pub network_magic: Option<u64>,

    /// Cardano network
    #[example = "`testnet` or `mainnet` or `devnet`"]
    pub network: String,

    /// Cardano chain observer type
    pub chain_observer_type: ChainObserverType,

    /// Protocol parameters
    #[example = "`{ k: 5, m: 100, phi_f: 0.65 }`"]
    pub protocol_parameters: ProtocolParameters,

    /// Type of snapshot uploader to use
    #[example = "`gcp` or `local`"]
    pub snapshot_uploader_type: SnapshotUploaderType,

    /// Bucket name where the snapshots are stored if snapshot_uploader_type is Gcp
    pub snapshot_bucket_name: Option<String>,

    /// Use CDN domain to construct snapshot urls if snapshot_uploader_type is Gcp
    pub snapshot_use_cdn_domain: bool,

    /// Server listening IP
    pub server_ip: String,

    /// Server listening port
    pub server_port: u16,

    /// Server URL that can be accessed from the outside
    pub public_server_url: Option<String>,

    /// Run Interval is the interval between two runtime cycles in ms
    #[example = "`60000`"]
    pub run_interval: u64,

    /// Directory of the Cardano node store.
    pub db_directory: PathBuf,

    /// Directory to store snapshot
    pub snapshot_directory: PathBuf,

    /// Directory to store aggregator data (Certificates, Snapshots, Protocol Parameters, ...)
    #[example = "`./mithril-aggregator/stores`"]
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

    /// Signed entity types parameters (discriminants names in an ordered, case-sensitive, comma
    /// separated list).
    ///
    /// The values `MithrilStakeDistribution` and `CardanoImmutableFilesFull` are prepended
    /// automatically to the list.
    #[example = "`MithrilStakeDistribution,CardanoImmutableFilesFull,CardanoStakeDistribution`"]
    pub signed_entity_types: Option<String>,

    /// Compression algorithm used for the snapshot archive artifacts.
    #[example = "`gzip` or `zstandard`"]
    pub snapshot_compression_algorithm: CompressionAlgorithm,

    /// Specific parameters when [snapshot_compression_algorithm][Self::snapshot_compression_algorithm]
    /// is set to [zstandard][CompressionAlgorithm::Zstandard].
    #[example = "`{ level: 9, number_of_workers: 4 }`"]
    pub zstandard_parameters: Option<ZstandardCompressionParameters>,

    /// Url to CExplorer list of pools to import as signer in the database.
    pub cexplorer_pools_url: Option<String>,

    /// Time interval at which the signers in [Self::cexplorer_pools_url] will be imported (in minutes).
    pub signer_importer_run_interval: u64,

    /// If set no error is returned in case of unparsable block and an error log is written instead.
    ///
    /// Will be ignored on (pre)production networks.
    pub allow_unparsable_block: bool,

    /// Cardano transactions prover cache pool size
    pub cardano_transactions_prover_cache_pool_size: usize,

    /// Cardano transactions database connection pool size
    pub cardano_transactions_database_connection_pool_size: usize,

    /// Cardano transactions signing configuration
    #[example = "`{ security_parameter: 3000, step: 120 }`"]
    pub cardano_transactions_signing_config: CardanoTransactionsSigningConfig,

    /// Maximum number of transactions hashes allowed by request to the prover of the Cardano transactions
    pub cardano_transactions_prover_max_hashes_allowed_by_request: usize,

    /// The maximum number of roll forwards during a poll of the block streamer when importing transactions.
    pub cardano_transactions_block_streamer_max_roll_forwards_per_poll: usize,

    /// Enable metrics server (Prometheus endpoint on /metrics).
    pub enable_metrics_server: bool,

    /// Metrics HTTP Server IP.
    pub metrics_server_ip: String,

    /// Metrics HTTP Server listening port.
    pub metrics_server_port: u16,

    /// Time interval at which usage metrics are persisted in event database (in seconds).
    pub persist_usage_report_interval_in_seconds: u64,
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
            chain_observer_type: ChainObserverType::Fake,
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
            public_server_url: None,
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
            signed_entity_types: None,
            snapshot_compression_algorithm: CompressionAlgorithm::Zstandard,
            zstandard_parameters: Some(ZstandardCompressionParameters::default()),
            cexplorer_pools_url: None,
            signer_importer_run_interval: 1,
            allow_unparsable_block: false,
            cardano_transactions_prover_cache_pool_size: 3,
            cardano_transactions_database_connection_pool_size: 5,
            cardano_transactions_signing_config: CardanoTransactionsSigningConfig {
                security_parameter: BlockNumber(120),
                step: BlockNumber(15),
            },
            cardano_transactions_prover_max_hashes_allowed_by_request: 100,
            cardano_transactions_block_streamer_max_roll_forwards_per_poll: 1000,
            enable_metrics_server: true,
            metrics_server_ip: "0.0.0.0".to_string(),
            metrics_server_port: 9090,
            persist_usage_report_interval_in_seconds: 10,
        }
    }

    /// Build the local server URL from configuration.
    pub fn get_local_server_url(&self) -> StdResult<Url> {
        let url = Url::parse(&format!("http://{}:{}/", self.server_ip, self.server_port))?;
        Ok(url)
    }

    /// Get the server URL from the configuration.
    ///
    /// Will return the public server URL if it is set, otherwise the local server URL.
    pub fn get_server_url(&self) -> StdResult<Url> {
        match &self.public_server_url {
            Some(url) => Ok(url_sanitizer::sanitize_url_path(&Url::parse(url)?)?),
            None => self.get_local_server_url(),
        }
    }

    /// Check configuration and return a representation of the Cardano network.
    pub fn get_network(&self) -> StdResult<CardanoNetwork> {
        CardanoNetwork::from_code(self.network.clone(), self.network_magic)
            .map_err(|e| anyhow!(ConfigError::Message(e.to_string())))
    }

    /// Return the directory of the SQLite stores. If the directory does not exist, it is created.
    pub fn get_sqlite_dir(&self) -> PathBuf {
        let store_dir = &self.data_stores_directory;

        if !store_dir.exists() {
            std::fs::create_dir_all(store_dir).unwrap();
        }

        self.data_stores_directory.clone()
    }

    /// Return the snapshots directory.
    pub fn get_snapshot_dir(&self) -> StdResult<PathBuf> {
        if !&self.snapshot_directory.exists() {
            std::fs::create_dir_all(&self.snapshot_directory)?;
        }

        Ok(self.snapshot_directory.clone())
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

    /// Compute the list of signed entity discriminants that are allowed to be processed based on this configuration.
    pub fn compute_allowed_signed_entity_types_discriminants(
        &self,
    ) -> StdResult<BTreeSet<SignedEntityTypeDiscriminants>> {
        let allowed_discriminants = self
            .signed_entity_types
            .as_ref()
            .map(SignedEntityTypeDiscriminants::parse_list)
            .transpose()
            .with_context(|| "Invalid 'signed_entity_types' configuration")?
            .unwrap_or_default();
        let allowed_discriminants =
            SignedEntityConfig::append_allowed_signed_entity_types_discriminants(
                allowed_discriminants,
            );

        Ok(allowed_discriminants)
    }

    /// Check if the HTTP server can serve static directories.
    // TODO: This function should be completed when the configuration of the uploaders for the Cardano database is done.
    pub fn allow_http_serve_directory(&self) -> bool {
        match self.snapshot_uploader_type {
            SnapshotUploaderType::Local => true,
            SnapshotUploaderType::Gcp => false,
        }
    }
}

/// Default configuration with all the default values for configurations.
#[derive(Debug, Clone, DocumenterDefault)]
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
    #[example = "`gcp` or `local`"]
    pub snapshot_store_type: String,

    /// Type of snapshot uploader to use
    pub snapshot_uploader_type: String,

    /// Era reader adapter type
    pub era_reader_adapter_type: String,

    /// Chain observer type
    pub chain_observer_type: String,

    /// ImmutableDigesterCacheProvider default setting
    pub reset_digests_cache: String,

    /// ImmutableDigesterCacheProvider default setting
    pub disable_digests_cache: String,

    /// Snapshot compression algorithm default setting
    pub snapshot_compression_algorithm: String,

    /// Use CDN domain to construct snapshot urls default setting (if snapshot_uploader_type is Gcp)
    pub snapshot_use_cdn_domain: String,

    /// Signer importer run interval default setting
    pub signer_importer_run_interval: u64,

    /// If set no error is returned in case of unparsable block and an error log is written instead.
    ///
    /// Will be ignored on (pre)production networks.
    pub allow_unparsable_block: String,

    /// Cardano transactions prover cache pool size
    pub cardano_transactions_prover_cache_pool_size: u32,

    /// Cardano transactions database connection pool size
    pub cardano_transactions_database_connection_pool_size: u32,

    /// Cardano transactions signing configuration
    pub cardano_transactions_signing_config: CardanoTransactionsSigningConfig,

    /// Maximum number of transactions hashes allowed by request to the prover of the Cardano transactions
    pub cardano_transactions_prover_max_hashes_allowed_by_request: u32,

    /// The maximum number of roll forwards during a poll of the block streamer when importing transactions.
    pub cardano_transactions_block_streamer_max_roll_forwards_per_poll: u32,

    /// Enable metrics server (Prometheus endpoint on /metrics).
    pub enable_metrics_server: String,

    /// Metrics HTTP server IP.
    pub metrics_server_ip: String,

    /// Metrics HTTP server listening port.
    pub metrics_server_port: u16,

    /// Time interval at which metrics are persisted in event database (in seconds).
    pub persist_usage_report_interval_in_seconds: u64,
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
            chain_observer_type: "pallas".to_string(),
            reset_digests_cache: "false".to_string(),
            disable_digests_cache: "false".to_string(),
            snapshot_compression_algorithm: "zstandard".to_string(),
            snapshot_use_cdn_domain: "false".to_string(),
            signer_importer_run_interval: 720,
            allow_unparsable_block: "false".to_string(),
            cardano_transactions_prover_cache_pool_size: 10,
            cardano_transactions_database_connection_pool_size: 10,
            cardano_transactions_signing_config: CardanoTransactionsSigningConfig {
                security_parameter: BlockNumber(3000),
                step: BlockNumber(120),
            },
            cardano_transactions_prover_max_hashes_allowed_by_request: 100,
            cardano_transactions_block_streamer_max_roll_forwards_per_poll: 10000,
            enable_metrics_server: "false".to_string(),
            metrics_server_ip: "0.0.0.0".to_string(),
            metrics_server_port: 9090,
            persist_usage_report_interval_in_seconds: 10,
        }
    }
}

impl DefaultConfiguration {
    fn namespace() -> String {
        "default configuration".to_string()
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

    fn collect(&self) -> Result<Map<String, Value>, ConfigError> {
        macro_rules! insert_default_configuration {
            ( $map:ident, $config:ident.$parameter:ident ) => {{
                $map.insert(
                    stringify!($parameter).to_string(),
                    into_value($config.$parameter),
                );
            }};
        }

        fn into_value<V: Into<ValueKind>>(value: V) -> Value {
            Value::new(Some(&DefaultConfiguration::namespace()), value.into())
        }
        let mut result = Map::new();
        let myself = self.clone();
        insert_default_configuration!(result, myself.environment);
        insert_default_configuration!(result, myself.server_ip);
        insert_default_configuration!(result, myself.server_port);
        insert_default_configuration!(result, myself.db_directory);
        insert_default_configuration!(result, myself.snapshot_directory);
        insert_default_configuration!(result, myself.snapshot_store_type);
        insert_default_configuration!(result, myself.snapshot_uploader_type);
        insert_default_configuration!(result, myself.era_reader_adapter_type);
        insert_default_configuration!(result, myself.reset_digests_cache);
        insert_default_configuration!(result, myself.disable_digests_cache);
        insert_default_configuration!(result, myself.snapshot_compression_algorithm);
        insert_default_configuration!(result, myself.snapshot_use_cdn_domain);
        insert_default_configuration!(result, myself.signer_importer_run_interval);
        insert_default_configuration!(result, myself.allow_unparsable_block);
        insert_default_configuration!(result, myself.cardano_transactions_prover_cache_pool_size);
        insert_default_configuration!(
            result,
            myself.cardano_transactions_database_connection_pool_size
        );
        insert_default_configuration!(
            result,
            myself.cardano_transactions_prover_max_hashes_allowed_by_request
        );
        insert_default_configuration!(
            result,
            myself.cardano_transactions_block_streamer_max_roll_forwards_per_poll
        );
        insert_default_configuration!(result, myself.enable_metrics_server);
        insert_default_configuration!(result, myself.metrics_server_ip);
        insert_default_configuration!(result, myself.metrics_server_port);
        insert_default_configuration!(result, myself.persist_usage_report_interval_in_seconds);
        result.insert(
            "cardano_transactions_signing_config".to_string(),
            into_value(HashMap::from([
                (
                    "security_parameter".to_string(),
                    ValueKind::from(
                        *myself
                            .cardano_transactions_signing_config
                            .security_parameter,
                    ),
                ),
                (
                    "step".to_string(),
                    ValueKind::from(*myself.cardano_transactions_signing_config.step),
                ),
            ])),
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

    #[test]
    fn can_build_config_with_ctx_signing_config_from_default_configuration() {
        #[derive(Debug, Deserialize)]
        struct TargetConfig {
            cardano_transactions_signing_config: CardanoTransactionsSigningConfig,
        }

        let config_builder = config::Config::builder().add_source(DefaultConfiguration::default());
        let target: TargetConfig = config_builder.build().unwrap().try_deserialize().unwrap();

        assert_eq!(
            target.cardano_transactions_signing_config,
            DefaultConfiguration::default().cardano_transactions_signing_config
        );
    }

    #[test]
    fn compute_allowed_signed_entity_types_discriminants_append_default_discriminants() {
        let config = Configuration {
            signed_entity_types: None,
            ..Configuration::new_sample()
        };

        assert_eq!(
            config
                .compute_allowed_signed_entity_types_discriminants()
                .unwrap(),
            BTreeSet::from(SignedEntityConfig::DEFAULT_ALLOWED_DISCRIMINANTS)
        );
    }

    #[test]
    fn allow_http_serve_directory() {
        let config = Configuration {
            snapshot_uploader_type: SnapshotUploaderType::Local,
            ..Configuration::new_sample()
        };

        assert!(config.allow_http_serve_directory());

        let config = Configuration {
            snapshot_uploader_type: SnapshotUploaderType::Gcp,
            ..Configuration::new_sample()
        };

        assert!(!config.allow_http_serve_directory());
    }

    #[test]
    fn get_server_url_return_local_url_if_public_url_is_not_set() {
        let config = Configuration {
            server_ip: "1.2.3.4".to_string(),
            server_port: 5678,
            public_server_url: None,
            ..Configuration::new_sample()
        };

        assert_eq!(
            config.get_server_url().unwrap().as_str(),
            "http://1.2.3.4:5678/"
        );
    }

    #[test]
    fn get_server_url_return_sanitized_public_url_if_it_is_set() {
        let config = Configuration {
            server_ip: "1.2.3.4".to_string(),
            server_port: 5678,
            public_server_url: Some("https://example.com".to_string()),
            ..Configuration::new_sample()
        };

        assert_eq!(
            config.get_server_url().unwrap().as_str(),
            "https://example.com/"
        );
    }
}
