use anyhow::Context;
use config::{ConfigError, Map, Source, Value, ValueKind};
use mithril_doc::{Documenter, DocumenterDefault, StructDoc};
use serde::{Deserialize, Serialize};
use std::{path::PathBuf, sync::Arc};

use mithril_common::{
    chain_observer::ChainObserver,
    crypto_helper::tests_setup,
    entities::{BlockNumber, PartyId},
    era::{
        adapters::{EraReaderAdapterBuilder, EraReaderAdapterType},
        EraReaderAdapter,
    },
    CardanoNetwork, StdResult,
};

/// Client configuration
#[derive(Debug, Clone, Serialize, Deserialize, Documenter)]
pub struct Configuration {
    /// Cardano CLI tool path
    #[example = "`cardano-cli`"]
    pub cardano_cli_path: PathBuf,

    /// Path of the socket used by the Cardano CLI tool
    /// to communicate with the Cardano node
    #[example = "`/tmp/cardano.sock`"]
    pub cardano_node_socket_path: PathBuf,

    /// Cardano network
    #[example = "`testnet` or `mainnet` or `devnet`"]
    pub network: String,

    /// Cardano Network Magic number
    /// useful for TestNet & DevNet
    #[example = "`1097911063` or `42`"]
    pub network_magic: Option<u64>,

    /// Also known as `k`, it defines the number of blocks that are required for the blockchain to
    /// be considered final, preventing any further rollback `[default: 2160]`.
    pub network_security_parameter: BlockNumber,

    /// Aggregator endpoint
    #[example = "`https://aggregator.pre-release-preview.api.mithril.network/aggregator`"]
    pub aggregator_endpoint: String,

    /// Relay endpoint
    pub relay_endpoint: Option<String>,

    /// Party Id
    // TODO: Field should be removed once the signer certification is fully deployed
    #[example = "`pool1pxaqe80sqpde7902er5kf6v0c7y0sv6d5g676766v2h829fvs3x`"]
    pub party_id: Option<PartyId>,

    /// Run Interval
    #[example = "`60000`"]
    pub run_interval: u64,

    /// Directory to snapshot
    pub db_directory: PathBuf,

    /// Directory to store signer data (Stakes, Protocol initializers, ...)
    #[example = "`./mithril-signer/stores`"]
    pub data_stores_directory: PathBuf,

    /// Store retention limit. If set to None, no limit will be set.
    pub store_retention_limit: Option<usize>,

    /// File path to the KES secret key of the pool
    pub kes_secret_key_path: Option<PathBuf>,

    /// File path to the operational certificate of the pool
    pub operational_certificate_path: Option<PathBuf>,

    /// Disable immutables digests cache.
    pub disable_digests_cache: bool,

    /// If set the existing immutables digests cache will be reset.
    ///
    /// Will be ignored if set in conjunction with `disable_digests_cache`.
    pub reset_digests_cache: bool,

    /// Era reader adapter type
    pub era_reader_adapter_type: EraReaderAdapterType,

    /// Era reader adapter parameters
    pub era_reader_adapter_params: Option<String>,

    /// Enable metrics server (Prometheus endpoint on /metrics).
    pub enable_metrics_server: bool,

    /// Metrics HTTP Server IP.
    pub metrics_server_ip: String,

    /// Metrics HTTP Server listening port.
    pub metrics_server_port: u16,

    /// If set no error is returned in case of unparsable block and an error log is written instead.
    ///
    /// Will be ignored on (pre)production networks.
    pub allow_unparsable_block: bool,

    /// If set, the signer will prune the cardano transactions in database older than the
    /// [network_security_parameter][Self::network_security_parameter] blocks after each import
    /// `[default: true]`.
    pub enable_transaction_pruning: bool,
}

impl Configuration {
    /// Create a sample configuration mainly for tests
    #[doc(hidden)]
    pub fn new_sample<P: Into<PartyId>>(party_id: P) -> Self {
        let party_id: PartyId = party_id.into();
        let signer_temp_dir = tests_setup::setup_temp_directory_for_signer(&party_id, false);
        Self {
            aggregator_endpoint: "http://0.0.0.0:8000".to_string(),
            relay_endpoint: None,
            cardano_cli_path: PathBuf::new(),
            cardano_node_socket_path: PathBuf::new(),
            db_directory: PathBuf::new(),
            network: "devnet".to_string(),
            network_magic: Some(42),
            network_security_parameter: 2160,
            party_id: Some(party_id),
            run_interval: 5000,
            data_stores_directory: PathBuf::new(),
            store_retention_limit: None,
            kes_secret_key_path: signer_temp_dir.as_ref().map(|dir| dir.join("kes.sk")),
            operational_certificate_path: signer_temp_dir
                .as_ref()
                .map(|dir| dir.join("opcert.cert")),
            disable_digests_cache: false,
            reset_digests_cache: false,
            era_reader_adapter_type: EraReaderAdapterType::Bootstrap,
            era_reader_adapter_params: None,
            enable_metrics_server: true,
            metrics_server_ip: "0.0.0.0".to_string(),
            metrics_server_port: 9090,
            allow_unparsable_block: false,
            enable_transaction_pruning: false,
        }
    }

    /// Return the CardanoNetwork value from the configuration.
    pub fn get_network(&self) -> StdResult<CardanoNetwork> {
        CardanoNetwork::from_code(self.network.clone(), self.network_magic).with_context(|| {
            format!(
                "Could not read Network '{}' from configuration.",
                &self.network
            )
        })
    }

    /// Create the SQL store directory if not exist and return the path of the
    /// SQLite3 file.
    pub fn get_sqlite_file(&self, sqlite_file_name: &str) -> StdResult<PathBuf> {
        let store_dir = &self.data_stores_directory;

        if !store_dir.exists() {
            std::fs::create_dir_all(store_dir).with_context(|| {
                format!(
                    "Could not create directory '{}' for Sqlite3 file.",
                    store_dir.display()
                )
            })?;
        }

        Ok(self.data_stores_directory.join(sqlite_file_name))
    }

    /// Create era reader adapter from configuration settings.
    pub fn build_era_reader_adapter(
        &self,
        chain_observer: Arc<dyn ChainObserver>,
    ) -> StdResult<Arc<dyn EraReaderAdapter>> {
        EraReaderAdapterBuilder::new(
            &self.era_reader_adapter_type,
            &self.era_reader_adapter_params,
        )
        .build(chain_observer)
        .with_context(|| {
            format!(
                "Configuration: can not create era reader for adapter '{}'.",
                &self.era_reader_adapter_type
            )
        })
    }
}

/// Default configuration with all the default values for configurations.
#[derive(Debug, Clone, DocumenterDefault)]
pub struct DefaultConfiguration {
    /// Era reader adapter type
    pub era_reader_adapter_type: String,

    /// Metrics HTTP server IP.
    pub metrics_server_ip: String,

    /// Metrics HTTP server listening port.
    pub metrics_server_port: u16,

    /// Network security parameter
    pub network_security_parameter: BlockNumber,

    /// Transaction pruning toggle
    pub enable_transaction_pruning: bool,
}

impl DefaultConfiguration {
    fn namespace() -> String {
        "default configuration".to_string()
    }
}

impl Default for DefaultConfiguration {
    fn default() -> Self {
        Self {
            era_reader_adapter_type: "bootstrap".to_string(),
            metrics_server_ip: "0.0.0.0".to_string(),
            metrics_server_port: 9090,
            network_security_parameter: 2160, // 2160 is the mainnet value
            enable_transaction_pruning: true,
        }
    }
}

impl Source for DefaultConfiguration {
    fn clone_into_box(&self) -> Box<dyn Source + Send + Sync> {
        Box::new(self.clone())
    }

    fn collect(&self) -> Result<Map<String, Value>, ConfigError> {
        fn into_value<V: Into<ValueKind>>(value: V) -> Value {
            Value::new(Some(&DefaultConfiguration::namespace()), value.into())
        }
        let mut result = Map::new();
        let myself = self.clone();

        result.insert(
            "era_reader_adapter_type".to_string(),
            into_value(myself.era_reader_adapter_type),
        );

        result.insert(
            "metrics_server_ip".to_string(),
            into_value(myself.metrics_server_ip),
        );

        result.insert(
            "metrics_server_port".to_string(),
            into_value(myself.metrics_server_port),
        );

        result.insert(
            "network_security_parameter".to_string(),
            into_value(myself.network_security_parameter),
        );

        result.insert(
            "enable_transaction_pruning".to_string(),
            into_value(myself.enable_transaction_pruning),
        );

        Ok(result)
    }
}
