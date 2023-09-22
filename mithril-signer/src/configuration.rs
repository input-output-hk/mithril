use anyhow::Context;
use config::{ConfigError, Map, Source, Value, ValueKind};
use serde::{Deserialize, Serialize};
use std::{path::PathBuf, sync::Arc};

use mithril_common::{
    chain_observer::ChainObserver,
    entities::PartyId,
    era::{
        adapters::{EraReaderAdapterBuilder, EraReaderAdapterType},
        EraReaderAdapter,
    },
    CardanoNetwork, StdResult,
};

const SQLITE_FILE: &str = "signer.sqlite3";

/// Client configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Configuration {
    /// Cardano CLI tool path
    pub cardano_cli_path: PathBuf,

    /// Path of the socket used by the Cardano CLI tool
    /// to communicate with the Cardano node
    pub cardano_node_socket_path: PathBuf,

    /// Cardano Network Magic number
    /// useful for TestNet & DevNet
    pub network_magic: Option<u64>,

    /// Cardano network
    pub network: String,

    /// Aggregator endpoint
    pub aggregator_endpoint: String,

    /// Relay endpoint
    pub relay_endpoint: Option<String>,

    /// Party Id
    // TODO: Field should be removed once the signer certification is fully deployed
    pub party_id: Option<PartyId>,

    /// Run Interval
    pub run_interval: u64,

    /// Directory to snapshot
    pub db_directory: PathBuf,

    /// Directory to store signer data (Stakes, Protocol initializers, ...)
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
}

impl Configuration {
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
    pub fn get_sqlite_file(&self) -> StdResult<PathBuf> {
        let store_dir = &self.data_stores_directory;

        if !store_dir.exists() {
            std::fs::create_dir_all(store_dir).with_context(|| {
                format!(
                    "Could not create directory '{}' for Sqlite3 file.",
                    store_dir.display()
                )
            })?;
        }

        Ok(self.data_stores_directory.join(SQLITE_FILE))
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
#[derive(Debug, Clone)]
pub struct DefaultConfiguration {
    /// Era reader adapter type
    pub era_reader_adapter_type: String,
}

impl Default for DefaultConfiguration {
    fn default() -> Self {
        Self {
            era_reader_adapter_type: "bootstrap".to_string(),
        }
    }
}

impl Source for DefaultConfiguration {
    fn clone_into_box(&self) -> Box<dyn Source + Send + Sync> {
        Box::new(self.clone())
    }

    fn collect(&self) -> Result<Map<String, Value>, ConfigError> {
        let mut result = Map::new();
        let namespace = "default configuration".to_string();
        let myself = self.clone();

        result.insert(
            "era_reader_adapter_type".to_string(),
            Value::new(
                Some(&namespace),
                ValueKind::from(myself.era_reader_adapter_type),
            ),
        );

        Ok(result)
    }
}
