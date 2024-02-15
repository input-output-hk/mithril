use anyhow::Context;
use config::{ConfigError, Map, Source, Value, ValueKind};
use mithril_doc::{DocExtractor, DocExtractorDefault, StructDoc};
use mithril_doc_derive::{DocExtractor, DocExtractorDefault};
use serde::{Deserialize, Serialize};
use std::{path::PathBuf, sync::Arc};

use mithril_common::{
    chain_observer::ChainObserver,
    crypto_helper::tests_setup,
    entities::PartyId,
    era::{
        adapters::{EraReaderAdapterBuilder, EraReaderAdapterType},
        EraReaderAdapter,
    },
    CardanoNetwork, StdResult,
};

/// Client configuration
#[derive(Debug, Clone, Serialize, Deserialize, DocExtractor)]
pub struct Configuration {
    /// Cardano CLI tool path
    #[example = "`cardano-cli`"]
    pub cardano_cli_path: PathBuf,

    /// Path of the socket used by the Cardano CLI tool
    /// to communicate with the Cardano node
    #[example = "`/tmp/cardano.sock`"]
    pub cardano_node_socket_path: PathBuf,

    /// Cardano Network Magic number
    /// useful for TestNet & DevNet
    #[example = "`1097911063` or `42`"]
    pub network_magic: Option<u64>,

    /// Cardano network
    #[example = "`testnet` or `mainnet` or `devnet`"]
    pub network: String,

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
}

impl Configuration {
    /// Create a sample configuration mainly for tests
    #[doc(hidden)]
    pub fn new_sample(party_id: &PartyId) -> Self {
        let signer_temp_dir = tests_setup::setup_temp_directory_for_signer(party_id, false);
        Self {
            aggregator_endpoint: "http://0.0.0.0:8000".to_string(),
            relay_endpoint: None,
            cardano_cli_path: PathBuf::new(),
            cardano_node_socket_path: PathBuf::new(),
            db_directory: PathBuf::new(),
            network: "devnet".to_string(),
            network_magic: Some(42),
            party_id: Some(party_id.to_owned()),
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
#[derive(Debug, Clone, DocExtractorDefault)]
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
