use config::ConfigError;
use serde::{Deserialize, Serialize};
use std::{error::Error, path::PathBuf, sync::Arc};

use mithril_common::{
    chain_observer::{ChainAddress, ChainObserver},
    crypto_helper::EraMarkersVerifierVerificationKey,
    entities::PartyId,
    era::{
        adapters::{
            EraReaderBootstrapAdapter, EraReaderCardanoChainAdapter, EraReaderDummyAdapter,
        },
        EraMarker, EraReaderAdapter,
    },
    CardanoNetwork,
};

const SQLITE_FILE: &str = "signer.sqlite3";

/// Client configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
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

/// Era reader adapter type used to retrieve the era activation markers.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum EraReaderAdapterType {
    /// Cardano chain adapter.
    CardanoChain,
    /// Dummy adapter.
    Dummy,
    /// Bootstrap adapter.
    Bootstrap,
}

impl Config {
    /// Return the CardanoNetwork value from the configuration.
    pub fn get_network(&self) -> Result<CardanoNetwork, ConfigError> {
        CardanoNetwork::from_code(self.network.clone(), self.network_magic)
            .map_err(|e| ConfigError::Message(e.to_string()))
    }

    /// Create the SQL store directory if not exist and return the path of the
    /// SQLite3 file.
    pub fn get_sqlite_file(&self) -> PathBuf {
        let store_dir = &self.data_stores_directory;

        if !store_dir.exists() {
            std::fs::create_dir_all(store_dir).unwrap();
        }

        self.data_stores_directory.join(SQLITE_FILE)
    }

    /// Create era reader adapter from configuration settings.
    pub fn build_era_reader_adapter(
        &self,
        chain_observer: Arc<dyn ChainObserver>,
    ) -> Result<Box<dyn EraReaderAdapter>, Box<dyn Error>> {
        match self.era_reader_adapter_type {
            EraReaderAdapterType::CardanoChain => {
                #[derive(Deserialize)]
                struct EraReaderCardanoChainAdapterConfig {
                    address: ChainAddress,
                    verification_key: EraMarkersVerifierVerificationKey,
                }
                let adapter_config: EraReaderCardanoChainAdapterConfig = serde_json::from_str(
                    self.era_reader_adapter_params.as_ref().ok_or_else(|| {
                        ConfigError::Message(
                            "missing era adapter configuration parameters".to_string(),
                        )
                    })?,
                )?;
                Ok(Box::new(EraReaderCardanoChainAdapter::new(
                    adapter_config.address,
                    chain_observer,
                    adapter_config.verification_key,
                )))
            }
            EraReaderAdapterType::Dummy => {
                #[derive(Deserialize)]
                struct EraReaderDummyAdapterConfig {
                    markers: Vec<EraMarker>,
                }
                let adapter_config: EraReaderDummyAdapterConfig = serde_json::from_str(
                    self.era_reader_adapter_params.as_ref().ok_or_else(|| {
                        ConfigError::Message(
                            "missing era adapter configuration parameters".to_string(),
                        )
                    })?,
                )?;
                let mut dummy_adapter = EraReaderDummyAdapter::default();
                dummy_adapter.set_markers(adapter_config.markers);
                Ok(Box::new(dummy_adapter))
            }
            EraReaderAdapterType::Bootstrap => Ok(Box::new(EraReaderBootstrapAdapter)),
        }
    }
}
