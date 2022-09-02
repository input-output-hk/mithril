use config::ConfigError;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

use mithril_common::{entities::PartyId, CardanoNetwork};

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
    pub party_id: PartyId,

    /// Run Interval
    pub run_interval: u64,

    /// Directory to snapshot
    pub db_directory: PathBuf,

    /// Directory to store signer data (Stakes, Protocol initializers, ...)
    pub data_stores_directory: PathBuf,
}

impl Config {
    /// Return the CardanoNetwork value from the configuration.
    pub fn get_network(&self) -> Result<CardanoNetwork, ConfigError> {
        CardanoNetwork::from_code(self.network.clone(), self.network_magic)
            .map_err(|e| ConfigError::Message(e.to_string()))
    }
}
