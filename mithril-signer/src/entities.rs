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

    /// Directory to store signer data (stakes, protocol initializers, ...)
    pub data_stores_directory: PathBuf,
}

impl Config {
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
