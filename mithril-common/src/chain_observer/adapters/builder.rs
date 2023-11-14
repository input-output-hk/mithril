use serde::{Deserialize, Serialize};
use std::{fmt::Display, path::PathBuf, sync::Arc};

use thiserror::Error;

use crate::{
    chain_observer::{CardanoCliChainObserver, CardanoCliRunner, ChainObserver, PallasObserver},
    CardanoNetwork, StdError, StdResult,
};

/// `AdapterType` is an enumeration that identifies the type of an adapter.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum AdapterType {
    /// `CardanoCliChainObserver`: An adapter for the Cardano CLI chain observer.
    #[serde(rename = "cardano-cli-chain-observer")]
    CardanoCliChainObserver,

    /// * `PallasObserver`: An adapter for the Pallas observer.
    #[serde(rename = "pallas-observer")]
    PallasObserver,
}

impl std::str::FromStr for AdapterType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_str() {
            "cardano-cli-chain-observer" => Ok(Self::CardanoCliChainObserver),
            "pallas-observer" => Ok(Self::PallasObserver),
            _ => Err(format!(
                "'{}' is not valid value for ChainObserverAdapterType",
                s
            )),
        }
    }
}

impl Display for AdapterType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AdapterType::CardanoCliChainObserver => write!(f, "cardano-cli-chain-observer"),
            AdapterType::PallasObserver => write!(f, "pallas-observer"),
        }
    }
}

/// Error type for observer adapter builder service.
#[derive(Error, Debug)]
pub enum AdapterBuilderError {
    /// Missing parameters error.
    #[error("chain observer adapter parameters are missing")]
    MissingParameters(),

    /// Parameters parse error.
    #[error("chain observer adapter parameters parse error")]
    ParseParameters(#[source] serde_json::Error),

    /// Parameters decode error.
    #[error("chain observer adapter parameters decode error")]
    Decode(#[source] StdError),
}

/// `AdapterBuilder`. A toolkit for crafting adapters.
pub struct AdapterBuilder {
    adapter_type: AdapterType,
    cli_path: PathBuf,
    socket_path: PathBuf,
    network: CardanoNetwork,
}

impl AdapterBuilder {
    /// Creates a fresh `AdapterBuilder` with the given details.
    pub fn new(
        adapter_type: &AdapterType,
        cli_path: &PathBuf,
        socket_path: &PathBuf,
        network: &CardanoNetwork,
    ) -> Self {
        Self {
            adapter_type: adapter_type.to_owned(),
            cli_path: cli_path.to_owned(),
            socket_path: socket_path.to_owned(),
            network: network.to_owned(),
        }
    }

    /// Builds a `ChainObserver` based on the chosen `AdapterType`.
    pub fn build(&self) -> StdResult<Arc<dyn ChainObserver>> {
        match self.adapter_type {
            AdapterType::CardanoCliChainObserver => {
                let cli_runner = CardanoCliRunner::new(
                    self.cli_path.clone(),
                    self.socket_path.clone(),
                    self.network,
                );
                let chain = Box::new(cli_runner);
                let observer = CardanoCliChainObserver::new(chain);
                Ok(Arc::new(observer))
            }
            AdapterType::PallasObserver => {
                let observer =
                    PallasObserver::new(self.socket_path.as_ref(), self.network.as_u64());
                Ok(Arc::new(observer))
            }
        }
    }
}
