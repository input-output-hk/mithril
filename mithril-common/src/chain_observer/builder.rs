use serde::{Deserialize, Serialize};
use std::{fmt::Display, path::PathBuf, sync::Arc};
use thiserror::Error;

use crate::{chain_observer::ChainObserver, CardanoNetwork, StdResult};

#[cfg(any(test, feature = "test_tools"))]
use super::FakeObserver;
use super::{CardanoCliChainObserver, CardanoCliRunner, PallasChainObserver};

/// Type of chain observers available
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum ChainObserverType {
    /// Cardano Cli chain observer.
    #[serde(rename = "cardano-cli")]
    CardanoCli,
    /// Pallas chain observer.
    Pallas,
    /// Fake chain observer.
    #[cfg(any(test, feature = "test_tools"))]
    Fake,
}

impl Display for ChainObserverType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CardanoCli => write!(f, "cardano-cli"),
            Self::Pallas => write!(f, "pallas"),
            #[cfg(any(test, feature = "test_tools"))]
            Self::Fake => write!(f, "fake"),
        }
    }
}

/// Error type for chain observer builder service.
#[derive(Error, Debug)]
pub enum ChainObserverBuilderError {
    /// Missing cardano cli runner error.
    #[error("cardano cli runner is missing")]
    MissingCardanoCliRunner,
}

/// Chain observer builder
pub struct ChainObserverBuilder {
    chain_observer_type: ChainObserverType,
    cardano_node_socket_path: PathBuf,
    cardano_network: CardanoNetwork,
    cardano_cli_runner: Option<Box<CardanoCliRunner>>,
}

impl ChainObserverBuilder {
    /// Chain observer builder factory
    pub fn new(
        chain_observer_type: &ChainObserverType,
        cardano_node_socket_path: &PathBuf,
        cardano_node_network: &CardanoNetwork,
        cardano_cli_runner: Option<&CardanoCliRunner>,
    ) -> Self {
        Self {
            chain_observer_type: chain_observer_type.to_owned(),
            cardano_node_socket_path: cardano_node_socket_path.to_owned(),
            cardano_network: cardano_node_network.to_owned(),
            cardano_cli_runner: cardano_cli_runner.map(|c| c.to_owned().into()),
        }
    }

    /// Create chain observer
    pub fn build(&self) -> StdResult<Arc<dyn ChainObserver>> {
        match self.chain_observer_type {
            ChainObserverType::CardanoCli => Ok(Arc::new(CardanoCliChainObserver::new(
                self.cardano_cli_runner
                    .as_ref()
                    .ok_or(ChainObserverBuilderError::MissingCardanoCliRunner)?
                    .to_owned(),
            ))),
            ChainObserverType::Pallas => {
                let fallback = CardanoCliChainObserver::new(
                    self.cardano_cli_runner
                        .as_ref()
                        .ok_or(ChainObserverBuilderError::MissingCardanoCliRunner)?
                        .to_owned(),
                );
                let observer = PallasChainObserver::new(
                    &self.cardano_node_socket_path,
                    self.cardano_network,
                    fallback,
                );
                Ok(Arc::new(observer))
            }
            #[cfg(any(test, feature = "test_tools"))]
            ChainObserverType::Fake => Ok(Arc::new(FakeObserver::default())),
        }
    }
}
