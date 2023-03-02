use serde::{Deserialize, Serialize};
use std::{path::PathBuf, sync::Arc};
use thiserror::Error;

use crate::{
    chain_observer::{ChainAddress, ChainObserver},
    crypto_helper::key_decode_hex,
    entities::HexEncodedEraMarkersSignature,
    era::{
        adapters::{
            EraReaderBootstrapAdapter, EraReaderCardanoChainAdapter, EraReaderDummyAdapter,
            EraReaderFileAdapter,
        },
        EraMarker, EraReaderAdapter,
    },
};

/// Type of era reader adapaters available
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum AdapterType {
    /// Cardano chain adapter.
    #[serde(rename = "cardano-chain")]
    CardanoChain,
    /// File adapter.
    File,
    /// Dummy adapter.
    Dummy,
    /// Bootstrap adapter.
    Bootstrap,
}

/// Error type for era adapter builder service.
#[derive(Error, Debug)]
pub enum AdapterBuilderError {
    /// Missing parameters error.
    #[error("era reader adapter parameters are missing")]
    MissingParameters(),

    /// Parameters parse error.
    #[error("era reader adapter parameters parse error: {0:?}")]
    ParseParameters(serde_json::Error),

    /// Parameters decode error.
    #[error("era reader adapter parameters decode error: {0:?}")]
    Decode(String),
}

/// Era adapter builder
pub struct AdapterBuilder {
    adapter_type: AdapterType,
    adapter_params: Option<String>,
}

impl AdapterBuilder {
    /// Era reader adapter builder factory
    pub fn new(adapter_type: &AdapterType, adapter_params: &Option<String>) -> Self {
        Self {
            adapter_type: adapter_type.to_owned(),
            adapter_params: adapter_params.to_owned(),
        }
    }

    /// Create era reader adapter from configuration settings.
    pub fn build(
        &self,
        chain_observer: Arc<dyn ChainObserver>,
    ) -> Result<Box<dyn EraReaderAdapter>, AdapterBuilderError> {
        match self.adapter_type {
            AdapterType::CardanoChain => {
                #[derive(Deserialize)]
                struct CardanoChainAdapterConfig {
                    address: ChainAddress,
                    verification_key: HexEncodedEraMarkersSignature,
                }

                let adapter_config: CardanoChainAdapterConfig = serde_json::from_str(
                    self.adapter_params
                        .as_ref()
                        .ok_or_else(AdapterBuilderError::MissingParameters)?,
                )
                .map_err(AdapterBuilderError::ParseParameters)?;

                Ok(Box::new(EraReaderCardanoChainAdapter::new(
                    adapter_config.address,
                    chain_observer,
                    key_decode_hex(&adapter_config.verification_key)
                        .map_err(AdapterBuilderError::Decode)?,
                )))
            }
            AdapterType::File => {
                #[derive(Deserialize)]
                struct EraReaderFileAdapterConfig {
                    markers_file: PathBuf,
                }

                let adapter_config: EraReaderFileAdapterConfig = serde_json::from_str(
                    self.adapter_params
                        .as_ref()
                        .ok_or_else(AdapterBuilderError::MissingParameters)?,
                )
                .map_err(AdapterBuilderError::ParseParameters)?;
                let file_adapter = EraReaderFileAdapter::new(adapter_config.markers_file);

                Ok(Box::new(file_adapter))
            }
            AdapterType::Dummy => {
                #[derive(Deserialize)]
                struct EraReaderDummyAdapterConfig {
                    markers: Vec<EraMarker>,
                }

                let adapter_config: EraReaderDummyAdapterConfig = serde_json::from_str(
                    self.adapter_params
                        .as_ref()
                        .ok_or_else(AdapterBuilderError::MissingParameters)?,
                )
                .map_err(AdapterBuilderError::ParseParameters)?;
                let mut dummy_adapter = EraReaderDummyAdapter::default();
                dummy_adapter.set_markers(adapter_config.markers);

                Ok(Box::new(dummy_adapter))
            }
            AdapterType::Bootstrap => Ok(Box::new(EraReaderBootstrapAdapter)),
        }
    }
}
