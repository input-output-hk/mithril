use crate::chain_observer::ChainAddress;
use crate::crypto_helper::key_decode_hex;
use crate::{chain_observer::ChainObserver, entities::Epoch};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::error::Error as StdError;
use std::sync::Arc;
use thiserror::Error;

// TODO: remove EraMarker & EraReaderAdapter w/ they are available

/// Value object that represents a tag of Era change.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EraMarker {
    /// Era name
    pub name: String,

    /// Eventual information that advertises the Epoch of transition.
    pub epoch: Option<Epoch>,
}

impl EraMarker {
    /// instanciate a new [EraMarker].
    pub fn new(name: &str, epoch: Option<Epoch>) -> Self {
        let name = name.to_string();

        Self { name, epoch }
    }
}

#[async_trait]
pub trait EraReaderAdapter: Sync + Send {
    /// Read era markers from the underlying adapter.
    async fn read(&self) -> Result<Vec<EraMarker>, Box<dyn StdError + Sync + Send>>;
}

// TODO: Keep Cardano Chain Adapter part below

type HexEncodeEraMarkerSignature = String;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct EraMarkersPayload {
    markers: Vec<EraMarker>,
    signature: HexEncodeEraMarkerSignature,
}

/// Cardano Chain adapter retrieves era markers on chain
pub struct CardanoChainAdapter {
    address: ChainAddress,
    chain_observer: Arc<dyn ChainObserver>,
}

impl CardanoChainAdapter {
    /// CardanoChainAdapter factory
    pub fn new(address: ChainAddress, chain_observer: Arc<dyn ChainObserver>) -> Self {
        Self {
            address,
            chain_observer,
        }
    }
}

#[async_trait]
impl EraReaderAdapter for CardanoChainAdapter {
    async fn read(&self) -> Result<Vec<EraMarker>, Box<dyn StdError + Sync + Send>> {
        let tx_datums = self
            .chain_observer
            .get_current_datums(&self.address)
            .await?;
        let markers_list = tx_datums
            .into_iter()
            .filter_map(|datum| datum.get_field_raw_value("bytes", 0).ok())
            .filter_map(|field_value| field_value.as_str().map(|s| s.to_string()))
            .filter_map(|field_value_str| key_decode_hex(&field_value_str).ok())
            .map(|era_markers_payload: EraMarkersPayload| era_markers_payload.markers)
            .collect::<Vec<Vec<EraMarker>>>();
        Ok(markers_list.first().unwrap_or(&Vec::new()).to_owned())
    }
}

#[cfg(test)]
mod test {
    use crate::chain_observer::{FakeObserver, TxDatum};
    use crate::crypto_helper::key_encode_hex;

    use super::*;

    fn dummy_tx_datums_from_markers_payload(payloads: Vec<EraMarkersPayload>) -> Vec<TxDatum> {
        payloads
            .into_iter()
            .map(|payload| {
                TxDatum(format!(
                    "{{\"constructor\":0,\"fields\":[{{\"bytes\":\"{}\"}}]}}",
                    key_encode_hex(payload).unwrap()
                ))
            })
            .collect()
    }

    #[tokio::test]
    async fn test_cardano_chain_adapter() {
        let fake_address = "addr_test_123456".to_string();
        let era_marker_payload_1 = EraMarkersPayload {
            markers: vec![
                EraMarker::new("thales", Some(Epoch(1))),
                EraMarker::new("pythagors", None),
            ],
            signature: "".to_string(),
        };
        let era_marker_payload_2 = EraMarkersPayload {
            markers: vec![
                EraMarker::new("thales", Some(Epoch(1))),
                EraMarker::new("pythagors", Some(Epoch(2))),
            ],
            signature: "".to_string(),
        };
        let mut fake_datums = dummy_tx_datums_from_markers_payload(vec![
            era_marker_payload_1.clone(),
            era_marker_payload_2,
        ]);
        fake_datums.push(TxDatum("not_valid_datum".to_string()));
        let chain_observer = FakeObserver::default();
        chain_observer.set_datums(fake_datums.clone()).await;
        let cardano_chain_adapter =
            CardanoChainAdapter::new(fake_address, Arc::new(chain_observer));
        let markers = cardano_chain_adapter
            .read()
            .await
            .expect("CardanoChainAdapter read should not fail");
        let expected_markers = era_marker_payload_1.markers.to_owned();
        assert_eq!(expected_markers, markers);
    }
}
