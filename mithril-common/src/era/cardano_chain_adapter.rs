use crate::{
    chain_observer::{ChainAddress, ChainObserver},
    crypto_helper::{
        key_decode_hex, EraMarkersSigner, EraMarkersVerifier, EraMarkersVerifierSignature,
        EraMarkersVerifierVerificationKey,
    },
    entities::Epoch,
};
use async_trait::async_trait;
use hex::{FromHex, ToHex};
use serde::{Deserialize, Serialize};
use std::error::Error as StdError;
use std::sync::Arc;
use thiserror::Error;

type GeneralError = Box<dyn StdError + Sync + Send>;

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
    async fn read(&self) -> Result<Vec<EraMarker>, GeneralError>;
}

// TODO: Keep Cardano Chain Adapter part below

type HexEncodeEraMarkerSignature = String;

/// [EraMarkersPayload] related errors.
#[derive(Debug, Error)]
pub enum EraMarkersPayloadError {
    /// Error raised when the message serialization fails
    #[error("could not serialize message: {0}")]
    SerializeMessage(GeneralError),

    /// Error raised when the signature deserialization fails
    #[error("could not deserialize signature: {0}")]
    DeserializeSignature(GeneralError),

    /// Error raised when the signature is invalid
    #[error("could not verify signature: {0}")]
    VerifySignature(GeneralError),

    /// Error raised when the signing the markers
    #[error("could not create signature: {0}")]
    CreateSignature(GeneralError),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct EraMarkersPayload {
    markers: Vec<EraMarker>,
    signature: HexEncodeEraMarkerSignature,
}

impl EraMarkersPayload {
    fn message_to_sign(&self) -> Result<Vec<u8>, EraMarkersPayloadError> {
        serde_json::to_vec(&self.markers)
            .map_err(|e| EraMarkersPayloadError::SerializeMessage(e.into()))
    }

    fn verify_signature(
        &self,
        verification_key: EraMarkersVerifierVerificationKey,
    ) -> Result<(), EraMarkersPayloadError> {
        let signature = EraMarkersVerifierSignature::from_bytes(
            &Vec::from_hex(&self.signature)
                .map_err(|e| EraMarkersPayloadError::DeserializeSignature(e.into()))?,
        )
        .map_err(|e| EraMarkersPayloadError::DeserializeSignature(e.into()))?;
        let markers_verifier = EraMarkersVerifier::from_verification_key(verification_key);
        markers_verifier
            .verify(&self.message_to_sign()?, &signature)
            .map_err(|e| EraMarkersPayloadError::VerifySignature(e.into()))
    }

    #[cfg(any(test, feature = "test_only"))]
    fn sign(self, signer: &EraMarkersSigner) -> Result<Self, EraMarkersPayloadError> {
        let signature = signer
            .sign(
                &self
                    .message_to_sign()
                    .map_err(|e| EraMarkersPayloadError::CreateSignature(e.into()))?,
            )
            .encode_hex::<String>();
        Ok(Self {
            markers: self.markers,
            signature,
        })
    }
}

/// Cardano Chain adapter retrieves era markers on chain
pub struct CardanoChainAdapter {
    address: ChainAddress,
    chain_observer: Arc<dyn ChainObserver>,
    verification_key: EraMarkersVerifierVerificationKey,
}

impl CardanoChainAdapter {
    /// CardanoChainAdapter factory
    pub fn new(
        address: ChainAddress,
        chain_observer: Arc<dyn ChainObserver>,
        verification_key: EraMarkersVerifierVerificationKey,
    ) -> Self {
        Self {
            address,
            chain_observer,
            verification_key,
        }
    }
}

#[async_trait]
impl EraReaderAdapter for CardanoChainAdapter {
    async fn read(&self) -> Result<Vec<EraMarker>, GeneralError> {
        let tx_datums = self
            .chain_observer
            .get_current_datums(&self.address)
            .await?;
        let markers_list = tx_datums
            .into_iter()
            .filter_map(|datum| datum.get_field_raw_value("bytes", 0).ok())
            .filter_map(|field_value| field_value.as_str().map(|s| s.to_string()))
            .filter_map(|field_value_str| key_decode_hex(&field_value_str).ok())
            .filter_map(|era_markers_payload: EraMarkersPayload| {
                era_markers_payload
                    .verify_signature(self.verification_key)
                    .ok()
                    .map(|_| era_markers_payload.markers)
            })
            .collect::<Vec<Vec<EraMarker>>>();
        Ok(markers_list.first().unwrap_or(&Vec::new()).to_owned())
    }
}

#[cfg(test)]
mod test {
    use crate::chain_observer::{FakeObserver, TxDatum};
    use crate::crypto_helper::{key_encode_hex, EraMarkersSigner};

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
        let era_markers_signer = EraMarkersSigner::create_deterministic_signer();
        let fake_address = "addr_test_123456".to_string();
        let era_marker_payload_1 = EraMarkersPayload {
            markers: vec![
                EraMarker::new("thales", Some(Epoch(1))),
                EraMarker::new("pythagoras", None),
            ],
            signature: "".to_string(),
        };
        let era_marker_payload_2 = EraMarkersPayload {
            markers: vec![
                EraMarker::new("thales", Some(Epoch(1))),
                EraMarker::new("pythagoras", Some(Epoch(2))),
            ],
            signature: "".to_string(),
        };
        let mut fake_datums = dummy_tx_datums_from_markers_payload(vec![
            era_marker_payload_1,
            era_marker_payload_2
                .clone()
                .sign(&era_markers_signer)
                .unwrap(),
        ]);
        fake_datums.push(TxDatum("not_valid_datum".to_string()));
        let chain_observer = FakeObserver::default();
        chain_observer.set_datums(fake_datums.clone()).await;
        let cardano_chain_adapter = CardanoChainAdapter::new(
            fake_address,
            Arc::new(chain_observer),
            era_markers_signer.create_verifier().to_verification_key(),
        );
        let markers = cardano_chain_adapter
            .read()
            .await
            .expect("CardanoChainAdapter read should not fail");
        let expected_markers = era_marker_payload_2.markers.to_owned();
        assert_eq!(expected_markers, markers);
    }
}
