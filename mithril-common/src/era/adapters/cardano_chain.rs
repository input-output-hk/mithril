use crate::{
    chain_observer::{ChainAddress, ChainObserver, TxDatumFieldTypeName},
    crypto_helper::{
        key_decode_hex, EraMarkersSigner, EraMarkersVerifier, EraMarkersVerifierSignature,
        EraMarkersVerifierVerificationKey,
    },
    entities::{Epoch, HexEncodedEraMarkersSignature},
    era::{EraMarker, EraReaderAdapter},
};
use async_trait::async_trait;
use hex::{FromHex, ToHex};
use serde::{Deserialize, Serialize};
use std::error::Error as StdError;
use std::sync::Arc;
use thiserror::Error;

type GeneralError = Box<dyn StdError + Sync + Send>;

/// [EraMarkersPayload] related errors.
#[derive(Debug, Error)]
pub enum EraMarkersPayloadError {
    /// Error raised when the message serialization fails
    #[error("could not serialize message: {0}")]
    SerializeMessage(GeneralError),

    /// Error raised when the signature deserialization fails
    #[error("could not deserialize signature: {0}")]
    DeserializeSignature(GeneralError),

    /// Error raised when the signature is missing
    #[error("could not verify signature: signature is missing")]
    MissingSignature,

    /// Error raised when the signature is invalid
    #[error("could not verify signature: {0}")]
    VerifySignature(GeneralError),

    /// Error raised when the signing the markers
    #[error("could not create signature: {0}")]
    CreateSignature(GeneralError),
}

/// Era marker item
/// Value object that represents a tag of Era change.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EraMarkerItem {
    /// Era name
    #[serde(rename = "n")]
    pub name: String,

    /// Eventual information that advertises the Epoch of transition.
    #[serde(rename = "e")]
    pub epoch: Option<Epoch>,
}

impl EraMarkerItem {
    /// Instantiate a new [EraMarkerItem].
    pub fn new(name: &str, epoch: Option<Epoch>) -> Self {
        let name = name.to_string();

        Self { name, epoch }
    }
}

impl From<EraMarker> for EraMarkerItem {
    fn from(other: EraMarker) -> EraMarkerItem {
        EraMarkerItem::new(&other.name, other.epoch)
    }
}

impl From<EraMarkerItem> for EraMarker {
    fn from(other: EraMarkerItem) -> EraMarker {
        EraMarker::new(&other.name, other.epoch)
    }
}

/// Era markers payload
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EraMarkersPayload {
    /// List of Era markers
    pub markers: Vec<EraMarkerItem>,

    /// Era markers signature
    pub signature: Option<HexEncodedEraMarkersSignature>,
}

impl EraMarkersPayload {
    fn message_to_bytes(&self) -> Result<Vec<u8>, EraMarkersPayloadError> {
        serde_json::to_vec(&self.markers)
            .map_err(|e| EraMarkersPayloadError::SerializeMessage(e.into()))
    }

    fn deserialize_signature(&self) -> Result<EraMarkersVerifierSignature, EraMarkersPayloadError> {
        EraMarkersVerifierSignature::from_bytes(
            &Vec::from_hex(
                self.signature
                    .as_ref()
                    .ok_or(EraMarkersPayloadError::MissingSignature)?,
            )
            .map_err(|e| EraMarkersPayloadError::DeserializeSignature(e.into()))?,
        )
        .map_err(|e| EraMarkersPayloadError::DeserializeSignature(e.into()))
    }

    /// Verify the signature an era markers payload
    pub fn verify_signature(
        &self,
        verification_key: EraMarkersVerifierVerificationKey,
    ) -> Result<(), EraMarkersPayloadError> {
        let markers_verifier = EraMarkersVerifier::from_verification_key(verification_key);

        markers_verifier
            .verify(&self.message_to_bytes()?, &self.deserialize_signature()?)
            .map_err(|e| EraMarkersPayloadError::VerifySignature(e.into()))
    }

    /// Sign an era markers payload
    pub fn sign(self, signer: &EraMarkersSigner) -> Result<Self, EraMarkersPayloadError> {
        let signature = signer
            .sign(
                &self
                    .message_to_bytes()
                    .map_err(|e| EraMarkersPayloadError::CreateSignature(e.into()))?,
            )
            .encode_hex::<String>();

        Ok(Self {
            markers: self.markers,
            signature: Some(signature),
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
            .filter_map(|datum| {
                match (
                    datum.get_nth_field_by_type(&TxDatumFieldTypeName::Bytes, 0),
                    datum.get_nth_field_by_type(&TxDatumFieldTypeName::Bytes, 1),
                ) {
                    (Ok(markers), Ok(signature)) => {
                        let markers = markers.as_str().map(|s| s.to_string()).unwrap_or_default();
                        let signature = signature.as_str().map(|s| s.to_string());
                        match key_decode_hex::<Vec<_>>(&markers) {
                            Ok(markers) => EraMarkersPayload {
                                markers: markers.clone(),
                                signature,
                            }
                            .verify_signature(self.verification_key)
                            .ok()
                            .map(|_| markers.into_iter().map(|em| em.into()).collect()),
                            Err(_) => None,
                        }
                    }
                    _ => None,
                }
            })
            .collect::<Vec<Vec<EraMarker>>>();

        Ok(markers_list.first().unwrap_or(&Vec::new()).to_owned())
    }
}

#[cfg(test)]
mod test {
    use crate::chain_observer::{FakeObserver, TxDatum, TxDatumBuilder, TxDatumFieldValue};
    use crate::crypto_helper::{key_encode_hex, EraMarkersSigner};
    use crate::entities::Epoch;

    use super::*;

    fn dummy_tx_datums_from_markers_payload(payloads: Vec<EraMarkersPayload>) -> Vec<TxDatum> {
        payloads
            .into_iter()
            .map(|payload| {
                TxDatumBuilder::new()
                    .add_field(TxDatumFieldValue::Bytes(
                        key_encode_hex(payload.markers).unwrap(),
                    ))
                    .add_field(TxDatumFieldValue::Bytes(
                        payload.signature.unwrap_or_default(),
                    ))
                    .build()
                    .unwrap()
            })
            .collect()
    }

    #[tokio::test]
    async fn test_cardano_chain_adapter() {
        let era_markers_signer = EraMarkersSigner::create_deterministic_signer();
        let fake_address = "addr_test_123456".to_string();
        let era_marker_payload_1 = EraMarkersPayload {
            markers: vec![
                EraMarkerItem::new("thales", Some(Epoch(1))),
                EraMarkerItem::new("pythagoras", None),
            ],
            signature: None,
        };
        let era_marker_payload_2 = EraMarkersPayload {
            markers: vec![
                EraMarkerItem::new("thales", Some(Epoch(1))),
                EraMarkerItem::new("pythagoras", Some(Epoch(2))),
            ],
            signature: None,
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
        let expected_markers = era_marker_payload_2
            .markers
            .into_iter()
            .map(|em| em.into())
            .collect::<Vec<EraMarker>>();
        assert_eq!(expected_markers, markers);
    }
}
