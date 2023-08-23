use crate::{
    chain_observer::{ChainAddress, ChainObserver, TxDatumFieldTypeName},
    crypto_helper::{
        key_decode_hex, EraMarkersSigner, EraMarkersVerifier, EraMarkersVerifierSignature,
        EraMarkersVerifierVerificationKey,
    },
    era::{EraMarker, EraReaderAdapter},
    StdError, StdResult,
};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use thiserror::Error;

/// [EraMarkersPayload] related errors.
#[derive(Debug, Error)]
pub enum EraMarkersPayloadError {
    /// Error raised when the message serialization fails
    #[error("could not serialize message: {0:?}")]
    SerializeMessage(StdError),

    /// Error raised when the signature deserialization fails
    #[error("could not deserialize signature: {0:?}")]
    DeserializeSignature(StdError),

    /// Error raised when the signature is missing
    #[error("could not verify signature: signature is missing")]
    MissingSignature,

    /// Error raised when the signature is invalid
    #[error("could not verify signature: {0:?}")]
    VerifySignature(StdError),

    /// Error raised when the signing the markers
    #[error("could not create signature: {0:?}")]
    CreateSignature(StdError),
}

/// Era markers payload
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EraMarkersPayload {
    /// List of Era markers
    pub markers: Vec<EraMarker>,

    /// Era markers signature
    pub signature: Option<EraMarkersVerifierSignature>,
}

impl EraMarkersPayload {
    fn message_to_bytes(&self) -> Result<Vec<u8>, EraMarkersPayloadError> {
        serde_json::to_vec(&self.markers)
            .map_err(|e| EraMarkersPayloadError::SerializeMessage(e.into()))
    }

    /// Verify the signature an era markers payload
    pub fn verify_signature(
        &self,
        verification_key: EraMarkersVerifierVerificationKey,
    ) -> Result<(), EraMarkersPayloadError> {
        let signature = self
            .signature
            .ok_or(EraMarkersPayloadError::MissingSignature)?;
        let markers_verifier = EraMarkersVerifier::from_verification_key(verification_key);

        markers_verifier
            .verify(&self.message_to_bytes()?, &signature)
            .map_err(|e| EraMarkersPayloadError::VerifySignature(e.into()))
    }

    /// Sign an era markers payload
    pub fn sign(self, signer: &EraMarkersSigner) -> Result<Self, EraMarkersPayloadError> {
        let signature = signer.sign(
            &self
                .message_to_bytes()
                .map_err(|e| EraMarkersPayloadError::CreateSignature(e.into()))?,
        );

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
    async fn read(&self) -> StdResult<Vec<EraMarker>> {
        let tx_datums = self
            .chain_observer
            .get_current_datums(&self.address)
            .await?;
        let markers_list = tx_datums
            .into_iter()
            .filter_map(|datum| datum.get_fields_by_type(&TxDatumFieldTypeName::Bytes).ok())
            .map(|fields| {
                fields
                    .iter()
                    .filter_map(|field_value| field_value.as_str().map(|s| s.to_string()))
                    .collect::<Vec<String>>()
                    .join("")
            })
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
    use crate::chain_observer::{FakeObserver, TxDatum, TxDatumBuilder, TxDatumFieldValue};
    use crate::crypto_helper::{key_encode_hex, EraMarkersSigner};
    use crate::entities::Epoch;

    use super::*;

    const GOLDEN_ERA_MARKERS_PAYLOAD_WITH_SIGNATURE: &str =
        "7b226d61726b657273223a5b7b226e616d65223a227468616c6573222c2265706f6368223a317d2c7b226e616d\
        65223a227079746861676f726173222c2265706f6368223a327d5d2c227369676e6174757265223a22633539373\
        9653333663163336234376361306162353239386536353562316264653235656564303866356232653536663361\
        6439623964373638316164663138653164656562623731616135616132636234363564643831323239633637656\
        33030326463396632663563363664663931333164366561633039666565373065227d";

    fn dummy_tx_datums_from_markers_payload(payloads: Vec<EraMarkersPayload>) -> Vec<TxDatum> {
        payloads
            .into_iter()
            .map(|payload| {
                TxDatumBuilder::new()
                    .add_field(TxDatumFieldValue::Bytes(key_encode_hex(payload).unwrap()))
                    .build()
                    .unwrap()
            })
            .collect()
    }

    #[test]
    fn golden_markers_payload_with_signature() {
        let _: EraMarkersPayload = key_decode_hex(GOLDEN_ERA_MARKERS_PAYLOAD_WITH_SIGNATURE)
            .expect("Decoding golden markers payload should not fail");
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
            signature: None,
        };
        let era_marker_payload_2 = EraMarkersPayload {
            markers: vec![
                EraMarker::new("thales", Some(Epoch(1))),
                EraMarker::new("pythagoras", Some(Epoch(2))),
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
        let expected_markers = era_marker_payload_2.markers.to_owned();
        assert_eq!(expected_markers, markers);
    }
}
