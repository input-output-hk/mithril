use std::{error::Error, sync::Arc};

use mithril_common::{
    chain_observer::{TxDatumBuilder, TxDatumFieldTypeName, TxDatumFieldValue},
    crypto_helper::{key_encode_hex, EraMarkersSigner, EraMarkersVerifier},
    entities::Epoch,
    era::{adapters::EraMarkersPayloadCardanoChain, EraMarker, SupportedEra},
};

type EraToolsResult<R> = Result<R, Box<dyn Error>>;

pub struct EraToolsDependency {
    /// Era markers signature verifier service.
    pub era_markers_verifier: Option<Arc<EraMarkersVerifier>>,
}

pub struct EraTools {}

impl EraTools {
    pub fn new() -> Self {
        Self {}
    }

    pub async fn from_dependencies(_dependencies: EraToolsDependency) -> EraToolsResult<Self> {
        Ok(Self::new())
    }

    /// Get list of supported eras
    pub fn get_supported_eras_list(&self) -> EraToolsResult<String> {
        Ok(serde_json::to_string(&SupportedEra::eras())?)
    }

    /// Generate TxDatum for eras
    pub fn generate_tx_datum(
        &self,
        current_era_epoch: Epoch,
        maybe_next_era_epoch: Option<Epoch>,
        era_markers_signer: &EraMarkersSigner,
    ) -> EraToolsResult<String> {
        if maybe_next_era_epoch.unwrap_or_default() >= current_era_epoch {
            Err("next era epoch must be strictly greater than the current era epoch".to_string())?;
        }

        let mut era_markers = Vec::new();
        for (index, era) in SupportedEra::eras().into_iter().enumerate() {
            let era_marker = match index {
                0 => EraMarker::new(&era.to_string(), Some(current_era_epoch)),
                1 => EraMarker::new(&era.to_string(), maybe_next_era_epoch),
                _ => unreachable!(),
            };
            era_markers.push(era_marker);
        }
        let era_markers_payload = EraMarkersPayloadCardanoChain {
            markers: era_markers,
            signature: None,
        }
        .sign(era_markers_signer)?;

        let tx_datum = TxDatumBuilder::new()
            .add_field(
                TxDatumFieldTypeName::Bytes,
                TxDatumFieldValue::Bytes(
                    key_encode_hex(era_markers_payload)
                        .map_err(|e| format!("era markerspayload could not be hex encoded: {e}"))?,
                ),
            )
            .build()?;

        Ok(tx_datum.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn build_tools() -> EraTools {
        EraTools {}
    }

    #[test]
    fn get_supported_eras_list() {
        let era_tools = build_tools();
        let supported_eras_list_serialized = era_tools
            .get_supported_eras_list()
            .expect("get_supported_eras_list should not fail");
        let supported_eras_list: Vec<SupportedEra> =
            serde_json::from_str(&supported_eras_list_serialized)
                .expect("supported_eras_list_serialized should be a valid JSON");
        assert_eq!(supported_eras_list, SupportedEra::eras());
    }

    #[test]
    fn generate_tx_datum_ok() {
        let era_markers_signer = EraMarkersSigner::create_deterministic_signer();
        let era_tools = build_tools();
        let _ = era_tools
            .generate_tx_datum(Epoch(1), None, &era_markers_signer)
            .expect("generate_tx_datum should not fail");
    }

    #[test]
    fn generate_tx_datum_wrong_epochs() {
        let era_markers_signer = EraMarkersSigner::create_deterministic_signer();
        let era_tools = build_tools();
        let _ = era_tools
            .generate_tx_datum(Epoch(1), Some(Epoch(2)), &era_markers_signer)
            .expect_err("generate_tx_datum should have failed");
    }
}
