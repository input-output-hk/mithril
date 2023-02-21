use std::error::Error;

use mithril_common::{
    chain_observer::{TxDatumBuilder, TxDatumFieldValue},
    crypto_helper::{key_encode_hex, EraMarkersSigner},
    entities::Epoch,
    era::{adapters::EraMarkersPayloadCardanoChain, EraMarker, SupportedEra},
};

type EraToolsResult<R> = Result<R, Box<dyn Error>>;

pub struct EraTools {}

impl EraTools {
    pub fn new() -> Self {
        Self {}
    }

    /// Get list of supported eras
    pub fn get_supported_eras_list(&self) -> EraToolsResult<Vec<SupportedEra>> {
        Ok(SupportedEra::eras())
    }

    /// Generate TxDatum for eras with sanity check of epochs
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
        for (index, era) in SupportedEra::eras().iter().enumerate() {
            let era_marker = match index {
                0 => EraMarker::new(&era.to_string(), Some(current_era_epoch)),
                1 => EraMarker::new(&era.to_string(), maybe_next_era_epoch),
                _ => Err("too many eras retrieved, can't generate tx datum".to_string())?,
            };
            era_markers.push(era_marker);
        }
        let era_markers_payload = EraMarkersPayloadCardanoChain {
            markers: era_markers,
            signature: None,
        }
        .sign(era_markers_signer)?;

        let tx_datum = TxDatumBuilder::new()
            .add_field(TxDatumFieldValue::Bytes(
                key_encode_hex(era_markers_payload)
                    .map_err(|e| format!("era markerspayload could not be hex encoded: {e}"))?,
            ))
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
        let supported_eras_list = era_tools
            .get_supported_eras_list()
            .expect("get_supported_eras_list should not fail");
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
