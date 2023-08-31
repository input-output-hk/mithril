use anyhow::anyhow;
use serde::Serialize;
use serde_json::Value;
use std::collections::HashMap;
use strum::{Display, EnumDiscriminants};
use thiserror::Error;

use crate::{StdError, StdResult};

/// [ChainAddress] represents an on chain address.
pub type ChainAddress = String;

/// [TxDatum] related errors.
#[derive(Debug, Error)]
pub enum TxDatumError {
    /// Error raised when the content could not be parsed.
    #[error("could not parse tx datum: {0:?}")]
    InvalidContent(StdError),

    /// Error raised when building the tx datum failed.
    #[error("could not build tx datum: {0}")]
    Build(serde_json::Error),
}

/// [TxDatum] represents transaction Datum.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TxDatum(pub String);

impl TxDatum {
    /// Retrieves the fields of the datum with given type
    pub fn get_fields_by_type(&self, type_name: &TxDatumFieldTypeName) -> StdResult<Vec<Value>> {
        let tx_datum_raw = &self.0;
        // 1- Parse the Utxo raw data to a hashmap
        let v: HashMap<String, Value> = serde_json::from_str(tx_datum_raw).map_err(|e| {
            TxDatumError::InvalidContent(
                anyhow!(e).context(format!("tx datum was = '{tx_datum_raw}'")),
            )
        })?;
        // 2- Convert the 'fields' entry to a vec of json objects
        let fields = v.get("fields").ok_or_else(|| {
            TxDatumError::InvalidContent(
                anyhow!("Error: missing 'fields' entry, tx datum was = '{tx_datum_raw}'"),
            )
        })?.as_array().ok_or_else(|| {
            TxDatumError::InvalidContent(
                anyhow!("Error: 'fields' entry is not correctly structured, tx datum was = '{tx_datum_raw}'"),
            )
        })?;
        // 3- Filter the vec (keep the ones that match the given type), and retrieve the nth entry of this filtered vec
        Ok(fields
            .iter()
            .filter(|&field| field.get(type_name.to_string()).is_some())
            .map(|field| field.get(type_name.to_string()).unwrap().to_owned())
            .collect::<_>())
    }

    /// Retrieves the nth field of the datum with given type
    pub fn get_nth_field_by_type(
        &self,
        type_name: &TxDatumFieldTypeName,
        index: usize,
    ) -> StdResult<Value> {
        Ok(self
            .get_fields_by_type(type_name)?
            .get(index)
            .ok_or_else(|| {
                TxDatumError::InvalidContent(anyhow!("Error: missing field at index {index}"))
            })?
            .to_owned())
    }
}

/// [TxDatumFieldValue] represents a field value of TxDatum.
#[derive(Debug, EnumDiscriminants, Serialize, Display)]
#[serde(untagged, rename_all = "lowercase")]
#[strum(serialize_all = "lowercase")]
#[strum_discriminants(derive(Serialize, Hash, Display))]
#[strum_discriminants(name(TxDatumFieldTypeName))]
#[strum_discriminants(strum(serialize_all = "lowercase"))]
#[strum_discriminants(serde(rename_all = "lowercase"))]
pub enum TxDatumFieldValue {
    /// Bytes datum field value.
    Bytes(String),
    /// Integer datum field value
    #[allow(dead_code)]
    Int(u32),
}

/// [TxDatumBuilder] is a [TxDatum] builder utility.
#[derive(Debug, Serialize)]
pub struct TxDatumBuilder {
    constructor: usize,
    fields: Vec<HashMap<TxDatumFieldTypeName, TxDatumFieldValue>>,
}

impl TxDatumBuilder {
    /// [TxDatumBuilder] factory
    pub fn new() -> Self {
        Self {
            constructor: 0,
            fields: Vec::new(),
        }
    }

    /// Add a field to the builder
    pub fn add_field(&mut self, field_value: TxDatumFieldValue) -> &mut TxDatumBuilder {
        match &field_value {
            TxDatumFieldValue::Bytes(datum_str) => {
                // TODO: Remove this chunking of the bytes fields once the cardano-cli 1.36.0+ is released
                // The bytes fields are currently limited to 128 bytes and need to be chunked in multiple fields
                let field_type = TxDatumFieldTypeName::from(&field_value);
                let field_value_chunks = datum_str.as_bytes().chunks(128);
                for field_value_chunk in field_value_chunks {
                    let mut field = HashMap::new();
                    field.insert(
                        field_type,
                        TxDatumFieldValue::Bytes(
                            std::str::from_utf8(field_value_chunk).unwrap().to_string(),
                        ),
                    );
                    self.fields.push(field);
                }
            }
            _ => {
                let mut field = HashMap::new();
                field.insert(TxDatumFieldTypeName::from(&field_value), field_value);
                self.fields.push(field);
            }
        }

        self
    }

    /// Build a [TxDatum]
    pub fn build(&self) -> Result<TxDatum, TxDatumError> {
        Ok(TxDatum(
            serde_json::to_string(&self).map_err(TxDatumError::Build)?,
        ))
    }
}

impl Default for TxDatumBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn dummy_tx_datum() -> TxDatum {
        let mut tx_datum_builder = TxDatumBuilder::new();
        let tx_datum = tx_datum_builder
            .add_field(TxDatumFieldValue::Bytes("bytes0".to_string()))
            .add_field(TxDatumFieldValue::Int(0))
            .add_field(TxDatumFieldValue::Int(1))
            .add_field(TxDatumFieldValue::Bytes("bytes1".to_string()))
            .add_field(TxDatumFieldValue::Bytes("bytes2".to_string()))
            .add_field(TxDatumFieldValue::Int(2))
            .add_field(TxDatumFieldValue::Bytes("012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789".to_string()))
            .build()
            .expect("tx_datum build should not fail");
        tx_datum
    }

    #[test]
    fn test_build_tx_datum() {
        let tx_datum = dummy_tx_datum();
        let tx_datum_expected = TxDatum(r#"{"constructor":0,"fields":[{"bytes":"bytes0"},{"int":0},{"int":1},{"bytes":"bytes1"},{"bytes":"bytes2"},{"int":2},{"bytes":"01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567"},{"bytes":"8901234567890123456789"}]}"#.to_string());
        assert_eq!(tx_datum_expected, tx_datum);
    }

    #[test]
    fn test_can_retrieve_field_raw_value_bytes() {
        let tx_datum = dummy_tx_datum();
        assert_eq!(
            "bytes0",
            tx_datum
                .get_nth_field_by_type(&TxDatumFieldTypeName::Bytes, 0)
                .unwrap()
                .as_str()
                .unwrap()
        );
        assert_eq!(
            "bytes1",
            tx_datum
                .get_nth_field_by_type(&TxDatumFieldTypeName::Bytes, 1)
                .unwrap()
                .as_str()
                .unwrap()
        );
        assert_eq!(
            "bytes2",
            tx_datum
                .get_nth_field_by_type(&TxDatumFieldTypeName::Bytes, 2)
                .unwrap()
                .as_str()
                .unwrap()
        );
        tx_datum
            .get_nth_field_by_type(&TxDatumFieldTypeName::Bytes, 100)
            .expect_err("should have returned an error");
    }

    #[test]
    fn test_can_retrieve_field_raw_value_int() {
        let tx_datum = dummy_tx_datum();
        assert_eq!(
            0,
            tx_datum
                .get_nth_field_by_type(&TxDatumFieldTypeName::Int, 0)
                .unwrap()
                .as_u64()
                .unwrap()
        );
        assert_eq!(
            1,
            tx_datum
                .get_nth_field_by_type(&TxDatumFieldTypeName::Int, 1)
                .unwrap()
                .as_u64()
                .unwrap()
        );
        assert_eq!(
            2,
            tx_datum
                .get_nth_field_by_type(&TxDatumFieldTypeName::Int, 2)
                .unwrap()
                .as_u64()
                .unwrap()
        );
        tx_datum
            .get_nth_field_by_type(&TxDatumFieldTypeName::Int, 100)
            .expect_err("should have returned an error");
    }
}
