use serde::Serialize;
use serde_json::Value;
use std::{collections::HashMap, error::Error as StdError};
use strum_macros::Display;
use thiserror::Error;

/// [ChainAddress] represents an on chain address.
pub type ChainAddress = String;

/// [TxDatum] related errors.
#[derive(Debug, Error)]
pub enum TxDatumError {
    /// Error raised when the content could not be parsed.
    #[error("could not parse tx datum: {0}")]
    InvalidContent(Box<dyn StdError + Sync + Send>),

    /// Error raised when building the tx datum failed.
    #[error("could not build tx datum: {0}")]
    Build(serde_json::Error),
}

/// [TxDatum] represents transaction Datum.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TxDatum(pub String);

impl TxDatum {
    /// Retrieves the nth field of the datum with given type
    pub fn get_nth_field_by_type(
        &self,
        type_name: &TxDatumFieldTypeName,
        index: usize,
    ) -> Result<Value, Box<dyn StdError>> {
        let tx_datum_raw = &self.0;
        // 1- Parse the Utxo raw data to a hashmap
        let v: HashMap<String, Value> = serde_json::from_str(tx_datum_raw).map_err(|e| {
            TxDatumError::InvalidContent(
                format!("Error: {e:?}, tx datum was = '{tx_datum_raw}'").into(),
            )
        })?;
        // 2- Convert the 'fields' entry to a vec of json objects
        let fields = v.get("fields").ok_or_else(|| {
            TxDatumError::InvalidContent(
                format!("Error: missing 'fields' entry, tx datum was = '{tx_datum_raw}'").into(),
            )
        })?.as_array().ok_or_else(|| {
            TxDatumError::InvalidContent(
                format!("Error: 'fields' entry is not correctly structured, tx datum was = '{tx_datum_raw}'").into(),
            )
        })?;
        // 3- Filter the vec (keep the ones that match the given type), and retrieve the nth entry of this filtered vec
        let field_value = fields
            .iter()
            .filter(|&field| field.get(type_name.to_string()).is_some())
            .nth(index)
            .ok_or_else(|| {
                TxDatumError::InvalidContent(
                    format!(
                        "Error: missing field at index {index}, tx datum was = '{tx_datum_raw}'"
                    )
                    .into(),
                )
            })?
            .get(type_name.to_string())
            .unwrap();

        Ok(field_value.to_owned())
    }
}

/// [TxDatumFieldTypeName] represents a fiel type name of TxDatum.
#[derive(Debug, Serialize, Hash, PartialEq, Eq, Display)]
#[serde(rename_all = "lowercase")]
#[strum(serialize_all = "lowercase")]
pub enum TxDatumFieldTypeName {
    /// Bytes datum field type name.
    Bytes,
    /// Integer datum field type name
    #[allow(dead_code)]
    Int,
}

/// [TxDatumFieldValue] represents a fiel value of TxDatum.
#[derive(Debug, Serialize)]
#[serde(untagged)]
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
    pub fn add_field(
        &mut self,
        field_type: TxDatumFieldTypeName,
        field_value: TxDatumFieldValue,
    ) -> &mut TxDatumBuilder {
        let mut field = HashMap::new();
        field.insert(field_type, field_value);
        self.fields.push(field);

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
            .add_field(
                TxDatumFieldTypeName::Bytes,
                TxDatumFieldValue::Bytes("bytes0".to_string()),
            )
            .add_field(TxDatumFieldTypeName::Int, TxDatumFieldValue::Int(0))
            .add_field(TxDatumFieldTypeName::Int, TxDatumFieldValue::Int(1))
            .add_field(
                TxDatumFieldTypeName::Bytes,
                TxDatumFieldValue::Bytes("bytes1".to_string()),
            )
            .add_field(
                TxDatumFieldTypeName::Bytes,
                TxDatumFieldValue::Bytes("bytes2".to_string()),
            )
            .add_field(TxDatumFieldTypeName::Int, TxDatumFieldValue::Int(2))
            .build()
            .expect("tx_datum build should not fail");
        tx_datum
    }

    #[test]
    fn test_build_tx_datum() {
        let tx_datum = dummy_tx_datum();
        let tx_datum_expected = TxDatum("{\"constructor\":0,\"fields\":[{\"bytes\":\"bytes0\"},{\"int\":0},{\"int\":1},{\"bytes\":\"bytes1\"},{\"bytes\":\"bytes2\"},{\"int\":2}]}".to_string());
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
