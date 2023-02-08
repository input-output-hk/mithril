use serde_json::Value;
use std::{collections::HashMap, error::Error as StdError};
use thiserror::Error;

/// [ChainAddress] represents an on chain address
pub type ChainAddress = String;

/// [TxDatum] related errors.
#[derive(Debug, Error)]
pub enum TxDatumError {
    /// Generic [TxDatum] error.
    #[error("general error {0}")]
    _General(Box<dyn StdError + Sync + Send>),

    /// Error raised when the content could not be parsed.
    #[error("could not parse content: {0}")]
    InvalidContent(Box<dyn StdError + Sync + Send>),
}

/// [TxDatum] represents transaction Datum
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TxDatum(pub String);

impl TxDatum {
    /// Retrieves the nth field of the datum with given type
    pub fn get_nth_field_by_type(
        &self,
        type_name: &str,
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
            .filter(|&field| field.get(type_name).is_some())
            .nth(index)
            .ok_or_else(|| {
                TxDatumError::InvalidContent(
                    format!(
                        "Error: missing field at index {index}, tx datum was = '{tx_datum_raw}'"
                    )
                    .into(),
                )
            })?
            .get(type_name)
            .unwrap();

        Ok(field_value.to_owned())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn dummy_tx_datum() -> TxDatum {
        TxDatum("{\"constructor\":0,\"fields\":[{\"bytes\":\"bytes0\"}, {\"int\":0}, {\"int\":1}, {\"bytes\":\"bytes1\"}, {\"bytes\":\"bytes2\"}, {\"int\":2}]}".to_string())
    }

    #[test]
    fn test_can_retrieve_field_raw_value_bytes() {
        let tx_datum = dummy_tx_datum();
        assert_eq!(
            "bytes0",
            tx_datum
                .get_nth_field_by_type("bytes", 0)
                .unwrap()
                .as_str()
                .unwrap()
        );
        assert_eq!(
            "bytes1",
            tx_datum
                .get_nth_field_by_type("bytes", 1)
                .unwrap()
                .as_str()
                .unwrap()
        );
        assert_eq!(
            "bytes2",
            tx_datum
                .get_nth_field_by_type("bytes", 2)
                .unwrap()
                .as_str()
                .unwrap()
        );
        tx_datum
            .get_nth_field_by_type("bytes", 100)
            .expect_err("should have returned an error");
    }

    #[test]
    fn test_can_retrieve_field_raw_value_int() {
        let tx_datum = dummy_tx_datum();
        assert_eq!(
            0,
            tx_datum
                .get_nth_field_by_type("int", 0)
                .unwrap()
                .as_u64()
                .unwrap()
        );
        assert_eq!(
            1,
            tx_datum
                .get_nth_field_by_type("int", 1)
                .unwrap()
                .as_u64()
                .unwrap()
        );
        assert_eq!(
            2,
            tx_datum
                .get_nth_field_by_type("int", 2)
                .unwrap()
                .as_u64()
                .unwrap()
        );
        tx_datum
            .get_nth_field_by_type("int", 100)
            .expect_err("should have returned an error");
    }
}
