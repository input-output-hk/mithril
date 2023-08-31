use crate::entities::{HexEncodedKey, HexEncodedKeySlice};

use hex::{FromHex, ToHex};
use serde::de::DeserializeOwned;
use serde::Serialize;
use thiserror::Error;

/// Error raised when the encoding or decoding fails
#[derive(Error, Debug)]
#[error("Codec error: {msg}")]
pub struct CodecError {
    msg: String,
    #[source]
    source: anyhow::Error,
}

impl CodecError {
    /// [CodecError] factory.
    pub fn new(msg: &str, source: anyhow::Error) -> Self {
        Self {
            msg: msg.to_string(),
            source,
        }
    }
}

/// Encode key to hex helper
pub fn key_encode_hex<T>(from: T) -> Result<HexEncodedKey, CodecError>
where
    T: Serialize,
{
    Ok(serde_json::to_string(&from)
        .map_err(|e| CodecError::new("Key encode hex: can not convert to hex", e.into()))?
        .encode_hex::<String>())
}

/// Decode key from hex helper
pub fn key_decode_hex<T>(from: HexEncodedKeySlice) -> Result<T, CodecError>
where
    T: DeserializeOwned,
{
    let from_vec = Vec::from_hex(from).map_err(|e| {
        CodecError::new(
            "Key decode hex: can not turn hexadecimal value into bytes",
            e.into(),
        )
    })?;
    serde_json::from_slice(from_vec.as_slice()).map_err(|e| {
        CodecError::new(
            &format!(
                "Key decode hex: can not deserialize to type '{}' from binary JSON",
                std::any::type_name::<T>()
            ),
            e.into(),
        )
    })
}

#[cfg(test)]
pub mod tests {
    use serde::{Deserialize, Serialize};

    use super::{key_decode_hex, key_encode_hex};

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct TestSerialize {
        inner_string: String,
    }

    #[test]
    fn test_key_encode_decode_hex() {
        let test_to_serialize = TestSerialize {
            inner_string: "my inner string".to_string(),
        };
        let test_to_serialize_hex =
            key_encode_hex(&test_to_serialize).expect("unexpected hex encoding error");
        let test_to_serialize_restored =
            key_decode_hex(&test_to_serialize_hex).expect("unexpected hex decoding error");
        assert_eq!(test_to_serialize, test_to_serialize_restored);
    }
}
