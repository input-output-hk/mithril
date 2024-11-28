use crate::entities::{HexEncodedKey, HexEncodedKeySlice};

use hex::{FromHex, ToHex};
use serde::de::DeserializeOwned;
use serde::Serialize;
use thiserror::Error;

use anyhow::anyhow;
use bech32::{self, Bech32, Hrp};

use crate::StdResult;

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

/// Encode to bech32 given Human Readable Part (hrp) and data
pub fn encode_bech32(human_readable_part: &str, data: &[u8]) -> StdResult<String> {
    let human_readable_part = Hrp::parse(human_readable_part).map_err(|e| anyhow!(e))?;
    bech32::encode::<Bech32>(human_readable_part, data).map_err(|e| anyhow!(e))
}

#[cfg(test)]
mod tests {
    use hex::FromHex;
    use serde::{Deserialize, Serialize};

    use super::{encode_bech32, key_decode_hex, key_encode_hex};

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

    #[test]
    fn test_bech32_encode() {
        let hrp = "pool";
        let data =
            Vec::from_hex("edfa208d441511f9595ba80e8f3a7b07b6a80cbc9dda9d8e9d1dc039").unwrap();
        let encoded_data = encode_bech32(hrp, &data).unwrap();
        let expected_encoded_data =
            "pool1ahazpr2yz5gljk2m4q8g7wnmq7m2sr9unhdfmr5arhqrjnntwdz".to_string();

        assert_eq!(expected_encoded_data, encoded_data);
    }
}
