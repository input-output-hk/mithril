use crate::entities::HexEncodedKey;

use hex::{FromHex, ToHex};
use serde::de::DeserializeOwned;
use serde::Serialize;

/// Encode key to hex helper
pub fn key_encode_hex<T>(from: T) -> Result<HexEncodedKey, String>
where
    T: Serialize,
{
    Ok(serde_json::to_string(&from)
        .map_err(|e| format!("can't convert to hex: {e}"))?
        .encode_hex::<String>())
}

/// Decode key from hex helper
pub fn key_decode_hex<T>(from: &HexEncodedKey) -> Result<T, String>
where
    T: DeserializeOwned,
{
    let from_vec = Vec::from_hex(from).map_err(|e| {
        format!("Key decode hex: can not turn hexadecimal '{from}' into bytes, error: {e}")
    })?;
    serde_json::from_slice(from_vec.as_slice()).map_err(|e| {
        format!(
            "Key decode hex: can not deserialize to type '{}' from binary JSON: error {e}",
            std::any::type_name::<T>()
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
