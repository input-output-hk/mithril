use anyhow::{anyhow, Context, Result as StdResult};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use std::any::type_name;

use crate::crypto_helper::{key_decode_hex, key_encode_hex};

/// A ProtocolKey is a wrapped that add Serialization capabilities.
///
/// When using serde to (de)serialize it will done using json hex (see [key_decode_hex] and
/// [key_encode_hex]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProtocolKey<T>
where
    T: Serialize + DeserializeOwned,
{
    key: T,
}

impl<T> ProtocolKey<T>
where
    T: Serialize + DeserializeOwned,
{
    /// Create a ProtocolKey from the given key
    pub fn new(key: T) -> Self {
        Self { key }
    }

    /// Get the inner key
    pub fn key(&self) -> &T {
        &self.key
    }

    /// Create an instance from a JSON hex representation
    pub fn from_json_hex(hex_string: &str) -> StdResult<Self> {
        let key = key_decode_hex::<T>(&hex_string.to_owned())
            .map_err(|e| anyhow!(e))
            .with_context(|| {
                format!(
                    "Could not deserialize a ProtocolKey from JSON hex string. Inner key type: {}",
                    type_name::<T>()
                )
            })?;

        Ok(Self { key })
    }

    /// Create a JSON hash representation of the key
    pub fn to_json_hex(&self) -> StdResult<String> {
        key_encode_hex(&self.key)
            .map_err(|e| anyhow!(e))
            .with_context(|| {
                format!(
                    "Could not serialize a ProtocolKey to JSON hex key string. Inner key type: {}",
                    type_name::<T>()
                )
            })
    }
}

impl<T> Serialize for ProtocolKey<T>
where
    T: Serialize + DeserializeOwned,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::Error;
        let hex = self.to_json_hex().map_err(Error::custom)?;

        hex.serialize(serializer)
    }
}

impl<'de, T> Deserialize<'de> for ProtocolKey<T>
where
    T: Serialize + DeserializeOwned,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;
        let string = String::deserialize(deserializer)?;

        Self::from_json_hex(&string).map_err(Error::custom)
    }
}

impl<T> TryFrom<String> for ProtocolKey<T>
where
    T: Serialize + DeserializeOwned,
{
    type Error = anyhow::Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::from_json_hex(&value)
    }
}

impl<T> TryFrom<&str> for ProtocolKey<T>
where
    T: Serialize + DeserializeOwned,
{
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::from_json_hex(value)
    }
}

impl<T> TryFrom<ProtocolKey<T>> for String
where
    T: Serialize + DeserializeOwned,
{
    type Error = anyhow::Error;

    fn try_from(value: ProtocolKey<T>) -> Result<Self, Self::Error> {
        value.to_json_hex()
    }
}

impl<T> TryFrom<&ProtocolKey<T>> for String
where
    T: Serialize + DeserializeOwned,
{
    type Error = anyhow::Error;

    fn try_from(value: &ProtocolKey<T>) -> Result<Self, Self::Error> {
        value.to_json_hex()
    }
}
