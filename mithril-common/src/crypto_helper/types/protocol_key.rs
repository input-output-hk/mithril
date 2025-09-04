use std::{
    any::type_name,
    fs::{File, read_to_string},
    io::Write,
    ops::Deref,
    path::Path,
};

use anyhow::Context;
use serde::{Deserialize, Serialize, Serializer, de::DeserializeOwned};

use crate::StdResult;
use crate::crypto_helper::{TryFromBytes, TryToBytes, key_decode_hex, key_encode_hex};

/// A ProtocolKey is a wrapped that add Serialization capabilities.
///
/// When using serde to (de)serialize it will done using json hex (see [key_decode_hex] and
/// [key_encode_hex]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProtocolKey<T>
where
    T: Serialize + DeserializeOwned,
{
    pub(crate) key: T,
}

/// The codec used to serialize/deserialize a [ProtocolKey].
///
/// Encodes to json hex and decodes from json hex or bytes hex.
pub trait ProtocolKeyCodec<T: Serialize + DeserializeOwned + TryToBytes + TryFromBytes>:
    Sized
{
    /// Do the decoding of the given key
    fn decode_key(encoded: &str) -> StdResult<ProtocolKey<T>> {
        match ProtocolKey::from_json_hex(encoded) {
            Ok(res) => Ok(res),
            Err(_) => ProtocolKey::from_bytes_hex(encoded),
        }
    }

    /// Do the encoding of the given key
    fn encode_key(key: &T) -> StdResult<String> {
        ProtocolKey::key_to_json_hex(key)
    }
}

impl<T> ProtocolKey<T>
where
    T: Serialize + DeserializeOwned + TryToBytes + TryFromBytes,
{
    /// Create a ProtocolKey from the given key
    pub fn new(key: T) -> Self {
        Self { key }
    }

    /// Create an instance from a JSON hex representation
    pub fn from_json_hex(hex_string: &str) -> StdResult<Self> {
        let key = key_decode_hex::<T>(hex_string).with_context(|| {
            format!(
                "Could not deserialize a ProtocolKey from JSON hex string. Inner key type: {}",
                type_name::<T>()
            )
        })?;

        Ok(Self { key })
    }

    /// Create a JSON hex representation of the key
    pub fn to_json_hex(&self) -> StdResult<String> {
        Self::key_to_json_hex(&self.key)
    }

    /// Create a JSON hash representation of the given key
    pub fn key_to_json_hex(key: &T) -> StdResult<String> {
        key_encode_hex(key).with_context(|| {
            format!(
                "Could not serialize a ProtocolKey to JSON hex key string. Inner key type: {}",
                type_name::<T>()
            )
        })
    }

    /// Write to file in JSON hex format
    pub fn write_json_hex_to_file(&self, path: &Path) -> StdResult<()> {
        let key_payload = self.to_json_hex()?;
        let mut key_file = File::create(path)?;
        key_file.write_all(key_payload.as_bytes())?;

        Ok(())
    }

    /// Read from file in JSON hex format
    pub fn read_json_hex_from_file(path: &Path) -> StdResult<Self> {
        let key_bytes = read_to_string(path)?;

        Self::from_json_hex(&key_bytes)
    }

    /// Create an instance from bytes
    pub fn from_bytes(bytes: &[u8]) -> StdResult<Self> {
        let key = T::try_from_bytes(bytes).with_context(|| {
            format!(
                "Could not deserialize a ProtocolKey from bytes. Inner key type: {}",
                type_name::<T>()
            )
        })?;

        Ok(Self { key })
    }

    /// Create an instance from a bytes hex representation
    pub fn from_bytes_hex(hex_string: &str) -> StdResult<Self> {
        let key = T::try_from_bytes_hex(hex_string).with_context(|| {
            format!(
                "Could not deserialize a ProtocolKey from bytes hex string. Inner key type: {}",
                type_name::<T>()
            )
        })?;

        Ok(Self { key })
    }

    /// Create a bytes hex representation of the key
    pub fn to_bytes_hex(&self) -> StdResult<String> {
        Self::key_to_bytes_hex(&self.key)
    }

    /// Create a bytes hex representation of the given key
    pub fn key_to_bytes_hex(key: &T) -> StdResult<String> {
        key.to_bytes_hex()
    }

    /// Consume self and return the inner key
    pub fn into_inner(self) -> T {
        self.key
    }
}

impl<T> Deref for ProtocolKey<T>
where
    T: Serialize + DeserializeOwned,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.key
    }
}

impl<T> Copy for ProtocolKey<T> where T: Copy + Serialize + DeserializeOwned {}

impl<T> Serialize for ProtocolKey<T>
where
    T: ProtocolKeyCodec<T> + Serialize + DeserializeOwned + TryToBytes + TryFromBytes,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use serde::ser::Error;
        let encoded = &T::encode_key(&self.key).map_err(Error::custom)?;

        encoded.serialize(serializer)
    }
}

impl<'de, T> Deserialize<'de> for ProtocolKey<T>
where
    T: ProtocolKeyCodec<T> + Serialize + DeserializeOwned + TryToBytes + TryFromBytes,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;
        let string = String::deserialize(deserializer)?;

        T::decode_key(&string).map_err(Error::custom)
    }
}

impl<T> TryFrom<String> for ProtocolKey<T>
where
    T: ProtocolKeyCodec<T> + Serialize + DeserializeOwned + TryToBytes + TryFromBytes,
{
    type Error = anyhow::Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        T::decode_key(&value)
    }
}

impl<T> TryFrom<&str> for ProtocolKey<T>
where
    T: ProtocolKeyCodec<T> + Serialize + DeserializeOwned + TryToBytes + TryFromBytes,
{
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        T::decode_key(value)
    }
}

impl<T> TryFrom<ProtocolKey<T>> for String
where
    T: ProtocolKeyCodec<T> + Serialize + DeserializeOwned + TryToBytes + TryFromBytes,
{
    type Error = anyhow::Error;

    fn try_from(value: ProtocolKey<T>) -> Result<Self, Self::Error> {
        T::encode_key(&value.key)
    }
}

impl<T> TryFrom<&ProtocolKey<T>> for String
where
    T: ProtocolKeyCodec<T> + Serialize + DeserializeOwned + TryToBytes + TryFromBytes,
{
    type Error = anyhow::Error;

    fn try_from(value: &ProtocolKey<T>) -> Result<Self, Self::Error> {
        T::encode_key(&value.key)
    }
}

/// Macro to batch define a [ProtocolKeyCodec] implementation and conversions From<ProtocolKey> / To
/// the given type and vis versa.
macro_rules! impl_codec_and_type_conversions_for_protocol_key {
    (json_hex_codec => $($key_type:ty),+) => {
        $(
            impl crate::crypto_helper::ProtocolKeyCodec<$key_type> for $key_type {}

            impl From<ProtocolKey<$key_type >> for $key_type {
                fn from(value: ProtocolKey<$key_type>) -> Self {
                    value.key
                }
            }

            impl From<$key_type> for ProtocolKey<$key_type> {
                fn from(value: $key_type) -> Self {
                    Self::new(value)
                }
            }
        )*
    };
    (bytes_hex_codec => $($key_type:ty),+) => {
        $(
            impl crate::crypto_helper::ProtocolKeyCodec<$key_type> for $key_type {
                fn decode_key(encoded: &str) -> crate::StdResult<ProtocolKey<$key_type>> {
                    match ProtocolKey::from_bytes_hex(encoded) {
                        Ok(res) => Ok(res),
                        Err(_) => ProtocolKey::from_json_hex(encoded),
                    }
                }

                fn encode_key(key: &$key_type) -> crate::StdResult<String> {
                    ProtocolKey::key_to_bytes_hex(key)
                }
            }

            impl From<ProtocolKey<$key_type >> for $key_type {
                fn from(value: ProtocolKey<$key_type>) -> Self {
                    value.key
                }
            }

            impl From<$key_type> for ProtocolKey<$key_type> {
                fn from(value: $key_type) -> Self {
                    Self::new(value)
                }
            }
        )*
    };
    (no_default_codec => $($key_type:ty),+) => {
        $(
            impl From<ProtocolKey<$key_type >> for $key_type {
                fn from(value: ProtocolKey<$key_type>) -> Self {
                    value.key
                }
            }

            impl From<$key_type> for ProtocolKey<$key_type> {
                fn from(value: $key_type) -> Self {
                    Self::new(value)
                }
            }
        )*
    };
}

#[cfg(test)]
mod test {
    use crate::{
        crypto_helper::ProtocolKey,
        test::{TempDir, double::fake_keys},
    };
    use mithril_stm::VerificationKeyProofOfPossession;
    use serde::{Deserialize, Serialize};

    static VERIFICATION_KEY_JSON_HEX: &str = fake_keys::signer_verification_key()[0];

    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
    struct Container {
        protocol_key: ProtocolKey<VerificationKeyProofOfPossession>,
    }

    #[test]
    fn serializing_directly_does_not_change_the_string() {
        let key: ProtocolKey<VerificationKeyProofOfPossession> =
            VERIFICATION_KEY_JSON_HEX.try_into().unwrap();
        let serialized = serde_json::to_string(&key).expect("Serialization should not fail");

        // Note: in json strings are enclosed in quotes
        assert_eq!(format!("\"{VERIFICATION_KEY_JSON_HEX}\""), serialized);
    }

    #[test]
    fn serialize_deserialize_are_the_same_object() {
        let container = Container {
            protocol_key: VERIFICATION_KEY_JSON_HEX.try_into().unwrap(),
        };
        let serialized = serde_json::to_string(&container).expect("Serialization should not fail");
        let deserialized: Container = serde_json::from_str(&serialized).unwrap();

        assert_eq!(container, deserialized);
    }

    #[test]
    fn can_serialize_a_struct_containing_a_verification_key() {
        let container = Container {
            protocol_key: VERIFICATION_KEY_JSON_HEX.try_into().unwrap(),
        };
        let expected = format!(r#"{{"protocol_key":"{VERIFICATION_KEY_JSON_HEX}"}}"#);

        let serialized = serde_json::to_string(&container).expect("Serialization should not fail");
        assert_eq!(expected, serialized);
    }

    #[test]
    fn can_deserialize_a_struct_containing_a_json_hex_verification_key() {
        let expected = Container {
            protocol_key: VERIFICATION_KEY_JSON_HEX.try_into().unwrap(),
        };
        let serialized = format!(r#"{{"protocol_key":"{VERIFICATION_KEY_JSON_HEX}"}}"#);

        let deserialized: Container =
            serde_json::from_str(&serialized).expect("Deserialization should not fail");
        assert_eq!(expected, deserialized);
    }

    #[test]
    fn can_deserialize_a_struct_containing_a_bytes_hex_verification_key() {
        let verification_key: ProtocolKey<VerificationKeyProofOfPossession> =
            VERIFICATION_KEY_JSON_HEX
                .try_into()
                .expect("Failed to convert verification key");
        let verification_key_bytes_hex = verification_key
            .to_bytes_hex()
            .expect("Failed to convert verification key to bytes hex");
        let expected = Container {
            protocol_key: verification_key,
        };
        let serialized = format!(r#"{{"protocol_key":"{verification_key_bytes_hex}"}}"#);

        let deserialized: Container =
            serde_json::from_str(&serialized).expect("Deserialization should not fail");
        assert_eq!(expected, deserialized);
    }

    #[test]
    fn can_read_and_write_to_file_a_verification_key() {
        let expected_key: ProtocolKey<VerificationKeyProofOfPossession> =
            VERIFICATION_KEY_JSON_HEX.try_into().unwrap();
        let key_path = TempDir::create(
            "protocol_key",
            "can_read_and_write_to_file_a_verification_key",
        )
        .join("key.out");

        expected_key
            .write_json_hex_to_file(&key_path)
            .expect("Writing to file should not fail");
        let read_key =
            ProtocolKey::<VerificationKeyProofOfPossession>::read_json_hex_from_file(&key_path)
                .expect("Reading from file should not fail");

        assert_eq!(expected_key, read_key);
    }
}
