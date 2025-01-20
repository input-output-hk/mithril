use std::{
    any::type_name,
    fs::{read_to_string, File},
    io::Write,
    ops::Deref,
    path::Path,
};

use anyhow::Context;
use serde::{de::DeserializeOwned, Deserialize, Serialize, Serializer};

use crate::crypto_helper::{key_decode_hex, key_encode_hex};
use crate::StdResult;

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
/// Default to json hex.
pub trait ProtocolKeyCodec<T: Serialize + DeserializeOwned>: Sized {
    /// Do the decoding of the given key
    fn decode_key(encoded: &str) -> StdResult<ProtocolKey<T>> {
        ProtocolKey::from_json_hex(encoded)
    }

    /// Do the encoding of the given key
    fn encode_key(key: &T) -> StdResult<String> {
        ProtocolKey::key_to_json_hex(key)
    }
}

impl<T> ProtocolKey<T>
where
    T: Serialize + DeserializeOwned,
{
    /// Create a ProtocolKey from the given key
    pub fn new(key: T) -> Self {
        Self { key }
    }

    /// Create an instance from a JSON hex representation
    pub fn from_json_hex(hex_string: &str) -> StdResult<Self> {
        let key = key_decode_hex::<T>(hex_string.trim()).with_context(|| {
            format!(
                "Could not deserialize a ProtocolKey from JSON hex string. Inner key type: {}",
                type_name::<T>()
            )
        })?;

        Ok(Self { key })
    }

    /// Create a JSON hash representation of the key
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
    T: ProtocolKeyCodec<T> + Serialize + DeserializeOwned,
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
    T: ProtocolKeyCodec<T> + Serialize + DeserializeOwned,
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
    T: ProtocolKeyCodec<T> + Serialize + DeserializeOwned,
{
    type Error = anyhow::Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        T::decode_key(&value)
    }
}

impl<T> TryFrom<&str> for ProtocolKey<T>
where
    T: ProtocolKeyCodec<T> + Serialize + DeserializeOwned,
{
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        T::decode_key(value)
    }
}

impl<T> TryFrom<ProtocolKey<T>> for String
where
    T: ProtocolKeyCodec<T> + Serialize + DeserializeOwned,
{
    type Error = anyhow::Error;

    fn try_from(value: ProtocolKey<T>) -> Result<Self, Self::Error> {
        T::encode_key(&value.key)
    }
}

impl<T> TryFrom<&ProtocolKey<T>> for String
where
    T: ProtocolKeyCodec<T> + Serialize + DeserializeOwned,
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
    use std::{io::Write, path::Path};

    use crate::{
        crypto_helper::ProtocolKey,
        test_utils::{fake_keys, TempDir},
        StdResult,
    };
    use mithril_stm::stm::StmVerificationKeyPoP;
    use serde::{Deserialize, Serialize};

    static VERIFICATION_KEY: &str = fake_keys::signer_verification_key()[0];

    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
    struct Container {
        protocol_key: ProtocolKey<StmVerificationKeyPoP>,
    }

    #[test]
    fn serializing_directly_does_not_change_the_string() {
        let key: ProtocolKey<StmVerificationKeyPoP> = VERIFICATION_KEY.try_into().unwrap();
        let serialized = serde_json::to_string(&key).expect("Serialization should not fail");

        // Note: in json strings are enclosed in quotes
        assert_eq!(format!("\"{VERIFICATION_KEY}\""), serialized);
    }

    #[test]
    fn serialize_deserialize_are_the_same_object() {
        let container = Container {
            protocol_key: VERIFICATION_KEY.try_into().unwrap(),
        };
        let serialized = serde_json::to_string(&container).expect("Serialization should not fail");
        let deserialized: Container = serde_json::from_str(&serialized).unwrap();

        assert_eq!(container, deserialized);
    }

    #[test]
    fn can_serialize_a_struct_containing_a_verification_key() {
        let container = Container {
            protocol_key: VERIFICATION_KEY.try_into().unwrap(),
        };
        let expected = format!(r#"{{"protocol_key":"{VERIFICATION_KEY}"}}"#);

        let serialized = serde_json::to_string(&container).expect("Serialization should not fail");
        assert_eq!(expected, serialized);
    }

    #[test]
    fn can_deserialize_a_struct_containing_a_verification_key() {
        let expected = Container {
            protocol_key: VERIFICATION_KEY.try_into().unwrap(),
        };
        let serialized = format!(r#"{{"protocol_key":"{VERIFICATION_KEY}"}}"#);

        let deserialized: Container =
            serde_json::from_str(&serialized).expect("Deserialization should not fail");
        assert_eq!(expected, deserialized);
    }

    #[test]
    fn can_read_and_write_to_file_a_verification_key() {
        let expected_key: ProtocolKey<StmVerificationKeyPoP> = VERIFICATION_KEY.try_into().unwrap();
        let key_path = TempDir::create(
            "protocol_key",
            "can_read_and_write_to_file_a_verification_key",
        )
        .join("key.out");

        expected_key
            .write_json_hex_to_file(&key_path)
            .expect("Writing to file should not fail");
        let read_key = ProtocolKey::<StmVerificationKeyPoP>::read_json_hex_from_file(&key_path)
            .expect("Reading from file should not fail");

        assert_eq!(expected_key, read_key);
    }

    #[test]
    fn can_read_a_verification_key_from_file_with_trailing_whitespaces() {
        fn add_space_to_file(key_path: &Path) -> StdResult<()> {
            let mut key_file = std::fs::OpenOptions::new().append(true).open(key_path)?;
            key_file.write_all(b"\n")?;

            Ok(())
        }

        let expected_key: ProtocolKey<StmVerificationKeyPoP> = VERIFICATION_KEY.try_into().unwrap();
        let key_path = TempDir::create(
            "protocol_key",
            "can_read_a_verification_key_from_file_with_trailing_whitespaces",
        )
        .join("key.out");

        expected_key
            .write_json_hex_to_file(&key_path)
            .expect("Writing to file should not fail");
        add_space_to_file(&key_path).expect("Adding space to file should not fail");
        let read_key = ProtocolKey::<StmVerificationKeyPoP>::read_json_hex_from_file(&key_path)
            .expect("Reading from file should not fail");

        assert_eq!(expected_key, read_key);
    }
}
