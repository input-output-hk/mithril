use anyhow::{anyhow, Context, Result as StdResult};
use mithril_stm::stm::StmSig;
use serde::{Deserialize, Serialize};

use crate::crypto_helper::{key_decode_hex, key_encode_hex};

/// Single Signature
#[derive(Debug, Clone, Eq)]
pub struct ProtocolSingleSignature {
    signature: StmSig,
}

impl ProtocolSingleSignature {
    /// Create a signature from a JSON Hex representation
    pub fn from_json_hex(hex_string: &str) -> StdResult<Self> {
        let signature = key_decode_hex::<StmSig>(&hex_string.to_owned())
            .map_err(|e| anyhow!(e))
            .with_context(|| {
                "Could not deserialize a ProtocolSingleSignature from JSON hex string."
            })?;

        Ok(Self { signature })
    }

    /// Dump a JSON Hex representation of the signature
    pub fn to_json_hex(&self) -> StdResult<String> {
        key_encode_hex(&self.signature)
            .map_err(|e| anyhow!(e))
            .with_context(|| {
                "Could not serialize a ProtocolSingleSignature to JSON hex key string."
            })
    }
}

impl From<ProtocolSingleSignature> for StmSig {
    fn from(value: ProtocolSingleSignature) -> Self {
        value.signature
    }
}

impl From<StmSig> for ProtocolSingleSignature {
    fn from(value: StmSig) -> Self {
        ProtocolSingleSignature { signature: value }
    }
}

impl TryFrom<String> for ProtocolSingleSignature {
    type Error = anyhow::Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        ProtocolSingleSignature::from_json_hex(&value)
    }
}

impl TryInto<String> for ProtocolSingleSignature {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<String, Self::Error> {
        self.to_json_hex()
    }
}

impl Serialize for ProtocolSingleSignature {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::Error;
        let hex = self.to_json_hex().map_err(Error::custom)?;

        hex.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for ProtocolSingleSignature {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;
        let string = String::deserialize(deserializer)?;

        Self::from_json_hex(&string).map_err(Error::custom)
    }
}

impl PartialEq for ProtocolSingleSignature {
    fn eq(&self, other: &Self) -> bool {
        self.to_json_hex().unwrap() == other.to_json_hex().unwrap()
    }
}
