use anyhow::{anyhow, Context, Result as StdResult};
use mithril_stm::stm::StmVerificationKeyPoP;
use serde::{Deserialize, Serialize};

use super::{key_decode_hex, key_encode_hex};

/// Wrapper of [MithrilStm:StmVerificationKeyPoP](type@mithril_stm::stm::StmVerificationKeyPoP)
/// to add serialization utilities.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProtocolSignerVerificationKey {
    #[serde(flatten)]
    verification_key: StmVerificationKeyPoP,
}

impl From<StmVerificationKeyPoP> for ProtocolSignerVerificationKey {
    fn from(value: StmVerificationKeyPoP) -> Self {
        Self {
            verification_key: value,
        }
    }
}

impl From<ProtocolSignerVerificationKey> for StmVerificationKeyPoP {
    /// Cast from the Stm representation
    fn from(val: ProtocolSignerVerificationKey) -> Self {
        val.verification_key
    }
}

impl TryInto<ProtocolSignerVerificationKey> for &str {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<ProtocolSignerVerificationKey, Self::Error> {
        ProtocolSignerVerificationKey::from_json_hex(self)
    }
}

impl TryInto<ProtocolSignerVerificationKey> for String {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<ProtocolSignerVerificationKey, Self::Error> {
        ProtocolSignerVerificationKey::from_json_hex(&self)
    }
}

impl ProtocolSignerVerificationKey {
    /// create an instance from a JSON hash representation
    pub fn from_json_hex(hex_string: &str) -> StdResult<Self> {
        key_decode_hex(&hex_string.to_owned())
            .map_err(|e| anyhow!(e))
            .with_context(|| {
                "Could not build a ProtocolSignerVerificationKey from hexadecimal key string."
            })
    }

    /// create a JSON hash representation of the verificationkey
    pub fn to_json_hex(&self) -> StdResult<String> {
        key_encode_hex(self.clone())
            .map_err(|e| anyhow!(e))
            .with_context(|| {
                "Could not export a ProtocolSignerVerificationKey to hexadecimal key string."
            })
    }

    /// Output the key's bytes in memory
    pub fn to_bytes(&self) -> [u8; 192] {
        self.verification_key.to_bytes()
    }
}
