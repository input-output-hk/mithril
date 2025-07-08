use std::ops::{Deref, DerefMut};

use pallas_network::miniprotocols::localmsgsubmission::DmqMsg;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

/// Type alias for a DMQ message.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DmqMessage(DmqMsg);

#[derive(Serialize, Deserialize)]
struct RawBytes(#[serde(with = "serde_bytes")] Vec<u8>);

impl Deref for DmqMessage {
    type Target = DmqMsg;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for DmqMessage {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<DmqMsg> for DmqMessage {
    fn from(msg: DmqMsg) -> Self {
        Self(msg)
    }
}

impl From<DmqMessage> for DmqMsg {
    fn from(msg: DmqMessage) -> Self {
        msg.0
    }
}

impl Serialize for DmqMessage {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let raw_bytes = RawBytes(
            bincode::serde::encode_to_vec(self, bincode::config::standard()).map_err(|e| {
                serde::ser::Error::custom(format!("DMQ message serialization error: {e}"))
            })?,
        );

        raw_bytes.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for DmqMessage {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw_bytes = RawBytes::deserialize(deserializer)?;
        let (res, _) =
            bincode::serde::decode_from_slice::<Self, _>(&raw_bytes.0, bincode::config::standard())
                .map_err(|e| {
                    serde::de::Error::custom(format!("DMQ message deserialization error: {e}"))
                })?;

        Ok(res)
    }
}
