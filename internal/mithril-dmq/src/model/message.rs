use std::ops::{Deref, DerefMut};

use pallas_codec::minicbor::{Decode, Decoder, Encode, Encoder};
use pallas_network::miniprotocols::localmsgsubmission::DmqMsg;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

/// Wrapper for a DMQ message which can be serialized and deserialized.
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
        let raw_bytes = RawBytes({
            let mut e = Encoder::new(Vec::new());
            self.0.encode(&mut e, &mut ()).map_err(|e| {
                serde::ser::Error::custom(format!("DMQ message serialization error: {e}"))
            })?;
            Ok(e.into_writer())
        }?);

        raw_bytes.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for DmqMessage {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw_bytes = RawBytes::deserialize(deserializer)?;
        let res = DmqMsg::decode(&mut Decoder::new(&raw_bytes.0), &mut ())
            .map_err(|e| {
                serde::de::Error::custom(format!("DMQ message deserialization error: {e}"))
            })?
            .into();

        Ok(res)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dmq_message_serialize_deserialize() {
        let dmq_msg = DmqMsg {
            msg_id: vec![0, 1],
            msg_body: vec![0, 1, 2],
            kes_signature: vec![0, 1, 2, 3],
            kes_period: 10,
            operational_certificate: vec![0, 1, 2, 3, 4],
            cold_verification_key: vec![0, 1, 2, 3, 4, 5],
            expires_at: 100,
        };

        let dmq_message = DmqMessage::from(dmq_msg.clone());
        let serialized = bincode::serde::encode_to_vec(&dmq_message, bincode::config::standard())
            .expect("Serialization failed");

        let (deserialized, _) =
            bincode::serde::decode_from_slice(&serialized, bincode::config::standard())
                .expect("Deserialization failed");

        assert_eq!(dmq_message, deserialized);
        assert_eq!(dmq_message.0, dmq_msg);
    }
}
