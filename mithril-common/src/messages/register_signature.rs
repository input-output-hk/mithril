use std::fmt::{Debug, Formatter};

use anyhow::anyhow;
use serde::{Deserialize, Serialize};

use crate::{
    crypto_helper::{ProtocolSingleSignature, TryFromBytes, TryToBytes},
    entities::{HexEncodedSingleSignature, LotteryIndex, PartyId, SignedEntityType},
    StdResult,
};

#[cfg(any(test, feature = "test_tools"))]
use crate::test_utils::fake_keys;

/// Message structure to register single signature through HTTP.
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RegisterSignatureMessageHttp {
    /// Signed entity type
    #[serde(rename = "entity_type")]
    pub signed_entity_type: SignedEntityType,

    /// The unique identifier of the signer
    pub party_id: PartyId,

    /// The single signature of the digest
    pub signature: HexEncodedSingleSignature,

    /// The indexes of the won lotteries that lead to the single signature
    #[serde(rename = "indexes")]
    pub won_indexes: Vec<LotteryIndex>,

    /// Message that is signed by the signer
    ///
    /// Used to buffer the signature for later if the aggregator has yet to create an open message
    /// for the signed entity type.
    pub signed_message: String,
}

impl RegisterSignatureMessageHttp {
    cfg_test_tools! {
        /// Return a dummy test entity (test-only).
        pub fn dummy() -> Self {
            use crate::entities::Epoch;
            Self {
                signed_entity_type: SignedEntityType::MithrilStakeDistribution(Epoch(5)),
                party_id: "party_id".to_string(),
                signature: fake_keys::single_signature()[0].to_string(),
                won_indexes: vec![1, 3],
                signed_message: "6a7e737c312972d2346b65ac3075696e04286d046dddaf8004121e3d5e27cc0d".to_string(),
            }
        }
    }
}

impl Debug for RegisterSignatureMessageHttp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let is_pretty_printing = f.alternate();
        let mut debug = f.debug_struct("RegisterSignatureMessageHttp");
        debug
            .field(
                "signed_entity_type",
                &format_args!("{:?}", self.signed_entity_type),
            )
            .field("party_id", &self.party_id)
            .field("won_indexes", &format_args!("{:?}", self.won_indexes));

        match is_pretty_printing {
            true => debug.field("signature", &self.signature).finish(),
            false => debug.finish_non_exhaustive(),
        }
    }
}

/// Message structure to register single signature through the DMQ network.
#[derive(Clone, PartialEq, Eq)]
pub struct RegisterSignatureMessageDmq {
    /// Signed entity type
    pub signed_entity_type: SignedEntityType,

    /// The single signature.
    pub signature: ProtocolSingleSignature,
}

impl RegisterSignatureMessageDmq {
    cfg_test_tools! {
        /// Return a dummy test entity (test-only).
        pub fn dummy() -> Self {
            use crate::entities::Epoch;
            Self {
                signed_entity_type: SignedEntityType::MithrilStakeDistribution(Epoch(5)),
                signature: fake_keys::single_signature()[0].try_into().unwrap(),
            }
        }
    }

    /// Convert an `RegisterSignatureMessageDmq` into bytes
    ///
    /// # Layout
    /// * Signed entity type length (u16)
    /// * Signed entity type
    /// * Protocol signature length (u32)
    /// * Protocol signature
    pub fn try_to_bytes_vec(&self) -> StdResult<Vec<u8>> {
        let mut bytes = Vec::new();

        let signed_entity_bytes = self.signed_entity_type.to_bytes_vec()?;
        bytes.extend_from_slice(&(signed_entity_bytes.len() as u16).to_be_bytes());
        bytes.extend_from_slice(&signed_entity_bytes);

        let signature_bytes = self.signature.to_bytes_vec()?;
        bytes.extend_from_slice(&(signature_bytes.len() as u32).to_be_bytes());
        bytes.extend_from_slice(&signature_bytes);

        Ok(bytes)
    }

    /// Extract a `RegisterSignatureMessageDmq` from bytes.
    pub fn try_from_bytes_vec(bytes: &[u8]) -> StdResult<Self> {
        let mut bytes_index = 0;

        let mut u16_bytes = [0u8; 2];
        u16_bytes.copy_from_slice(
            bytes
                .get(bytes_index..bytes_index + 2)
                .ok_or(anyhow!("Failed to read `Signed entity type length` bytes"))?,
        );
        let signed_entity_bytes_length = u16::from_be_bytes(u16_bytes) as usize;
        bytes_index += 2;

        let signed_entity_bytes = bytes
            .get(bytes_index..bytes_index + signed_entity_bytes_length)
            .ok_or(anyhow!("Failed to read `Signed entity type` bytes"))?;
        let signed_entity_type = SignedEntityType::try_from_bytes(signed_entity_bytes)?;
        bytes_index += signed_entity_bytes_length;

        let mut u32_bytes = [0u8; 4];
        u32_bytes.copy_from_slice(
            bytes
                .get(bytes_index..bytes_index + 4)
                .ok_or(anyhow!("Failed to read `Signature length` bytes"))?,
        );
        let signature_bytes_length = u32::from_be_bytes(u32_bytes) as usize;
        bytes_index += 4;

        let signature_bytes = bytes
            .get(bytes_index..bytes_index + signature_bytes_length)
            .ok_or(anyhow!("Failed to read `Signature` bytes"))?;
        let signature = ProtocolSingleSignature::from_bytes(signature_bytes)?;

        Ok(Self {
            signed_entity_type,
            signature,
        })
    }
}

impl Debug for RegisterSignatureMessageDmq {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let is_pretty_printing = f.alternate();
        let mut debug = f.debug_struct("RegisterSignatureMessageDmq");
        debug.field(
            "signed_entity_type",
            &format_args!("{:?}", self.signed_entity_type),
        );

        match is_pretty_printing {
            true => debug.field("signature", &self.signature).finish(),
            false => debug.finish_non_exhaustive(),
        }
    }
}

impl TryFromBytes for RegisterSignatureMessageDmq {
    fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
        Self::try_from_bytes_vec(bytes)
    }
}

impl TryToBytes for RegisterSignatureMessageDmq {
    fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
        self.try_to_bytes_vec()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod golden_http_message {
        use crate::entities::{CardanoDbBeacon, Epoch};

        use super::*;

        const CURRENT_JSON: &str = r#"{
        "entity_type": {
            "CardanoImmutableFilesFull": {
                "epoch": 10,
                "immutable_file_number": 1728
            }
        },
        "party_id": "party_id",
        "signature": "7b227369676d61223a5b3133302c3137372c31352c3232392c32342c3235312c3234372c3137312c3139362c3231302c3134332c3131332c38362c3138392c39322c35362c3131322c33332c3139332c3231322c35342c3231342c32382c3231362c3232372c3137332c3130302c3132372c3137382c34302c39382c38372c32392c3138312c3235352c3131312c3135372c3232342c3233352c34362c3130302c3136392c3233322c3138392c3235322c38322c3133392c33365d2c22696e6465786573223a5b302c312c332c342c362c382c392c31302c31312c31322c31342c31382c32312c32322c32332c32352c32362c32372c33302c33332c33342c33382c34312c34332c35302c35382c35392c36302c36312c36322c36372c36392c37312c37332c37352c37362c37372c38312c38322c38332c38342c39302c39312c39322c39332c39372c39385d2c227369676e65725f696e646578223a327d",
        "indexes": [1, 3],
        "signed_message": "6a7e737c312972d2346b65ac3075696e04286d046dddaf8004121e3d5e27cc0d"
    }"#;

        fn golden_message_current() -> RegisterSignatureMessageHttp {
            RegisterSignatureMessageHttp {
            signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(
                CardanoDbBeacon::new(*Epoch(10), 1728),
            ),
            party_id: "party_id".to_string(),
            signature: "7b227369676d61223a5b3133302c3137372c31352c3232392c32342c3235312c3234372c3137312c3139362c3231302c3134332c3131332c38362c3138392c39322c35362c3131322c33332c3139332c3231322c35342c3231342c32382c3231362c3232372c3137332c3130302c3132372c3137382c34302c39382c38372c32392c3138312c3235352c3131312c3135372c3232342c3233352c34362c3130302c3136392c3233322c3138392c3235322c38322c3133392c33365d2c22696e6465786573223a5b302c312c332c342c362c382c392c31302c31312c31322c31342c31382c32312c32322c32332c32352c32362c32372c33302c33332c33342c33382c34312c34332c35302c35382c35392c36302c36312c36322c36372c36392c37312c37332c37352c37362c37372c38312c38322c38332c38342c39302c39312c39322c39332c39372c39385d2c227369676e65725f696e646578223a327d".to_string(),
            won_indexes: vec![1, 3],
            signed_message: "6a7e737c312972d2346b65ac3075696e04286d046dddaf8004121e3d5e27cc0d".to_string(),
        }
        }

        #[test]
        fn test_current_json_deserialized_into_current_message() {
            let json = CURRENT_JSON;
            let message: RegisterSignatureMessageHttp = serde_json::from_str(json).unwrap();

            assert_eq!(golden_message_current(), message);
        }

        #[test]
        fn test_json_until_open_api_0_1_45_deserialized_into_current_message() {
            let json = r#"{
            "entity_type": {
                "CardanoImmutableFilesFull": {
                    "network": "testnet",
                    "epoch": 10,
                    "immutable_file_number": 1728
                }
            },
            "party_id": "party_id",
            "signature": "7b227369676d61223a5b3133302c3137372c31352c3232392c32342c3235312c3234372c3137312c3139362c3231302c3134332c3131332c38362c3138392c39322c35362c3131322c33332c3139332c3231322c35342c3231342c32382c3231362c3232372c3137332c3130302c3132372c3137382c34302c39382c38372c32392c3138312c3235352c3131312c3135372c3232342c3233352c34362c3130302c3136392c3233322c3138392c3235322c38322c3133392c33365d2c22696e6465786573223a5b302c312c332c342c362c382c392c31302c31312c31322c31342c31382c32312c32322c32332c32352c32362c32372c33302c33332c33342c33382c34312c34332c35302c35382c35392c36302c36312c36322c36372c36392c37312c37332c37352c37362c37372c38312c38322c38332c38342c39302c39312c39322c39332c39372c39385d2c227369676e65725f696e646578223a327d",
            "indexes": [1, 3],
            "signed_message": "6a7e737c312972d2346b65ac3075696e04286d046dddaf8004121e3d5e27cc0d"
        }"#;
            let message: RegisterSignatureMessageHttp = serde_json::from_str(json).unwrap();

            assert_eq!(golden_message_current(), message);
        }
    }

    mod golden_dmq_message {
        use hex::FromHex;

        use crate::entities::{CardanoDbBeacon, Epoch};

        use super::*;

        const CURRENT_BYTES_HEX: &str = r#"0005020afbc006000001b8000000000000002f0000000000000000000000000000000100000000000000030000000000000004000000000000000600000000000000080000000000000009000000000000000a000000000000000b000000000000000c000000000000000e00000000000000120000000000000015000000000000001600000000000000170000000000000019000000000000001a000000000000001b000000000000001e0000000000000021000000000000002200000000000000260000000000000029000000000000002b0000000000000032000000000000003a000000000000003b000000000000003c000000000000003d000000000000003e0000000000000043000000000000004500000000000000470000000000000049000000000000004b000000000000004c000000000000004d0000000000000051000000000000005200000000000000530000000000000054000000000000005a000000000000005b000000000000005c000000000000005d0000000000000061000000000000006282b10fe518fbf7abc4d28f7156bd5c387021c1d436d61cd8e3ad647fb22862571db5ff6f9de0eb2e64a9e8bdfc528b240000000000000002"#;

        fn golden_message_current() -> RegisterSignatureMessageDmq {
            RegisterSignatureMessageDmq {
            signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(
                CardanoDbBeacon::new(*Epoch(10), 1728),
            ),
            signature: "7b227369676d61223a5b3133302c3137372c31352c3232392c32342c3235312c3234372c3137312c3139362c3231302c3134332c3131332c38362c3138392c39322c35362c3131322c33332c3139332c3231322c35342c3231342c32382c3231362c3232372c3137332c3130302c3132372c3137382c34302c39382c38372c32392c3138312c3235352c3131312c3135372c3232342c3233352c34362c3130302c3136392c3233322c3138392c3235322c38322c3133392c33365d2c22696e6465786573223a5b302c312c332c342c362c382c392c31302c31312c31322c31342c31382c32312c32322c32332c32352c32362c32372c33302c33332c33342c33382c34312c34332c35302c35382c35392c36302c36312c36322c36372c36392c37312c37332c37352c37362c37372c38312c38322c38332c38342c39302c39312c39322c39332c39372c39385d2c227369676e65725f696e646578223a327d".to_string().try_into().unwrap(),
        }
        }

        #[test]
        fn test_current_bytes_decoded_into_current_message() {
            let message_from_bytes_hex = RegisterSignatureMessageDmq::try_from_bytes_vec(
                &Vec::from_hex(CURRENT_BYTES_HEX).unwrap(),
            )
            .unwrap();

            assert_eq!(golden_message_current(), message_from_bytes_hex);
        }

        #[test]
        fn test_current_bijective_bytes_codec() {
            let message_to_bytes = golden_message_current().try_to_bytes_vec().unwrap();
            let message_from_bytes =
                RegisterSignatureMessageDmq::try_from_bytes_vec(&message_to_bytes).unwrap();
            let message_from_bytes_to_bytes = message_from_bytes.try_to_bytes_vec().unwrap();

            assert_eq!(golden_message_current(), message_from_bytes);
            assert_eq!(message_to_bytes, message_from_bytes_to_bytes);
        }
    }
}
