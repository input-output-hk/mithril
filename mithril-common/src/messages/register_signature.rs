use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Formatter};

use crate::entities::{HexEncodedSingleSignature, LotteryIndex, PartyId};
use crate::messages::SignedEntityTypeMessagePart;

#[cfg(any(test, feature = "test_tools"))]
use crate::test_utils::fake_keys;

/// Message structure to register single signature.
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RegisterSignatureMessage {
    /// Signed entity type
    #[serde(rename = "entity_type")]
    pub signed_entity_type: SignedEntityTypeMessagePart,

    /// The unique identifier of the signer
    pub party_id: PartyId,

    /// The single signature of the digest
    pub signature: HexEncodedSingleSignature,

    /// The indexes of the won lotteries that lead to the single signatures
    #[serde(rename = "indexes")]
    pub won_indexes: Vec<LotteryIndex>,

    /// Message that is signed by the signer
    ///
    /// Used to buffer the signature for later if the aggregator has yet to create an open message
    /// for the signed entity type.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub signed_message: Option<String>,
}

impl RegisterSignatureMessage {
    cfg_test_tools! {
        /// Return a dummy test entity (test-only).
        pub fn dummy() -> Self {
            use crate::entities::Epoch;
            Self {
                signed_entity_type: SignedEntityTypeMessagePart::MithrilStakeDistribution(Epoch(5)),
                party_id: "party_id".to_string(),
                signature: fake_keys::single_signature()[0].to_string(),
                won_indexes: vec![1, 3],
                signed_message: None,
            }
        }
    }
}

impl Debug for RegisterSignatureMessage {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let is_pretty_printing = f.alternate();
        let mut debug = f.debug_struct("SingleSignatures");
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

#[cfg(test)]
mod tests {
    use crate::entities::Epoch;
    use crate::messages::CardanoDbBeaconMessagePart;

    use super::*;

    const CURRENT_JSON: &str = r#"{
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

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    pub struct RegisterSignatureMessageUntilV0_1_28 {
        #[serde(rename = "entity_type")]
        pub signed_entity_type: SignedEntityTypeMessagePart,
        pub party_id: PartyId,
        pub signature: HexEncodedSingleSignature,
        #[serde(rename = "indexes")]
        pub won_indexes: Vec<LotteryIndex>,
    }

    fn golden_message_until_open_api_0_1_28() -> RegisterSignatureMessageUntilV0_1_28 {
        RegisterSignatureMessageUntilV0_1_28 {
            signed_entity_type: SignedEntityTypeMessagePart::CardanoImmutableFilesFull(
                CardanoDbBeaconMessagePart::new("testnet", Epoch(10), 1728),
            ),
            party_id: "party_id".to_string(),
            signature: "7b227369676d61223a5b3133302c3137372c31352c3232392c32342c3235312c3234372c3137312c3139362c3231302c3134332c3131332c38362c3138392c39322c35362c3131322c33332c3139332c3231322c35342c3231342c32382c3231362c3232372c3137332c3130302c3132372c3137382c34302c39382c38372c32392c3138312c3235352c3131312c3135372c3232342c3233352c34362c3130302c3136392c3233322c3138392c3235322c38322c3133392c33365d2c22696e6465786573223a5b302c312c332c342c362c382c392c31302c31312c31322c31342c31382c32312c32322c32332c32352c32362c32372c33302c33332c33342c33382c34312c34332c35302c35382c35392c36302c36312c36322c36372c36392c37312c37332c37352c37362c37372c38312c38322c38332c38342c39302c39312c39322c39332c39372c39385d2c227369676e65725f696e646578223a327d".to_string(),
            won_indexes: vec![1, 3],
        }
    }

    fn golden_message_current() -> RegisterSignatureMessage {
        RegisterSignatureMessage {
            signed_entity_type: SignedEntityTypeMessagePart::CardanoImmutableFilesFull(
                CardanoDbBeaconMessagePart::new("testnet", Epoch(10), 1728),
            ),
            party_id: "party_id".to_string(),
            signature: "7b227369676d61223a5b3133302c3137372c31352c3232392c32342c3235312c3234372c3137312c3139362c3231302c3134332c3131332c38362c3138392c39322c35362c3131322c33332c3139332c3231322c35342c3231342c32382c3231362c3232372c3137332c3130302c3132372c3137382c34302c39382c38372c32392c3138312c3235352c3131312c3135372c3232342c3233352c34362c3130302c3136392c3233322c3138392c3235322c38322c3133392c33365d2c22696e6465786573223a5b302c312c332c342c362c382c392c31302c31312c31322c31342c31382c32312c32322c32332c32352c32362c32372c33302c33332c33342c33382c34312c34332c35302c35382c35392c36302c36312c36322c36372c36392c37312c37332c37352c37362c37372c38312c38322c38332c38342c39302c39312c39322c39332c39372c39385d2c227369676e65725f696e646578223a327d".to_string(),
            won_indexes: vec![1, 3],
            signed_message: Some("6a7e737c312972d2346b65ac3075696e04286d046dddaf8004121e3d5e27cc0d".to_string()),
        }
    }

    #[test]
    fn test_current_json_deserialized_into_message_supported_until_open_api_0_1_28() {
        let json = CURRENT_JSON;
        let message: RegisterSignatureMessageUntilV0_1_28 = serde_json::from_str(json).unwrap();

        assert_eq!(golden_message_until_open_api_0_1_28(), message);
    }

    #[test]
    fn test_current_json_deserialized_into_current_message() {
        let json = CURRENT_JSON;
        let message: RegisterSignatureMessage = serde_json::from_str(json).unwrap();

        assert_eq!(golden_message_current(), message);
    }
}
