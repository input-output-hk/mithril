use serde::{Deserialize, Serialize};

use crate::{
    entities::{HexEncodedSingleSignature, LotteryIndex, PartyId, SignedEntityType},
    test_utils::fake_keys,
};

era_deprecate!("make signed_entity_type of RegisterSignatureMessage not optional");
/// Message structure to register single signature.
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct RegisterSignatureMessage {
    /// Signed entity type
    #[serde(rename = "entity_type")]
    pub signed_entity_type: Option<SignedEntityType>,

    /// The unique identifier of the signer
    pub party_id: PartyId,

    /// The single signature of the digest
    pub signature: HexEncodedSingleSignature,

    /// The indexes of the won lotteries that lead to the single signatures
    #[serde(rename = "indexes")]
    pub won_indexes: Vec<LotteryIndex>,
}

impl RegisterSignatureMessage {
    /// Return a dummy test entity (test-only).
    pub fn dummy() -> Self {
        Self {
            signed_entity_type: Some(SignedEntityType::dummy()),
            party_id: "party_id".to_string(),
            signature: fake_keys::single_signature()[0].to_string(),
            won_indexes: vec![1, 3],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn golden_message() -> RegisterSignatureMessage {
        RegisterSignatureMessage {
            signed_entity_type: None,
            party_id: "party_id".to_string(),
            signature: "7b227369676d61223a5b3133302c3137372c31352c3232392c32342c3235312c3234372c3137312c3139362c3231302c3134332c3131332c38362c3138392c39322c35362c3131322c33332c3139332c3231322c35342c3231342c32382c3231362c3232372c3137332c3130302c3132372c3137382c34302c39382c38372c32392c3138312c3235352c3131312c3135372c3232342c3233352c34362c3130302c3136392c3233322c3138392c3235322c38322c3133392c33365d2c22696e6465786573223a5b302c312c332c342c362c382c392c31302c31312c31322c31342c31382c32312c32322c32332c32352c32362c32372c33302c33332c33342c33382c34312c34332c35302c35382c35392c36302c36312c36322c36372c36392c37312c37332c37352c37362c37372c38312c38322c38332c38342c39302c39312c39322c39332c39372c39385d2c227369676e65725f696e646578223a327d".to_string(),
            won_indexes: vec![1, 3],
        }
    }

    // Test the retro compatibility with possible future upgrades.
    #[test]
    fn test_v1() {
        let json = r#"{
"party_id": "party_id",
"signature":  "7b227369676d61223a5b3133302c3137372c31352c3232392c32342c3235312c3234372c3137312c3139362c3231302c3134332c3131332c38362c3138392c39322c35362c3131322c33332c3139332c3231322c35342c3231342c32382c3231362c3232372c3137332c3130302c3132372c3137382c34302c39382c38372c32392c3138312c3235352c3131312c3135372c3232342c3233352c34362c3130302c3136392c3233322c3138392c3235322c38322c3133392c33365d2c22696e6465786573223a5b302c312c332c342c362c382c392c31302c31312c31322c31342c31382c32312c32322c32332c32352c32362c32372c33302c33332c33342c33382c34312c34332c35302c35382c35392c36302c36312c36322c36372c36392c37312c37332c37352c37362c37372c38312c38322c38332c38342c39302c39312c39322c39332c39372c39385d2c227369676e65725f696e646578223a327d",
"indexes": [1, 3]
}"#;
        let message: RegisterSignatureMessage = serde_json::from_str(json).expect(
            "This JSON is expected to be succesfully parsed into a RegisterSignatureMessage instance.",
        );

        assert_eq!(golden_message(), message);
    }
}
