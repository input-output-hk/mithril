use std::str::FromStr;

use chrono::DateTime;
use chrono::Utc;
use serde::{Deserialize, Serialize};

use crate::entities::Epoch;
use crate::entities::ProtocolParameters;
use crate::test_utils::fake_data;

use super::SignerWithStakeMessagePart;
/// Message structure of a Mitrhil Stake Distribution
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct MithrilStakeDistributionMessage {
    /// Epoch at which the Mithril Stake Distribution is created
    pub epoch: Epoch,

    /// List of signers with stakes of the Mithril Stake Distribution
    #[serde(rename = "signers")]
    pub signers_with_stake: Vec<SignerWithStakeMessagePart>,

    /// Hash of the Mithril Stake Distribution (different from the AVK).
    pub hash: String,

    /// Hash of the associated certificate
    pub certificate_hash: String,

    /// DateTime of creation
    pub created_at: DateTime<Utc>,

    /// Protocol parameters used to compute AVK
    pub protocol_parameters: ProtocolParameters,
}

impl MithrilStakeDistributionMessage {
    /// Return a dummy test entity (test-only).
    pub fn dummy() -> Self {
        Self {
            epoch: Epoch(1),
            signers_with_stake: vec![SignerWithStakeMessagePart::dummy()],
            hash: "hash-123".to_string(),
            certificate_hash: "cert-hash-123".to_string(),
            created_at: DateTime::from_str("2023-06-13 17:05:41").unwrap(),
            protocol_parameters: fake_data::protocol_parameters(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn golden_message() -> MithrilStakeDistributionMessage {
        MithrilStakeDistributionMessage {
            epoch: Epoch(1),
            signers_with_stake: vec![
                SignerWithStakeMessagePart {
                    party_id: "0".to_string(),
                    verification_key: "7b22766b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c22706f70223a5b3136382c35302c3233332c3139332c31352c3133362c36352c37322c3132332c3134382c3132392c3137362c33382c3139382c3230392c34372c32382c3230342c3137362c3134342c35372c3235312c34322c32382c36362c37362c38392c39372c3135382c36332c35342c3139382c3139342c3137362c3133352c3232312c31342c3138352c3139372c3232352c3230322c39382c3234332c37342c3233332c3232352c3134332c3135312c3134372c3137372c3137302c3131372c36362c3136352c36362c36322c33332c3231362c3233322c37352c36382c3131342c3139352c32322c3130302c36352c34342c3139382c342c3136362c3130322c3233332c3235332c3234302c35392c3137352c36302c3131372c3134322c3131342c3134302c3132322c31372c38372c3131302c3138372c312c31372c31302c3139352c3135342c31332c3234392c38362c35342c3232365d7d".to_string(),
                    verification_key_signature: None,
                    operational_certificate: None,
                    kes_period: None,
                    stake: 826
                },
            ],
            hash: "hash-123".to_string(),
            certificate_hash: "cert-hash-123".to_string(),
            created_at: DateTime::from_str("2023-06-13T17:05:41Z").unwrap(),
            protocol_parameters: fake_data::protocol_parameters(),
        }
    }

    // Test the retro compatibility with possible future upgrades.
    #[test]
    fn test_v1() {
        let json = r#"{
            "epoch": 1,
            "signers": [
                {
                    "party_id": "0",
                    "verification_key": "7b22766b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c22706f70223a5b3136382c35302c3233332c3139332c31352c3133362c36352c37322c3132332c3134382c3132392c3137362c33382c3139382c3230392c34372c32382c3230342c3137362c3134342c35372c3235312c34322c32382c36362c37362c38392c39372c3135382c36332c35342c3139382c3139342c3137362c3133352c3232312c31342c3138352c3139372c3232352c3230322c39382c3234332c37342c3233332c3232352c3134332c3135312c3134372c3137372c3137302c3131372c36362c3136352c36362c36322c33332c3231362c3233322c37352c36382c3131342c3139352c32322c3130302c36352c34342c3139382c342c3136362c3130322c3233332c3235332c3234302c35392c3137352c36302c3131372c3134322c3131342c3134302c3132322c31372c38372c3131302c3138372c312c31372c31302c3139352c3135342c31332c3234392c38362c35342c3232365d7d",
                    "stake": 826
                }
            ],
            "hash": "hash-123",
            "certificate_hash": "cert-hash-123",
            "created_at": "2023-06-13T17:05:41Z",
            "protocol_parameters": {"k": 5, "m": 100, "phi_f": 0.65 }
        }"#;
        let message: MithrilStakeDistributionMessage = serde_json::from_str(json).expect(
            "This JSON is expected to be succesfully parsed into a MithrilStakeDistributionMessage instance.",
        );

        assert_eq!(golden_message(), message);
    }
}
