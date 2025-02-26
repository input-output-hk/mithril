use crate::entities::{CardanoTransactionsSigningConfig, Epoch, ProtocolParameters};
use crate::messages::SignerMessagePart;
use serde::{Deserialize, Serialize};

/// EpochSettings represents the settings of an epoch
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct EpochSettingsMessage {
    /// Current Epoch
    pub epoch: Epoch,

    /// Signer Registration Protocol parameters
    #[serde(rename = "signer_registration_protocol")]
    pub signer_registration_protocol_parameters: ProtocolParameters,

    /// Current Signers
    pub current_signers: Vec<SignerMessagePart>,

    /// Signers that will be able to sign on the next epoch
    pub next_signers: Vec<SignerMessagePart>,

    /// Cardano transactions signing configuration for the current epoch
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cardano_transactions_signing_config: Option<CardanoTransactionsSigningConfig>,

    /// Cardano transactions signing configuration for the next epoch
    #[serde(skip_serializing_if = "Option::is_none")]
    pub next_cardano_transactions_signing_config: Option<CardanoTransactionsSigningConfig>,
}

impl EpochSettingsMessage {
    cfg_test_tools! {
        /// Dummy instance for test purposes.
        pub fn dummy() -> Self {
            Self {
                epoch: Epoch(10),
                signer_registration_protocol_parameters: ProtocolParameters {
                    k: 5,
                    m: 100,
                    phi_f: 0.65,
                },
                current_signers: [SignerMessagePart::dummy()].to_vec(),
                next_signers: [SignerMessagePart::dummy()].to_vec(),
                cardano_transactions_signing_config: Some(CardanoTransactionsSigningConfig::dummy()),
                next_cardano_transactions_signing_config: Some(CardanoTransactionsSigningConfig::dummy()),
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::entities::BlockNumber;

    use super::*;

    const CURRENT_JSON: &str = r#"{
        "epoch": 10,
        "signer_registration_protocol":  { "k": 500, "m": 10000, "phi_f": 0.65 },
        "current_signers":[{
            "party_id":"123",
            "verification_key":"key_123",
            "verification_key_signature":"signature_123",
            "operational_certificate":"certificate_123",
            "kes_period":12
        }],
        "next_signers": [{
            "party_id":"456",
            "verification_key":"key_456",
            "verification_key_signature":"signature_456",
            "operational_certificate":"certificate_456",
            "kes_period":45
        }],
        "cardano_transactions_signing_config": {
            "security_parameter": 70,
            "step": 20
        },
        "next_cardano_transactions_signing_config": {
            "security_parameter": 50,
            "step": 10
        }
    }"#;

    fn golden_current_message() -> EpochSettingsMessage {
        EpochSettingsMessage {
            epoch: Epoch(10),
            signer_registration_protocol_parameters: ProtocolParameters {
                k: 500,
                m: 10000,
                phi_f: 0.65,
            },
            current_signers: vec![SignerMessagePart {
                party_id: "123".to_string(),
                verification_key: "key_123".to_string(),
                verification_key_signature: Some("signature_123".to_string()),
                operational_certificate: Some("certificate_123".to_string()),
                kes_period: Some(12),
            }],
            next_signers: vec![SignerMessagePart {
                party_id: "456".to_string(),
                verification_key: "key_456".to_string(),
                verification_key_signature: Some("signature_456".to_string()),
                operational_certificate: Some("certificate_456".to_string()),
                kes_period: Some(45),
            }],
            cardano_transactions_signing_config: Some(CardanoTransactionsSigningConfig {
                security_parameter: BlockNumber(70),
                step: BlockNumber(20),
            }),
            next_cardano_transactions_signing_config: Some(CardanoTransactionsSigningConfig {
                security_parameter: BlockNumber(50),
                step: BlockNumber(10),
            }),
        }
    }

    #[test]
    fn test_current_json_deserialized_into_current_message() {
        let json = CURRENT_JSON;
        let message: EpochSettingsMessage = serde_json::from_str(json).expect(
            "This JSON is expected to be successfully parsed into a EpochSettingsMessage instance.",
        );

        assert_eq!(golden_current_message(), message);
    }
}
