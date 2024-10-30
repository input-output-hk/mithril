use crate::entities::{CardanoTransactionsSigningConfig, Epoch, ProtocolParameters};
use crate::messages::SignerMessagePart;
use serde::{Deserialize, Serialize};

/// EpochSettings represents the settings of an epoch
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct EpochSettingsMessage {
    /// Current Epoch
    pub epoch: Epoch,

    /// Current Protocol parameters
    #[deprecated(
        since = "0.4.77",
        note = "use only signer_registration_protocol_parameters"
    )]
    #[serde(rename = "protocol", skip_serializing_if = "Option::is_none")]
    pub protocol_parameters: Option<ProtocolParameters>,

    /// Next Protocol parameters
    #[deprecated(
        since = "0.4.77",
        note = "use only signer_registration_protocol_parameters"
    )]
    #[serde(rename = "next_protocol", skip_serializing_if = "Option::is_none")]
    pub next_protocol_parameters: Option<ProtocolParameters>,

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
            #[allow(deprecated)]
            Self {
                epoch: Epoch(10),
                protocol_parameters: Some(ProtocolParameters {
                    k: 5,
                    m: 100,
                    phi_f: 0.65,
                }),
                next_protocol_parameters: Some(ProtocolParameters {
                    k: 5,
                    m: 100,
                    phi_f: 0.65,
                }),
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

    const ACTUAL_JSON: &str = r#"{
        "epoch": 10,
        "protocol":  { "k": 5, "m": 100, "phi_f": 0.65 },
        "next_protocol":  { "k": 50, "m": 1000, "phi_f": 0.65 },
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

    // Supported structure until OpenAPI version 0.1.28.
    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    struct EpochSettingsMessageUntilV0_1_28 {
        pub epoch: Epoch,
        #[serde(rename = "protocol")]
        pub protocol_parameters: ProtocolParameters,
        #[serde(rename = "next_protocol")]
        pub next_protocol_parameters: ProtocolParameters,
    }

    // Supported structure until OpenAPI version 0.1.29.
    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    struct EpochSettingsMessageUntilV0_1_29 {
        pub epoch: Epoch,
        #[serde(rename = "protocol")]
        pub protocol_parameters: ProtocolParameters,
        #[serde(rename = "next_protocol")]
        pub next_protocol_parameters: ProtocolParameters,
        pub current_signers: Vec<SignerMessagePart>,
        pub next_signers: Vec<SignerMessagePart>,
    }

    // Supported structure until OpenAPI version 0.1.32.
    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    struct EpochSettingsMessageUntilV0_1_32 {
        pub epoch: Epoch,
        #[serde(rename = "protocol")]
        pub protocol_parameters: ProtocolParameters,
        #[serde(rename = "next_protocol")]
        pub next_protocol_parameters: ProtocolParameters,
        pub current_signers: Vec<SignerMessagePart>,
        pub next_signers: Vec<SignerMessagePart>,
        #[serde(skip_serializing_if = "Option::is_none")]
        pub cardano_transactions_signing_config: Option<CardanoTransactionsSigningConfig>,
        #[serde(skip_serializing_if = "Option::is_none")]
        pub next_cardano_transactions_signing_config: Option<CardanoTransactionsSigningConfig>,
    }

    fn golden_message_until_open_api_0_1_28() -> EpochSettingsMessageUntilV0_1_28 {
        EpochSettingsMessageUntilV0_1_28 {
            epoch: Epoch(10),
            protocol_parameters: ProtocolParameters {
                k: 5,
                m: 100,
                phi_f: 0.65,
            },
            next_protocol_parameters: ProtocolParameters {
                k: 50,
                m: 1000,
                phi_f: 0.65,
            },
        }
    }

    fn golden_message_until_open_api_0_1_29() -> EpochSettingsMessageUntilV0_1_29 {
        EpochSettingsMessageUntilV0_1_29 {
            epoch: Epoch(10),
            protocol_parameters: ProtocolParameters {
                k: 5,
                m: 100,
                phi_f: 0.65,
            },
            next_protocol_parameters: ProtocolParameters {
                k: 50,
                m: 1000,
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
        }
    }

    fn golden_message_until_open_api_0_1_32() -> EpochSettingsMessageUntilV0_1_32 {
        EpochSettingsMessageUntilV0_1_32 {
            epoch: Epoch(10),
            protocol_parameters: ProtocolParameters {
                k: 5,
                m: 100,
                phi_f: 0.65,
            },
            next_protocol_parameters: ProtocolParameters {
                k: 50,
                m: 1000,
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

    fn golden_actual_message() -> EpochSettingsMessage {
        #[allow(deprecated)]
        EpochSettingsMessage {
            epoch: Epoch(10),
            protocol_parameters: Some(ProtocolParameters {
                k: 5,
                m: 100,
                phi_f: 0.65,
            }),
            next_protocol_parameters: Some(ProtocolParameters {
                k: 50,
                m: 1000,
                phi_f: 0.65,
            }),
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

    // Test the backward compatibility with the structure supported until OpenAPI version 0.1.28.
    #[test]
    fn test_actual_json_deserialized_into_message_supported_until_open_api_0_1_28() {
        let json = ACTUAL_JSON;
        let message: EpochSettingsMessageUntilV0_1_28 = serde_json::from_str(json).expect(
            "This JSON is expected to be successfully parsed into a EpochSettingsMessageUntilVersion0_1_28 instance.",
        );

        assert_eq!(golden_message_until_open_api_0_1_28(), message);
    }

    // Test the backward compatibility with the structure supported until OpenAPI version 0.1.29.
    #[test]
    fn test_actual_json_deserialized_into_message_supported_until_open_api_0_1_29() {
        let json = ACTUAL_JSON;
        let message: EpochSettingsMessageUntilV0_1_29 = serde_json::from_str(json).expect(
            "This JSON is expected to be successfully parsed into a EpochSettingsMessageUntilVersion0_1_29 instance.",
        );

        assert_eq!(golden_message_until_open_api_0_1_29(), message);
    }

    // Test the backward compatibility with the structure supported until OpenAPI version 0.1.32.
    #[test]
    fn test_actual_json_deserialized_into_message_supported_until_open_api_0_1_32() {
        let json = ACTUAL_JSON;
        let message: EpochSettingsMessageUntilV0_1_32 = serde_json::from_str(json).expect(
                "This JSON is expected to be successfully parsed into a EpochSettingsMessageUntilVersion0_1_32 instance.",
            );

        assert_eq!(golden_message_until_open_api_0_1_32(), message);
    }

    // Test the compatibility with current structure.
    #[test]
    fn test_actual_json_deserialized_into_actual_message() {
        let json = ACTUAL_JSON;
        let message: EpochSettingsMessage = serde_json::from_str(json).expect(
            "This JSON is expected to be successfully parsed into a EpochSettingsMessage instance.",
        );

        assert_eq!(golden_actual_message(), message);
    }
}
