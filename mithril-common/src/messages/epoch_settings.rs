use crate::entities::{Epoch, ProtocolParameters};
use crate::messages::SignerMessagePart;
use serde::{Deserialize, Serialize};

/// EpochSettings represents the settings of an epoch
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct EpochSettingsMessage {
    /// Current Epoch
    pub epoch: Epoch,

    /// Current Protocol parameters
    #[serde(rename = "protocol")]
    pub protocol_parameters: ProtocolParameters,

    /// Next Protocol parameters
    #[serde(rename = "next_protocol")]
    pub next_protocol_parameters: ProtocolParameters,

    /// Current Signers
    pub current_signers: Vec<SignerMessagePart>,

    /// Signers that will be able to sign on the next epoch
    pub next_signers: Vec<SignerMessagePart>,
}

impl EpochSettingsMessage {
    cfg_test_tools! {
        /// Dummy instance for test purposes.
        pub fn dummy() -> Self {
            Self {
                epoch: Epoch(10),
                protocol_parameters: ProtocolParameters {
                    k: 5,
                    m: 100,
                    phi_f: 0.65,
                },
                next_protocol_parameters: ProtocolParameters {
                    k: 5,
                    m: 100,
                    phi_f: 0.65,
                },
                current_signers: [SignerMessagePart::dummy()].to_vec(),
                next_signers: [SignerMessagePart::dummy()].to_vec(),
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    const ACTUAL_JSON: &str = r#"{
        "epoch": 10,
        "protocol":  { "k": 5, "m": 100, "phi_f": 0.65 },
        "next_protocol":  { "k": 50, "m": 1000, "phi_f": 0.65 },
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
        }]
        
        }"#;

    #[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
    pub struct EpochSettingsMessagePreviousVersion {
        /// Current Epoch
        pub epoch: Epoch,

        /// Current Protocol parameters
        #[serde(rename = "protocol")]
        pub protocol_parameters: ProtocolParameters,

        /// Next Protocol parameters
        #[serde(rename = "next_protocol")]
        pub next_protocol_parameters: ProtocolParameters,
    }

    fn golden_previous_message() -> EpochSettingsMessagePreviousVersion {
        EpochSettingsMessagePreviousVersion {
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

    fn golden_actual_message() -> EpochSettingsMessage {
        EpochSettingsMessage {
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

    // Test the backward compatibility with previous structure.
    #[test]
    fn test_actual_json_deserialized_into_previous_message() {
        let json = ACTUAL_JSON;
        let message: EpochSettingsMessagePreviousVersion = serde_json::from_str(json).expect(
            "This JSON is expected to be successfully parsed into a EpochSettingsMessagePreviousVersion instance.",
        );

        assert_eq!(golden_previous_message(), message);
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
