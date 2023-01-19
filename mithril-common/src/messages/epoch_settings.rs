use crate::entities::{Epoch, ProtocolParameters};
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
}

impl EpochSettingsMessage {
    #[cfg(any(test, feature = "test_only"))]
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
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn golden_message() -> EpochSettingsMessage {
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
        }
    }

    // Test the retro compatibility with possible future upgrades.
    #[test]
    fn test_v1() {
        let json = r#"{
"epoch": 10,
"protocol":  { "k": 5, "m": 100, "phi_f": 0.65 },
"next_protocol":  { "k": 50, "m": 1000, "phi_f": 0.65 }
}"#;
        let message: EpochSettingsMessage = serde_json::from_str(json).expect(
            "This JSON is expected to be succesfully parsed into a EpochSettingsMessage instance.",
        );

        assert_eq!(golden_message(), message);
    }
}
