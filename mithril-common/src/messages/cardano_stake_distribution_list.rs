use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use crate::entities::Epoch;

/// Message structure of a Cardano Stake Distribution list
pub type CardanoStakeDistributionListMessage = Vec<CardanoStakeDistributionListItemMessage>;

/// Message structure of a Cardano Stake Distribution list item
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct CardanoStakeDistributionListItemMessage {
    /// Epoch at the end of which the Cardano stake distribution is computed by the Cardano node
    pub epoch: Epoch,

    /// Hash of the Cardano Stake Distribution
    pub hash: String,

    /// Hash of the associated certificate
    pub certificate_hash: String,

    /// Date and time at which the Cardano Stake Distribution was created
    pub created_at: DateTime<Utc>,
}

impl CardanoStakeDistributionListItemMessage {
    /// Return a dummy test entity (test-only).
    pub fn dummy() -> Self {
        Self {
            epoch: Epoch(1),
            hash: "hash-123".to_string(),
            certificate_hash: "certificate-hash-123".to_string(),
            created_at: DateTime::parse_from_rfc3339("2024-07-29T16:15:05.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn golden_message_actual() -> CardanoStakeDistributionListMessage {
        vec![CardanoStakeDistributionListItemMessage {
            epoch: Epoch(1),
            hash: "hash-123".to_string(),
            certificate_hash: "cert-hash-123".to_string(),
            created_at: DateTime::parse_from_rfc3339("2024-07-29T16:15:05.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
        }]
    }

    const ACTUAL_JSON: &str = r#"[{
        "epoch": 1,
        "hash": "hash-123",
        "certificate_hash": "cert-hash-123",
        "created_at": "2024-07-29T16:15:05.618857482Z"
    }]"#;

    #[test]
    fn test_actual_json_deserialized_into_actual_message() {
        let json = ACTUAL_JSON;
        let message: CardanoStakeDistributionListMessage = serde_json::from_str(json).expect(
            "This JSON is expected to be successfully parsed into a CardanoStakeDistributionListMessage instance.",
        );

        assert_eq!(golden_message_actual(), message);
    }
}
