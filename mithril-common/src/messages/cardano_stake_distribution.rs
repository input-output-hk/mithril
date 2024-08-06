use chrono::DateTime;
use chrono::Utc;
use serde::{Deserialize, Serialize};

use crate::entities::Epoch;
use crate::entities::StakeDistribution;

/// Message structure of a Cardano Stake Distribution
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct CardanoStakeDistributionMessage {
    /// Epoch at the end of which the Cardano stake distribution is computed by the Cardano node
    pub epoch: Epoch,

    /// Hash of the Cardano Stake Distribution
    pub hash: String,

    /// Hash of the associated certificate
    pub certificate_hash: String,

    /// Represents the list of participants in the Cardano chain with their associated stake
    pub stake_distribution: StakeDistribution,

    /// DateTime of creation
    pub created_at: DateTime<Utc>,
}

impl CardanoStakeDistributionMessage {
    cfg_test_tools! {
        /// Return a dummy test entity (test-only).
        pub fn dummy() -> Self {
            Self {
                epoch: Epoch(1),
                hash: "hash-123".to_string(),
                certificate_hash: "cert-hash-123".to_string(),
                stake_distribution: StakeDistribution::from([
                    ("pool-123".to_string(), 1000),
                ]),
                created_at: DateTime::parse_from_rfc3339("2024-07-29T16:15:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn golden_message() -> CardanoStakeDistributionMessage {
        CardanoStakeDistributionMessage {
            epoch: Epoch(1),
            hash: "hash-123".to_string(),
            certificate_hash: "cert-hash-123".to_string(),
            stake_distribution: StakeDistribution::from([
                ("pool-123".to_string(), 1000),
                ("pool-456".to_string(), 2000),
            ]),
            created_at: DateTime::parse_from_rfc3339("2024-07-29T16:15:05.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
        }
    }

    // Test the backward compatibility with possible future upgrades.
    #[test]
    fn test_v1() {
        let json = r#"{
            "epoch": 1,
            "hash": "hash-123",
            "certificate_hash": "cert-hash-123",
            "stake_distribution": { "pool-123": 1000, "pool-456": 2000 },
            "created_at": "2024-07-29T16:15:05.618857482Z"
        }"#;
        let message: CardanoStakeDistributionMessage = serde_json::from_str(json).expect(
            "This JSON is expected to be successfully parsed into a CardanoStakeDistributionMessage instance.",
        );

        assert_eq!(golden_message(), message);
    }
}
