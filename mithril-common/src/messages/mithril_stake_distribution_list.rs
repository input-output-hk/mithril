use serde::{Deserialize, Serialize};

use crate::entities::Epoch;

/// Message structure of a Mithril Stake Distribution list
pub type MithrilStakeDistributionListMessage = Vec<MithrilStakeDistributionListItemMessage>;

/// Message structure of a Mithril Stake Distribution list item
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct MithrilStakeDistributionListItemMessage {
    /// Epoch at which the Mithril Stake Distribution is created
    pub epoch: Epoch,

    /// Hash of the Mithril Stake Distribution (different from the AVK).
    pub hash: String,
}

impl MithrilStakeDistributionListItemMessage {
    /// Return a dummy test entity (test-only).
    pub fn dummy() -> Self {
        Self {
            epoch: Epoch(1),
            hash: "hash-123".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn golden_message() -> MithrilStakeDistributionListMessage {
        vec![MithrilStakeDistributionListItemMessage {
            epoch: Epoch(1),
            hash: "hash-123".to_string(),
        }]
    }

    // Test the retro compatibility with possible future upgrades.
    #[test]
    fn test_v1() {
        let json = r#"[{
        "epoch": 1,
        "hash": "hash-123"
        }]"#;
        let message: MithrilStakeDistributionListMessage = serde_json::from_str(json).expect(
                    "This JSON is expected to be succesfully parsed into a MithrilStakeDistributionListMessage instance.",
                );

        assert_eq!(golden_message(), message);
    }
}
