use serde::{Deserialize, Serialize};

use crate::entities::Epoch;
use crate::entities::SignerWithStake;
use crate::test_utils::fake_data;
/// Message structure of a Mitrhil Stake Distribution
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct MithrilStakeDistributionMessage {
    /// Epoch at which the Mithril Stake Distribution is created
    pub epoch: Epoch,

    /// List of signers with stakes of the Mithril Stake Distribution
    #[serde(rename = "signers")]
    pub signers_with_stake: Vec<SignerWithStake>,

    /// Hash of the Mithril Stake Distribution (different from the AVK).
    pub hash: String,
}

impl MithrilStakeDistributionMessage {
    /// Return a dummy test entity (test-only).
    pub fn dummy() -> Self {
        Self {
            epoch: Epoch(1),
            signers_with_stake: vec![fake_data::signers_with_stakes(1)
                .first()
                .unwrap()
                .to_owned()],
            hash: "hash-123".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn golden_message() -> MithrilStakeDistributionMessage {
        MithrilStakeDistributionMessage {
            epoch: Epoch(1),
            signers_with_stake: fake_data::signers_with_stakes(2),
            hash: "hash-123".to_string(),
        }
    }

    // Test the retro compatibility with possible future upgrades.
    #[test]
    fn test_v1() {
        let json = r#"{
"digest": "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6",
"beacon": {
  "network": "preview",
  "epoch": 86,
  "immutable_file_number": 1728
},
"certificate_hash": "d5daf6c03ace4a9c074e951844075b9b373bafc4e039160e3e2af01823e9abfb",
"size": 807803196,
"created_at": "2023-01-19T13:43:05.618857482Z",
"locations": [
  "https://host/certificate.tar.gz"
]
}"#;
        let message: MithrilStakeDistributionMessage = serde_json::from_str(json).expect(
            "This JSON is expected to be succesfully parsed into a MithrilStakeDistributionMessage instance.",
        );

        assert_eq!(golden_message(), message);
    }
}
