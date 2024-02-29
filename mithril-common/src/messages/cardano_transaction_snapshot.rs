use chrono::DateTime;
use chrono::Utc;
use serde::{Deserialize, Serialize};

use crate::entities::Beacon;
#[cfg(any(test, feature = "test_tools"))]
use crate::test_utils::fake_data;

/// Message structure of a Cardano Transactions snapshot
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct CardanoTransactionSnapshotMessage {
    /// Merkle root of the Cardano transactions snapshot
    pub merkle_root: String,

    /// Beacon of the Cardano transactions snapshot
    pub beacon: Beacon,

    /// Hash of the Cardano Transactions snapshot
    pub hash: String,

    /// Hash of the associated certificate
    pub certificate_hash: String,

    /// DateTime of creation
    pub created_at: DateTime<Utc>,
}

impl CardanoTransactionSnapshotMessage {
    cfg_test_tools! {
        /// Return a dummy test entity (test-only).
        pub fn dummy() -> Self {
            Self {
                merkle_root: "mkroot-123".to_string(),
                beacon: fake_data::beacon(),
                hash: "hash-123".to_string(),
                certificate_hash: "cert-hash-123".to_string(),
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn golden_message() -> CardanoTransactionSnapshotMessage {
        CardanoTransactionSnapshotMessage {
            merkle_root: "mkroot-123".to_string(),
            beacon: fake_data::beacon(),
            hash: "hash-123".to_string(),
            certificate_hash: "certificate-hash-123".to_string(),
            created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
        }
    }

    // Test the retro compatibility with possible future upgrades.
    #[test]
    fn test_v1() {
        let json = r#"{
            "merkle_root": "mkroot-123",
            "beacon": {
                "network": "testnet",
                "epoch": 10,
                "immutable_file_number": 100
            },
            "hash": "hash-123",
            "certificate_hash": "certificate-hash-123",
            "created_at": "2023-01-19T13:43:05.618857482Z"
        }"#;
        let message: CardanoTransactionSnapshotMessage = serde_json::from_str(json).expect(
            "This JSON is expected to be successfully parsed into a CardanoTransactionSnapshotMessage instance.",
        );

        assert_eq!(golden_message(), message);
    }
}
