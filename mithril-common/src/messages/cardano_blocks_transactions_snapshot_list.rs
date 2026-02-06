use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use crate::entities::{BlockNumber, Epoch};

/// Message structure of a Cardano Transactions Snapshots list
pub type CardanoBlocksTransactionsSnapshotListMessage =
    Vec<CardanoBlocksTransactionsSnapshotListItemMessage>;

/// Message structure of a Cardano Transactions Snapshot list item
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CardanoBlocksTransactionsSnapshotListItemMessage {
    /// Merkle root of the Cardano Blocks and Transactions snapshot
    pub merkle_root: String,

    /// Epoch of the Cardano Blocks and Transactions snapshot
    pub epoch: Epoch,

    /// The maximum block number signed in the Cardano Blocks and Transactions snapshot
    pub block_number_signed: BlockNumber,

    /// The block number of the tip of the chain at snapshot time of the Cardano Blocks and Transactions
    pub block_number_tip: BlockNumber,

    /// Hash of the Cardano Blocks and Transactions snapshot
    pub hash: String,

    /// Hash of the associated certificate
    pub certificate_hash: String,

    /// Time of creation
    pub created_at: DateTime<Utc>,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn golden_message_current() -> CardanoBlocksTransactionsSnapshotListMessage {
        vec![CardanoBlocksTransactionsSnapshotListItemMessage {
            merkle_root: "mkroot-123".to_string(),
            epoch: Epoch(7),
            block_number_signed: BlockNumber(5),
            block_number_tip: BlockNumber(15),
            hash: "hash-123".to_string(),
            certificate_hash: "certificate-hash-123".to_string(),
            created_at: DateTime::parse_from_rfc3339("2023-01-19T13:41:01.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
        }]
    }

    const CURRENT_JSON: &str = r#"[{
        "merkle_root": "mkroot-123",
        "epoch": 7,
        "block_number_signed": 5,
        "block_number_tip":15,
        "hash": "hash-123",
        "certificate_hash": "certificate-hash-123",
        "created_at": "2023-01-19T13:41:01.618857482Z"
    }]"#;

    #[test]
    fn test_current_json_deserialized_into_current_message() {
        let json = CURRENT_JSON;
        let message: CardanoBlocksTransactionsSnapshotListMessage = serde_json::from_str(json).expect(
                    "This JSON is expected to be successfully parsed into a CardanoBlocksTransactionsSnapshotListMessage instance.",
                );
        assert_eq!(golden_message_current(), message);
    }
}
