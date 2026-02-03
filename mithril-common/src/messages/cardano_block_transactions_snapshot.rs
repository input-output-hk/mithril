use chrono::DateTime;
use chrono::Utc;
use serde::{Deserialize, Serialize};

use crate::entities::{BlockNumber, Epoch};

/// Message structure of a Cardano Transactions snapshot
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct CardanoBlockTransactionsSnapshotMessage {
    /// Merkle root of the Cardano block transactions snapshot
    pub merkle_root: String,

    /// Epoch of the Cardano block transactions snapshot
    pub epoch: Epoch,

    /// Block number of the Cardano block transactions snapshot
    pub block_number_signed: BlockNumber,

    /// The aproximate block number tip of the Cardano block transactions snapshot
    pub block_number_tip: BlockNumber,

    /// Hash of the Cardano block transactions snapshot
    pub hash: String,

    /// Hash of the associated certificate
    pub certificate_hash: String,

    /// DateTime of creation
    pub created_at: DateTime<Utc>,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn golden_message_current() -> CardanoBlockTransactionsSnapshotMessage {
        CardanoBlockTransactionsSnapshotMessage {
            merkle_root: "mkroot-123".to_string(),
            epoch: Epoch(7),
            block_number_signed: BlockNumber(5),
            block_number_tip: BlockNumber(15),
            hash: "hash-123".to_string(),
            certificate_hash: "certificate-hash-123".to_string(),
            created_at: DateTime::parse_from_rfc3339("2023-01-19T13:41:01.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
        }
    }

    const CURRENT_JSON: &str = r#"{
        "merkle_root": "mkroot-123",
        "epoch": 7,
        "block_number_signed": 5,
        "block_number_tip":15,
        "hash": "hash-123",
        "certificate_hash": "certificate-hash-123",
        "created_at": "2023-01-19T13:41:01.618857482Z"
    }"#;

    #[test]
    fn test_current_json_deserialized_into_current_message() {
        let json = CURRENT_JSON;
        let message: CardanoBlockTransactionsSnapshotMessage = serde_json::from_str(json).expect(
            "This JSON is expected to be successfully parsed into a CardanoBlockTransactionsSnapshotMessage instance.",
        );

        assert_eq!(golden_message_current(), message);
    }
}
