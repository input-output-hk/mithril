use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use crate::entities::{CompressionAlgorithm, Epoch};
use crate::messages::CardanoDbBeaconMessagePart;

/// Message structure of a snapshot list
pub type CardanoDatabaseSnapshotListMessage = Vec<CardanoDatabaseSnapshotListItemMessage>;

/// Message structure of a snapshot list item
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CardanoDatabaseSnapshotListItemMessage {
    /// Merkle root of the Cardano database snapshot
    pub merkle_root: String,

    /// Mithril beacon on the Cardano chain
    pub beacon: CardanoDbBeaconMessagePart,

    /// Hash of the associated certificate
    pub certificate_hash: String,

    /// Size of the uncompressed Cardano database files.
    pub total_db_size_uncompressed: u64,

    /// Compression algorithm of the Cardano database artifacts.
    pub compression_algorithm: CompressionAlgorithm,

    /// Version of the Cardano node used to create the snapshot.
    pub cardano_node_version: String,

    /// Date and time at which the snapshot was created
    pub created_at: DateTime<Utc>,
}

impl CardanoDatabaseSnapshotListItemMessage {
    /// Return a dummy test entity (test-only).
    pub fn dummy() -> Self {
        Self {
            merkle_root: "c8224920b9f5ad7377594eb8a15f34f08eb3103cc5241d57cafc5638403ec7c6"
                .to_string(),
            beacon: CardanoDbBeaconMessagePart {
                network: Some("preview".to_string()),
                epoch: Epoch(123),
                immutable_file_number: 2345,
            },
            certificate_hash: "f6c01b373bafc4e039844071d5da3ace4a9c0745b9e9560e3e2af01823e9abfb"
                .to_string(),
            total_db_size_uncompressed: 800796318,
            created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
            compression_algorithm: CompressionAlgorithm::default(),
            cardano_node_version: "0.0.1".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn golden_message() -> CardanoDatabaseSnapshotListMessage {
        vec![CardanoDatabaseSnapshotListItemMessage {
            merkle_root: "c8224920b9f5ad7377594eb8a15f34f08eb3103cc5241d57cafc5638403ec7c6"
                .to_string(),
            beacon: CardanoDbBeaconMessagePart {
                network: Some("preview".to_string()),
                epoch: Epoch(123),
                immutable_file_number: 2345,
            },
            certificate_hash: "f6c01b373bafc4e039844071d5da3ace4a9c0745b9e9560e3e2af01823e9abfb"
                .to_string(),
            total_db_size_uncompressed: 800796318,
            created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
            compression_algorithm: CompressionAlgorithm::default(),
            cardano_node_version: "0.0.1".to_string(),
        }]
    }

    // Test the backward compatibility with possible future upgrades.
    #[test]
    fn test_v1() {
        let json = r#"[
            {
                "merkle_root": "c8224920b9f5ad7377594eb8a15f34f08eb3103cc5241d57cafc5638403ec7c6",
                "beacon": {
                "network": "preview",
                "epoch": 123,
                "immutable_file_number": 2345
                },
                "certificate_hash": "f6c01b373bafc4e039844071d5da3ace4a9c0745b9e9560e3e2af01823e9abfb",
                "total_db_size_uncompressed": 800796318,
                "compression_algorithm": "gzip",
                "cardano_node_version": "0.0.1",
                "created_at": "2023-01-19T13:43:05.618857482Z"
            }
        ]"#;
        let message: CardanoDatabaseSnapshotListMessage = serde_json::from_str(json).expect(
            "This JSON is expected to be successfully parsed into a CardanoDatabaseSnapshotListMessage instance.",
        );

        assert_eq!(golden_message(), message);
    }
}