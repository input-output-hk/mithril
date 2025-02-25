use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use crate::entities::{CardanoDbBeacon, CompressionAlgorithm, Epoch};

/// Message structure of a snapshot list
pub type SnapshotListMessage = Vec<SnapshotListItemMessage>;

/// Message structure of a snapshot list item
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SnapshotListItemMessage {
    /// Digest that is signed by the signer participants
    pub digest: String,

    /// Cardano network
    pub network: String,

    /// Mithril beacon on the Cardano chain
    pub beacon: CardanoDbBeacon,

    /// Hash of the associated certificate
    pub certificate_hash: String,

    /// Size of the snapshot file in Bytes
    pub size: u64,

    /// Date and time at which the snapshot was created
    pub created_at: DateTime<Utc>,

    /// Locations where the binary content of the snapshot can be retrieved
    pub locations: Vec<String>,

    /// Compression algorithm of the snapshot archive
    pub compression_algorithm: CompressionAlgorithm,

    /// Cardano node version
    pub cardano_node_version: String,
}

impl SnapshotListItemMessage {
    /// Return a dummy test entity (test-only).
    pub fn dummy() -> Self {
        Self {
            digest: "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6".to_string(),
            network: "preview".to_string(),
            beacon: CardanoDbBeacon {
                epoch: Epoch(86),
                immutable_file_number: 1728,
            },
            certificate_hash: "d5daf6c03ace4a9c074e951844075b9b373bafc4e039160e3e2af01823e9abfb"
                .to_string(),
            size: 807803196,
            created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
            locations: vec!["https://host/certificate.tar.gz".to_string()],
            compression_algorithm: CompressionAlgorithm::default(),
            cardano_node_version: "0.0.1".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const CURRENT_JSON: &str = r#"[{
        "digest": "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6",
        "network": "preview",
        "beacon": {
            "epoch": 86,
            "immutable_file_number": 1728
        },
        "certificate_hash": "d5daf6c03ace4a9c074e951844075b9b373bafc4e039160e3e2af01823e9abfb",
        "size": 807803196,
        "created_at": "2023-01-19T13:43:05.618857482Z",
        "locations": [
            "https://host/certificate.tar.gz"
        ],
        "compression_algorithm": "zstandard",
        "cardano_node_version": "1.0.0"
    }]"#;

    fn golden_message_current() -> SnapshotListMessage {
        vec![SnapshotListItemMessage {
            digest: "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6".to_string(),
            network: "preview".to_string(),
            beacon: CardanoDbBeacon {
                epoch: Epoch(86),
                immutable_file_number: 1728,
            },
            certificate_hash: "d5daf6c03ace4a9c074e951844075b9b373bafc4e039160e3e2af01823e9abfb"
                .to_string(),
            size: 807803196,
            created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
            locations: vec!["https://host/certificate.tar.gz".to_string()],
            compression_algorithm: CompressionAlgorithm::Zstandard,
            cardano_node_version: "1.0.0".to_string(),
        }]
    }

    #[test]
    fn test_current_json_deserialized_into_current_message() {
        let json = CURRENT_JSON;
        let message: SnapshotListMessage = serde_json::from_str(json).unwrap();

        assert_eq!(golden_message_current(), message);
    }
}
