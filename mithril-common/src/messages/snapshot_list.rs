use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use crate::entities::{CompressionAlgorithm, Epoch};
use crate::messages::CardanoDbBeaconMessagePart;

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
    pub beacon: CardanoDbBeaconMessagePart,

    /// Hash of the associated certificate
    pub certificate_hash: String,

    /// Size of the snapshot file in Bytes
    pub size: u64,

    /// Date and time at which the snapshot was created
    pub created_at: DateTime<Utc>,

    /// Locations where the binary content of the snapshot can be retrieved
    pub locations: Vec<String>,

    /// Compression algorithm of the snapshot archive
    #[serde(skip_serializing_if = "Option::is_none")]
    pub compression_algorithm: Option<CompressionAlgorithm>,

    /// Cardano node version
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cardano_node_version: Option<String>,
}

impl SnapshotListItemMessage {
    /// Return a dummy test entity (test-only).
    pub fn dummy() -> Self {
        Self {
            digest: "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6".to_string(),
            network: "preview".to_string(),
            beacon: CardanoDbBeaconMessagePart {
                network: Some("preview".to_string()),
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
            compression_algorithm: Some(CompressionAlgorithm::default()),
            cardano_node_version: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const ACTUAL_JSON: &str = r#"[{
        "digest": "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6",
        "network": "preview",
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
        ],
        "compression_algorithm": "zstandard",
        "cardano_node_version": "1.0.0"
    }]"#;

    pub type SnapshotListMessageUntilV0_1_35 = Vec<SnapshotListItemMessageUntilV0_1_35>;

    #[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
    pub struct SnapshotListItemMessageUntilV0_1_35 {
        pub digest: String,
        pub beacon: CardanoDbBeaconMessagePart,
        pub certificate_hash: String,
        pub size: u64,
        pub created_at: DateTime<Utc>,
        pub locations: Vec<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        pub compression_algorithm: Option<CompressionAlgorithm>,
        #[serde(skip_serializing_if = "Option::is_none")]
        pub cardano_node_version: Option<String>,
    }

    fn golden_message_until_open_api_0_1_35() -> SnapshotListMessageUntilV0_1_35 {
        vec![SnapshotListItemMessageUntilV0_1_35 {
            digest: "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6".to_string(),
            beacon: CardanoDbBeaconMessagePart {
                network: Some("preview".to_string()),
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
            compression_algorithm: Some(CompressionAlgorithm::Zstandard),
            cardano_node_version: Some("1.0.0".to_string()),
        }]
    }

    fn golden_message_actual() -> SnapshotListMessage {
        vec![SnapshotListItemMessage {
            digest: "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6".to_string(),
            network: "preview".to_string(),
            beacon: CardanoDbBeaconMessagePart {
                network: Some("preview".to_string()),
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
            compression_algorithm: Some(CompressionAlgorithm::Zstandard),
            cardano_node_version: Some("1.0.0".to_string()),
        }]
    }

    #[test]
    fn test_actual_json_deserialized_into_message_supported_until_open_api_0_1_35() {
        let json = ACTUAL_JSON;
        let message: SnapshotListMessageUntilV0_1_35 = serde_json::from_str(json).unwrap();

        assert_eq!(golden_message_until_open_api_0_1_35(), message);
    }

    #[test]
    fn test_actual_json_deserialized_into_actual_message() {
        let json = ACTUAL_JSON;
        let message: SnapshotListMessage = serde_json::from_str(json).unwrap();

        assert_eq!(golden_message_actual(), message);
    }
}
