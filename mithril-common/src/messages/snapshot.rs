use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use crate::entities::{CardanoDbBeacon, CompressionAlgorithm};

/// Message structure of a snapshot
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SnapshotMessage {
    /// Digest that is signed by the signer participants
    pub digest: String,

    /// Cardano network
    pub network: String,

    /// Mithril beacon on the Cardano chain
    pub beacon: CardanoDbBeacon,

    /// Hash of the associated certificate
    pub certificate_hash: String,

    /// Size of the immutables snapshot file in Bytes
    pub size: u64,

    /// Size of the ancillary files in Bytes
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ancillary_size: Option<u64>,

    /// Date and time at which the snapshot was created
    pub created_at: DateTime<Utc>,

    /// Locations where the snapshot of the immutable files can be retrieved
    pub locations: Vec<String>,

    /// Locations where the snapshot of the ancillary files can be retrieved
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ancillary_locations: Option<Vec<String>>,

    /// Compression algorithm of the snapshot archive
    pub compression_algorithm: CompressionAlgorithm,

    /// Cardano node version
    pub cardano_node_version: String,
}

impl SnapshotMessage {
    /// Compute the total size of the snapshot including ancillary files
    pub fn compute_total_size(&self) -> u64 {
        self.size + self.ancillary_size.unwrap_or(0)
    }
}

#[cfg(test)]
mod tests {
    use crate::entities::Epoch;
    use crate::test::double::Dummy;

    use super::*;

    #[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
    pub struct SnapshotMessageUntilV0_1_47 {
        pub digest: String,
        pub network: String,
        pub beacon: CardanoDbBeacon,
        pub certificate_hash: String,
        pub size: u64,
        pub created_at: DateTime<Utc>,
        pub locations: Vec<String>,
        pub compression_algorithm: CompressionAlgorithm,
        pub cardano_node_version: String,
    }

    const CURRENT_JSON: &str = r#"{
        "digest": "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6",
        "network": "preview",
        "beacon": {
            "epoch": 86,
            "immutable_file_number": 1728
        },
        "certificate_hash": "d5daf6c03ace4a9c074e951844075b9b373bafc4e039160e3e2af01823e9abfb",
        "size": 807803196,
        "ancillary_size": 123456789,
        "created_at": "2023-01-19T13:43:05.618857482Z",
        "locations": [
            "https://host/certificate.tar.gz"
        ],
        "ancillary_locations": [
            "https://host/ancillary.tar.gz"
        ],
        "compression_algorithm": "gzip",
        "cardano_node_version": "0.0.1"
    }"#;

    fn golden_message_until_open_api_0_1_47() -> SnapshotMessageUntilV0_1_47 {
        SnapshotMessageUntilV0_1_47 {
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
            compression_algorithm: CompressionAlgorithm::Gzip,
            cardano_node_version: "0.0.1".to_string(),
        }
    }

    fn golden_message_current() -> SnapshotMessage {
        SnapshotMessage {
            digest: "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6".to_string(),
            network: "preview".to_string(),
            beacon: CardanoDbBeacon {
                epoch: Epoch(86),
                immutable_file_number: 1728,
            },
            certificate_hash: "d5daf6c03ace4a9c074e951844075b9b373bafc4e039160e3e2af01823e9abfb"
                .to_string(),
            size: 807803196,
            ancillary_size: Some(123456789),
            created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
            locations: vec!["https://host/certificate.tar.gz".to_string()],
            ancillary_locations: Some(vec!["https://host/ancillary.tar.gz".to_string()]),
            compression_algorithm: CompressionAlgorithm::Gzip,
            cardano_node_version: "0.0.1".to_string(),
        }
    }

    #[test]
    fn test_current_json_deserialized_into_message_supported_until_open_api_0_1_47() {
        let json = CURRENT_JSON;
        let message: SnapshotMessageUntilV0_1_47 = serde_json::from_str(json).unwrap();

        assert_eq!(golden_message_until_open_api_0_1_47(), message);
    }

    #[test]
    fn test_current_json_deserialized_into_current_message() {
        let json = CURRENT_JSON;
        let message: SnapshotMessage = serde_json::from_str(json).unwrap();

        assert_eq!(golden_message_current(), message);
    }

    #[test]
    fn compute_total_size_with_ancillary_size() {
        let message = SnapshotMessage {
            size: 12,
            ancillary_size: Some(33),
            ..SnapshotMessage::dummy()
        };

        assert_eq!(message.compute_total_size(), 45);
    }

    #[test]
    fn compute_total_size_without_ancillary_size() {
        let message = SnapshotMessage {
            size: 12,
            ancillary_size: None,
            ..SnapshotMessage::dummy()
        };

        assert_eq!(message.compute_total_size(), 12);
    }
}
