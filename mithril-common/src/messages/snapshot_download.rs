use serde::{Deserialize, Serialize};

use crate::entities::{CardanoDbBeacon, CompressionAlgorithm, Epoch};

/// Message structure of a snapshot
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct SnapshotDownloadMessage {
    /// Digest that is signed by the signer participants
    pub digest: String,

    /// Cardano network
    pub network: String,

    /// Mithril beacon on the Cardano chain
    pub beacon: CardanoDbBeacon,

    /// Size of the snapshot file in Bytes
    pub size: u64,

    /// Locations where the binary content of the snapshot can be retrieved
    pub locations: Vec<String>,

    /// Compression algorithm of the snapshot archive
    pub compression_algorithm: CompressionAlgorithm,

    /// Cardano node version
    pub cardano_node_version: String,
}

impl SnapshotDownloadMessage {
    /// Return a dummy test entity (test-only).
    pub fn dummy() -> Self {
        Self {
            digest: "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6".to_string(),
            network: "preview".to_string(),
            beacon: CardanoDbBeacon {
                network: "preview".to_string(),
                epoch: Epoch(86),
                immutable_file_number: 1728,
            },
            size: 807803196,
            locations: vec!["https://host/certificate.tar.gz".to_string()],
            compression_algorithm: CompressionAlgorithm::Gzip,
            cardano_node_version: "0.0.1".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const ACTUAL_JSON: &str = r#"{
        "digest": "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6",
        "network": "preview",
        "beacon": {
            "network": "preview",
            "epoch": 86,
            "immutable_file_number": 1728
        },
        "size": 807803196,
        "locations": [
            "https://host/certificate.tar.gz"
        ],
        "compression_algorithm": "gzip",
        "cardano_node_version": "0.0.1"
    }"#;

    fn golden_message_actual() -> SnapshotDownloadMessage {
        SnapshotDownloadMessage {
            digest: "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6".to_string(),
            network: "preview".to_string(),
            beacon: CardanoDbBeacon {
                network: "preview".to_string(),
                epoch: Epoch(86),
                immutable_file_number: 1728,
            },
            size: 807803196,
            locations: vec!["https://host/certificate.tar.gz".to_string()],
            compression_algorithm: CompressionAlgorithm::Gzip,
            cardano_node_version: "0.0.1".to_string(),
        }
    }

    #[test]
    fn test_actual_json_deserialized_into_actual_message() {
        let json = ACTUAL_JSON;
        let message: SnapshotDownloadMessage = serde_json::from_str(json).unwrap();

        assert_eq!(golden_message_actual(), message);
    }
}
