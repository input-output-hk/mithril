use serde::{Deserialize, Serialize};

use crate::entities::{Beacon, CompressionAlgorithm, Epoch};

/// Message structure of a snapshot
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct SnapshotDownloadMessage {
    /// Digest that is signed by the signer participants
    pub digest: String,

    /// Mithril beacon on the Cardano chain
    pub beacon: Beacon,

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
            beacon: Beacon {
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

    fn golden_message_v1() -> SnapshotDownloadMessage {
        SnapshotDownloadMessage {
            digest: "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6".to_string(),
            beacon: Beacon {
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
"size": 807803196,
"locations": [
  "https://host/certificate.tar.gz"
],
"compression_algorithm": "gzip",
"cardano_node_version": "0.0.1"
}
"#;
        let message: SnapshotDownloadMessage = serde_json::from_str(json).expect(
            "This JSON is expected to be succesfully parsed into a SnapshotDownloadMessage instance.",
        );

        assert_eq!(golden_message_v1(), message);
    }
}
