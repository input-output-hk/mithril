use serde::{Deserialize, Serialize};

use crate::entities::{CardanoDbBeacon, CompressionAlgorithm};
use crate::messages::SnapshotMessage;

/// Message structure of a snapshot
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SnapshotDownloadMessage {
    /// Digest that is signed by the signer participants
    pub digest: String,

    /// Cardano network
    pub network: String,

    /// Mithril beacon on the Cardano chain
    pub beacon: CardanoDbBeacon,

    /// Size of the immutables snapshot file in Bytes
    pub size: u64,

    /// Size of the ancillary files in Bytes
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ancillary_size: Option<u64>,

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

impl From<SnapshotMessage> for SnapshotDownloadMessage {
    fn from(message: SnapshotMessage) -> Self {
        Self {
            digest: message.digest,
            network: message.network,
            beacon: message.beacon,
            size: message.size,
            ancillary_size: message.ancillary_size,
            locations: message.locations,
            ancillary_locations: message.ancillary_locations,
            compression_algorithm: message.compression_algorithm,
            cardano_node_version: message.cardano_node_version,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::entities::Epoch;
    use crate::test::double::Dummy;

    use super::*;

    #[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
    pub struct SnapshotDownloadMessageUntilV0_1_47 {
        pub digest: String,
        pub network: String,
        pub beacon: CardanoDbBeacon,
        pub size: u64,
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
        "size": 807803196,
        "ancillary_size": 123456789,
        "locations": [
            "https://host/certificate.tar.gz"
        ],
        "ancillary_locations": [
            "https://host/ancillary.tar.gz"
        ],
        "compression_algorithm": "gzip",
        "cardano_node_version": "0.0.1"
    }"#;

    fn golden_message_until_open_api_0_1_47() -> SnapshotDownloadMessageUntilV0_1_47 {
        SnapshotDownloadMessageUntilV0_1_47 {
            digest: "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6".to_string(),
            network: "preview".to_string(),
            beacon: CardanoDbBeacon {
                epoch: Epoch(86),
                immutable_file_number: 1728,
            },
            size: 807803196,
            locations: vec!["https://host/certificate.tar.gz".to_string()],
            compression_algorithm: CompressionAlgorithm::Gzip,
            cardano_node_version: "0.0.1".to_string(),
        }
    }

    fn golden_message_current() -> SnapshotDownloadMessage {
        SnapshotDownloadMessage {
            digest: "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6".to_string(),
            network: "preview".to_string(),
            beacon: CardanoDbBeacon {
                epoch: Epoch(86),
                immutable_file_number: 1728,
            },
            size: 807803196,
            ancillary_size: Some(123456789),
            locations: vec!["https://host/certificate.tar.gz".to_string()],
            ancillary_locations: Some(vec!["https://host/ancillary.tar.gz".to_string()]),
            compression_algorithm: CompressionAlgorithm::Gzip,
            cardano_node_version: "0.0.1".to_string(),
        }
    }

    #[test]
    fn test_current_json_deserialized_into_message_supported_until_open_api_0_1_47() {
        let json = CURRENT_JSON;
        let message: SnapshotDownloadMessageUntilV0_1_47 = serde_json::from_str(json).unwrap();

        assert_eq!(golden_message_until_open_api_0_1_47(), message);
    }

    #[test]
    fn test_current_json_deserialized_into_current_message() {
        let json = CURRENT_JSON;
        let message: SnapshotDownloadMessage = serde_json::from_str(json).unwrap();

        assert_eq!(golden_message_current(), message);
    }

    #[test]
    fn convert_from_snapshot_message() {
        let snapshot_message = SnapshotMessage::dummy();
        let snapshot_download_message = SnapshotDownloadMessage::from(snapshot_message.clone());

        assert_eq!(
            snapshot_download_message,
            SnapshotDownloadMessage {
                digest: snapshot_message.digest,
                network: snapshot_message.network,
                beacon: snapshot_message.beacon,
                size: snapshot_message.size,
                ancillary_size: snapshot_message.ancillary_size,
                locations: snapshot_message.locations,
                ancillary_locations: snapshot_message.ancillary_locations,
                compression_algorithm: snapshot_message.compression_algorithm,
                cardano_node_version: snapshot_message.cardano_node_version,
            }
        )
    }
}
