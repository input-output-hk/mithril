use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use crate::entities::{
    AncillaryLocation, ArtifactsLocations, CardanoDbBeacon, CompressionAlgorithm, DigestLocation,
    Epoch, ImmutablesLocation, MultiFilesUri, TemplateUri,
};

/// Locations of the Cardano database related files.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArtifactsLocationsMessagePart {
    /// Locations of the the immutable file digests.
    pub digests: Vec<DigestLocation>,
    /// Locations of the immutable files.
    pub immutables: Vec<ImmutablesLocation>,
    /// Locations of the ancillary files.
    pub ancillary: Vec<AncillaryLocation>,
}

impl From<ArtifactsLocations> for ArtifactsLocationsMessagePart {
    fn from(part: ArtifactsLocations) -> Self {
        Self {
            digests: part.digests,
            immutables: part.immutables,
            ancillary: part.ancillary,
        }
    }
}

/// Cardano database snapshot.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CardanoDatabaseSnapshotMessage {
    /// Hash of the Cardano database snapshot.
    pub hash: String,

    /// Merkle root of the Cardano database snapshot.
    pub merkle_root: String,

    /// Mithril beacon on the Cardano chain.
    pub beacon: CardanoDbBeacon,

    /// Hash of the associated certificate
    pub certificate_hash: String,

    /// Size of the uncompressed Cardano database files.
    pub total_db_size_uncompressed: u64,

    /// Locations of the Cardano database artifacts.
    pub locations: ArtifactsLocationsMessagePart,

    /// Compression algorithm of the Cardano database artifacts.
    pub compression_algorithm: CompressionAlgorithm,

    /// Version of the Cardano node used to create the snapshot.
    pub cardano_node_version: String,

    /// Date and time at which the snapshot was created
    pub created_at: DateTime<Utc>,
}

impl CardanoDatabaseSnapshotMessage {
    /// Return a dummy test entity (test-only).
    pub fn dummy() -> Self {
        Self {
            hash: "d4071d518a3ace0f6c04a9c0745b9e9560e3e2af1b373bafc4e0398423e9abfb".to_string(),
            merkle_root: "c8224920b9f5ad7377594eb8a15f34f08eb3103cc5241d57cafc5638403ec7c6"
                .to_string(),
            beacon: CardanoDbBeacon {
                epoch: Epoch(123),
                immutable_file_number: 2345,
            },
            certificate_hash: "f6c01b373bafc4e039844071d5da3ace4a9c0745b9e9560e3e2af01823e9abfb"
                .to_string(),
            total_db_size_uncompressed: 800796318,
            created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
            locations: ArtifactsLocationsMessagePart {
                digests: vec![DigestLocation::Aggregator {
                    uri: "https://host-1/digest-1".to_string(),
                }],
                immutables: vec![
                    ImmutablesLocation::CloudStorage {
                        uri: MultiFilesUri::Template(TemplateUri(
                            "https://host-1/immutables-2".to_string(),
                        )),
                    },
                    ImmutablesLocation::CloudStorage {
                        uri: MultiFilesUri::Template(TemplateUri(
                            "https://host-2/immutables-2".to_string(),
                        )),
                    },
                ],
                ancillary: vec![AncillaryLocation::CloudStorage {
                    uri: "https://host-1/ancillary-3".to_string(),
                }],
            },
            compression_algorithm: CompressionAlgorithm::Gzip,
            cardano_node_version: "0.0.1".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const CURRENT_JSON: &str = r#"
    {
        "hash": "d4071d518a3ace0f6c04a9c0745b9e9560e3e2af1b373bafc4e0398423e9abfb",
        "merkle_root": "c8224920b9f5ad7377594eb8a15f34f08eb3103cc5241d57cafc5638403ec7c6",
        "beacon": {
            "epoch": 123,
            "immutable_file_number": 2345
        },
        "certificate_hash": "f6c01b373bafc4e039844071d5da3ace4a9c0745b9e9560e3e2af01823e9abfb",
        "total_db_size_uncompressed": 800796318,
        "locations": {
            "digests": [
            {
                "type": "aggregator",
                "uri": "https://host-1/digest-1"
            }
            ],
            "immutables": [
            {
                "type": "cloud_storage",
                "uri": {
                    "Template": "https://host-1/immutables-{immutable_file_number}"
                }
            },
            {
                "type": "cloud_storage",
                "uri": {
                    "Template": "https://host-2/immutables-{immutable_file_number}"
                }
            }
            ],
            "ancillary": [
            {
                "type": "cloud_storage",
                "uri": "https://host-1/ancillary-3"
            }
            ]
        },
        "compression_algorithm": "gzip",
        "cardano_node_version": "0.0.1",
        "created_at": "2023-01-19T13:43:05.618857482Z"
    }"#;

    fn golden_current_message() -> CardanoDatabaseSnapshotMessage {
        CardanoDatabaseSnapshotMessage {
            hash: "d4071d518a3ace0f6c04a9c0745b9e9560e3e2af1b373bafc4e0398423e9abfb".to_string(),
            merkle_root: "c8224920b9f5ad7377594eb8a15f34f08eb3103cc5241d57cafc5638403ec7c6"
                .to_string(),
            beacon: CardanoDbBeacon {
                epoch: Epoch(123),
                immutable_file_number: 2345,
            },
            certificate_hash: "f6c01b373bafc4e039844071d5da3ace4a9c0745b9e9560e3e2af01823e9abfb"
                .to_string(),
            total_db_size_uncompressed: 800796318,
            created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
            locations: ArtifactsLocationsMessagePart {
                digests: vec![DigestLocation::Aggregator {
                    uri: "https://host-1/digest-1".to_string(),
                }],
                immutables: vec![
                    ImmutablesLocation::CloudStorage {
                        uri: MultiFilesUri::Template(TemplateUri(
                            "https://host-1/immutables-{immutable_file_number}".to_string(),
                        )),
                    },
                    ImmutablesLocation::CloudStorage {
                        uri: MultiFilesUri::Template(TemplateUri(
                            "https://host-2/immutables-{immutable_file_number}".to_string(),
                        )),
                    },
                ],
                ancillary: vec![AncillaryLocation::CloudStorage {
                    uri: "https://host-1/ancillary-3".to_string(),
                }],
            },
            compression_algorithm: CompressionAlgorithm::Gzip,
            cardano_node_version: "0.0.1".to_string(),
        }
    }

    #[test]
    fn test_current_json_deserialized_into_current_message() {
        let json = CURRENT_JSON;
        let message: CardanoDatabaseSnapshotMessage = serde_json::from_str(json).expect(
            "This JSON is expected to be successfully parsed into a CardanoDatabaseSnapshotMessage instance.",
        );

        assert_eq!(golden_current_message(), message);
    }
}
