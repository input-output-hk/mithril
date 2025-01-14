use std::collections::HashSet;

use anyhow::anyhow;
use semver::Version;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use strum::EnumDiscriminants;

use crate::{
    entities::{CardanoDbBeacon, CompressionAlgorithm},
    signable_builder::Artifact,
    StdResult,
};

/// Cardano database snapshot.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CardanoDatabaseSnapshot {
    /// Unique hash of the Cardano database snapshot.
    pub hash: String,

    /// Merkle root of the Cardano database snapshot.
    pub merkle_root: String,

    /// Mithril beacon on the Cardano chain.
    pub beacon: CardanoDbBeacon,

    /// Size of the uncompressed Cardano database files.
    pub total_db_size_uncompressed: u64,

    /// Locations of the Cardano database artifacts.
    pub locations: ArtifactsLocations,

    /// Compression algorithm of the Cardano database artifacts.
    pub compression_algorithm: CompressionAlgorithm,

    /// Version of the Cardano node used to create the snapshot.
    pub cardano_node_version: String,
}

impl CardanoDatabaseSnapshot {
    /// [CardanoDatabaseSnapshot] factory
    pub fn new(
        merkle_root: String,
        beacon: CardanoDbBeacon,
        total_db_size_uncompressed: u64,
        locations: ArtifactsLocations,
        compression_algorithm: CompressionAlgorithm,
        cardano_node_version: &Version,
    ) -> Self {
        let cardano_node_version = format!("{cardano_node_version}");
        let mut cardano_database_snapshot = Self {
            hash: "".to_string(),
            merkle_root,
            beacon,
            locations,
            total_db_size_uncompressed,
            compression_algorithm,
            cardano_node_version,
        };
        cardano_database_snapshot.hash = cardano_database_snapshot.compute_hash();

        cardano_database_snapshot
    }

    /// Cardano database snapshot hash computation
    fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.beacon.epoch.to_be_bytes());
        hasher.update(self.merkle_root.as_bytes());

        hex::encode(hasher.finalize())
    }
}

/// [TemplateUri] represents an URI pattern used to build a file's location
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize)]
pub struct TemplateUri(pub String);

/// [MultiFilesUri] represents a unique location uri for multiple files
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize)]
pub enum MultiFilesUri {
    /// URI template representing several URI
    Template(TemplateUri),
}

impl MultiFilesUri {
    /// Extract a template from a list of URIs
    pub fn extract_template_from_uris(
        file_uris: Vec<String>,
        extractor: impl Fn(&str) -> StdResult<Option<String>>,
    ) -> StdResult<Option<TemplateUri>> {
        let mut templates = HashSet::new();
        for file_uri in file_uris {
            let template_uri = extractor(&file_uri)?;
            template_uri.map(|template| templates.insert(template));
        }

        if templates.len() > 1 {
            return Err(anyhow!("Multiple templates found in the file URIs"));
        }

        if let Some(template) = templates.into_iter().next() {
            Ok(Some(TemplateUri(template)))
        } else {
            Ok(None)
        }
    }
}

/// Locations of the immutable file digests.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "type")]
pub enum DigestLocation {
    /// Aggregator digest route location.
    Aggregator {
        /// URI of the aggregator digests route location.
        uri: String,
    },
    /// Cloud storage location.
    CloudStorage {
        /// URI of the cloud storage location.
        uri: String,
    },
}

/// Locations of the immutable files.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "type")]
pub enum ImmutablesLocation {
    /// Cloud storage location.
    CloudStorage {
        /// URI of the cloud storage location.
        uri: MultiFilesUri,
    },
}

/// Locations of the ancillary files.
#[derive(
    Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, EnumDiscriminants,
)]
#[serde(rename_all = "snake_case", tag = "type")]
pub enum AncillaryLocation {
    /// Cloud storage location.
    CloudStorage {
        /// URI of the cloud storage location.
        uri: String,
    },
}

/// Locations of the Cardano database related files.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArtifactsLocations {
    /// Locations of the immutable files digests.
    pub digests: Vec<DigestLocation>,

    /// Locations of the immutable files.
    pub immutables: Vec<ImmutablesLocation>,

    /// Locations of the ancillary files.
    pub ancillary: Vec<AncillaryLocation>,
}

#[typetag::serde]
impl Artifact for CardanoDatabaseSnapshot {
    fn get_id(&self) -> String {
        self.hash.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dummy() -> CardanoDatabaseSnapshot {
        CardanoDatabaseSnapshot::new(
            "mk-root-1111111111".to_string(),
            CardanoDbBeacon::new(2222, 55555),
            0,
            ArtifactsLocations {
                digests: vec![],
                immutables: vec![],
                ancillary: vec![],
            },
            CompressionAlgorithm::Gzip,
            &Version::new(1, 0, 0),
        )
    }

    mod cardano_database_snapshot_compute_hash {
        use super::*;

        #[test]
        fn test_cardano_database_snapshot_compute_hash() {
            let cardano_database_snapshot = CardanoDatabaseSnapshot {
                merkle_root: "mk-root-123".to_string(),
                beacon: CardanoDbBeacon::new(123, 98),
                ..dummy()
            };

            assert_eq!(
                "b1cc5e0deaa7856e8e811e349d6e639fa667aa70288602955f438c5893ce29c8",
                cardano_database_snapshot.compute_hash()
            );
        }

        #[test]
        fn compute_hash_returns_same_hash_with_same_cardano_database_snapshot() {
            assert_eq!(
                CardanoDatabaseSnapshot {
                    merkle_root: "mk-root-123".to_string(),
                    beacon: CardanoDbBeacon::new(123, 98),
                    ..dummy()
                }
                .compute_hash(),
                CardanoDatabaseSnapshot {
                    merkle_root: "mk-root-123".to_string(),
                    beacon: CardanoDbBeacon::new(123, 98),
                    ..dummy()
                }
                .compute_hash()
            );
        }

        #[test]
        fn compute_hash_returns_different_hash_with_different_merkle_root() {
            assert_ne!(
                CardanoDatabaseSnapshot {
                    merkle_root: "mk-root-123".to_string(),
                    beacon: CardanoDbBeacon::new(123, 98),
                    ..dummy()
                }
                .compute_hash(),
                CardanoDatabaseSnapshot {
                    merkle_root: "mk-root-456".to_string(),
                    beacon: CardanoDbBeacon::new(123, 98),
                    ..dummy()
                }
                .compute_hash()
            );
        }

        #[test]
        fn compute_hash_returns_different_hash_with_same_epoch_in_beacon() {
            assert_eq!(
                CardanoDatabaseSnapshot {
                    merkle_root: "mk-root-123".to_string(),
                    beacon: CardanoDbBeacon::new(123, 98),
                    ..dummy()
                }
                .compute_hash(),
                CardanoDatabaseSnapshot {
                    merkle_root: "mk-root-123".to_string(),
                    beacon: CardanoDbBeacon::new(123, 12),
                    ..dummy()
                }
                .compute_hash()
            );
        }

        #[test]
        fn compute_hash_returns_different_hash_with_different_beacon() {
            assert_ne!(
                CardanoDatabaseSnapshot {
                    merkle_root: "mk-root-123".to_string(),
                    beacon: CardanoDbBeacon::new(123, 98),
                    ..dummy()
                }
                .compute_hash(),
                CardanoDatabaseSnapshot {
                    merkle_root: "mk-root-123".to_string(),
                    beacon: CardanoDbBeacon::new(456, 98),
                    ..dummy()
                }
                .compute_hash()
            );
        }
    }

    mod extract_template_from_uris {
        use super::*;

        #[test]
        fn returns_template() {
            let file_uris = vec![
                "http://whatever/00001.tar.gz".to_string(),
                "http://whatever/00002.tar.gz".to_string(),
            ];
            fn extractor_returning_same_uri(_file_uri: &str) -> StdResult<Option<String>> {
                Ok(Some(
                    "http://whatever/{immutable_file_number}.tar.gz".to_string(),
                ))
            }

            let template =
                MultiFilesUri::extract_template_from_uris(file_uris, extractor_returning_same_uri)
                    .unwrap();

            assert_eq!(
                template,
                Some(TemplateUri(
                    "http://whatever/{immutable_file_number}.tar.gz".to_string()
                ))
            );
        }

        #[test]
        fn returns_error_with_multiple_templates() {
            let file_uris = vec![
                "http://whatever/00001.tar.gz".to_string(),
                "http://00002.tar.gz/whatever".to_string(),
            ];
            fn extractor_returning_different_uri(file_uri: &str) -> StdResult<Option<String>> {
                Ok(Some(file_uri.to_string()))
            }

            MultiFilesUri::extract_template_from_uris(file_uris, extractor_returning_different_uri)
                .expect_err(
                    "Should return an error when multiple templates are found in the file URIs",
                );
        }
    }
}
