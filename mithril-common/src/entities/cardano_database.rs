use semver::Version;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use strum::EnumDiscriminants;

use crate::{
    entities::{CardanoDbBeacon, CompressionAlgorithm},
    signable_builder::Artifact,
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

/// Locations of the the immutable file digests.
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

/// Locations of the ancillary files.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "type")]
pub enum ImmutablesLocation {
    /// Cloud storage location.
    CloudStorage {
        /// URI of the cloud storage location.
        uri: String,
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
