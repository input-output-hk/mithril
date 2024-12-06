use semver::Version;
use serde::{Deserialize, Serialize};

use crate::{
    entities::{CardanoDbBeacon, CompressionAlgorithm},
    signable_builder::Artifact,
};

/// Cardano database snapshot.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CardanoDatabaseSnapshot {
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

        Self {
            merkle_root,
            beacon,
            locations,
            total_db_size_uncompressed,
            compression_algorithm,
            cardano_node_version,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "type")]
enum DigestLocation {
    Aggregator { uri: String },
    CloudStorage { uri: String },
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "type")]
enum ImmutablesLocation {
    CloudStorage { uri: String },
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "type")]
enum AncillaryLocation {
    CloudStorage { uri: String },
}

/// Locations of the Cardano database related files.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArtifactsLocations {
    /// Locations of the the immutable file digests.
    digest: Vec<DigestLocation>,
    /// Locations of the immutable files.
    immutables: Vec<ImmutablesLocation>,
    /// Locations of the ancillary files.
    ancillary: Vec<AncillaryLocation>,
}

#[typetag::serde]
impl Artifact for CardanoDatabaseSnapshot {
    fn get_id(&self) -> String {
        self.merkle_root.clone()
    }
}
