use semver::Version;
use serde::{Deserialize, Serialize};

use crate::{
    entities::{CardanoDbBeacon, CompressionAlgorithm},
    signable_builder::Artifact,
};

/// Cardano database incremental.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CardanoDatabase {
    /// Merkle root of the Cardano database.
    pub merkle_root: String,

    /// Mithril beacon on the Cardano chain.
    pub beacon: CardanoDbBeacon,

    /// Size of the uncompressed Cardano database (including the ledger and volatile) in Bytes.
    pub total_db_size_uncompressed: u64,

    /// Locations of the Cardano database artifacts.
    pub locations: ArtifactsLocations,

    /// Compression algorithm of the Cardano database archives
    pub compression_algorithm: CompressionAlgorithm,

    /// Version of the Cardano node used to create the archives.
    pub cardano_node_version: String,
}

impl CardanoDatabase {
    /// [CardanoDatabase] factory
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
#[serde(rename_all = "snake_case")]
pub enum ArtifactLocationType {
    Aggregator,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArtifactLocationEntry {
    #[serde(rename = "type")]
    pub location_type: ArtifactLocationType,
    pub uri: String,
}

/// Locations of the Cardano database related files.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArtifactsLocations {
    /// Locations of the file containing the digests of the immutable files.
    pub digests: Vec<ArtifactLocationEntry>,
    /// Locations of the immutable files.
    pub immutables: Vec<ArtifactLocationEntry>,
    /// Locations of the ancillary files (ledger and volatile).
    pub ancillary: Vec<ArtifactLocationEntry>,
}

#[typetag::serde]
impl Artifact for CardanoDatabase {
    fn get_id(&self) -> String {
        self.merkle_root.clone()
    }
}
