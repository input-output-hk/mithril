use crate::{entities::Beacon, signable_builder::Artifact};
use semver::Version;
use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter, IntoEnumIterator};

/// Snapshot represents a snapshot file and its metadata
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Snapshot {
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

    /// Version of the Cardano node used to create snapshot archive.
    pub cardano_node_version: String,
}

/// Compression algorithm for the snapshot archive artifacts.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Default, EnumIter, Display)]
#[serde(rename_all = "lowercase")]
pub enum CompressionAlgorithm {
    /// Gzip compression format
    #[default]
    Gzip,
    /// Zstandard compression format
    Zstandard,
}

impl CompressionAlgorithm {
    /// Get the extension associated to tar archive using the current algorithm.
    pub fn tar_file_extension(&self) -> String {
        match self {
            CompressionAlgorithm::Gzip => "tar.gz".to_owned(),
            CompressionAlgorithm::Zstandard => "tar.zst".to_owned(),
        }
    }

    /// List all the available [algorithms][CompressionAlgorithm].
    pub fn list() -> Vec<Self> {
        Self::iter().collect()
    }

    /// Those ratio will be multiplied by the snapshot size to check if the available
    /// disk space is sufficient to store the archive plus the extracted files.
    /// If the available space is lower than that, a warning is raised.
    /// Those ratio have been experimentally established.
    pub fn free_space_snapshot_ratio(&self) -> f64 {
        match self {
            CompressionAlgorithm::Gzip => 2.5,
            CompressionAlgorithm::Zstandard => 4.0,
        }
    }
}

impl Snapshot {
    /// Snapshot factory
    pub fn new(
        digest: String,
        beacon: Beacon,
        size: u64,
        locations: Vec<String>,
        compression_algorithm: CompressionAlgorithm,
        cardano_node_version: &Version,
    ) -> Snapshot {
        let cardano_node_version = format!("{cardano_node_version}");

        Snapshot {
            digest,
            beacon,
            size,
            locations,
            compression_algorithm,
            cardano_node_version,
        }
    }
}

#[typetag::serde]
impl Artifact for Snapshot {
    fn get_id(&self) -> String {
        self.digest.clone()
    }
}
