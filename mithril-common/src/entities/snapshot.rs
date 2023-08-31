use crate::{entities::Beacon, signable_builder::Artifact};
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
}

/// Compression algorithm for the snapshot archive artifacts.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Default, EnumIter, Display)]
#[serde(rename_all = "lowercase")]
pub enum CompressionAlgorithm {
    /// Gunzip compression format
    #[default]
    Gunzip,
    /// Zstandard compression format
    Zstandard,
}

impl CompressionAlgorithm {
    /// Get the extension associated to tar archive using the current algorithm.
    pub fn tar_file_extension(&self) -> String {
        match self {
            CompressionAlgorithm::Gunzip => "tar.gz".to_owned(),
            CompressionAlgorithm::Zstandard => "tar.zst".to_owned(),
        }
    }

    /// List all the available [algorithms][CompressionAlgorithm].
    pub fn list() -> Vec<Self> {
        Self::iter().collect()
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
    ) -> Snapshot {
        Snapshot {
            digest,
            beacon,
            size,
            locations,
            compression_algorithm,
        }
    }
}

#[typetag::serde]
impl Artifact for Snapshot {
    fn get_id(&self) -> String {
        self.digest.clone()
    }
}
