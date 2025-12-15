use serde::{Deserialize, Serialize};

use crate::entities::{CardanoDbBeacon, CompressionAlgorithm};

/// Snapshot represents a snapshot file and its metadata
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Snapshot {
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

    /// Version of the Cardano node used to create snapshot archive.
    pub cardano_node_version: String,
}
