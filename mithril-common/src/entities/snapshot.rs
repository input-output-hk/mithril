use crate::entities::{CardanoDbBeacon, CompressionAlgorithm};
use semver::Version;
use serde::{Deserialize, Serialize};

/// Snapshot represents a snapshot file and its metadata
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Snapshot {
    /// Digest that is signed by the signer participants
    pub digest: String,

    /// Cardano network
    pub network: String,

    /// Mithril beacon on the Cardano chain
    pub beacon: CardanoDbBeacon,

    /// Size of the snapshot file in Bytes
    pub size: u64,

    /// Locations where the binary content of the snapshot can be retrieved
    pub locations: Vec<String>,

    /// Compression algorithm of the snapshot archive
    pub compression_algorithm: CompressionAlgorithm,

    /// Version of the Cardano node used to create snapshot archive.
    pub cardano_node_version: String,
}

impl Snapshot {
    /// Snapshot factory
    pub fn new<N: Into<String>>(
        digest: String,
        network: N,
        beacon: CardanoDbBeacon,
        size: u64,
        locations: Vec<String>,
        compression_algorithm: CompressionAlgorithm,
        cardano_node_version: &Version,
    ) -> Snapshot {
        let cardano_node_version = format!("{cardano_node_version}");

        Snapshot {
            digest,
            network: network.into(),
            beacon,
            size,
            locations,
            compression_algorithm,
            cardano_node_version,
        }
    }
}
