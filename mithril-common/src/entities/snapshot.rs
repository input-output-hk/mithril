use crate::{entities::Beacon, signable_builder::Artifact};
use serde::{Deserialize, Serialize};

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
}

impl Snapshot {
    /// Snapshot factory
    pub fn new(digest: String, beacon: Beacon, size: u64, locations: Vec<String>) -> Snapshot {
        Snapshot {
            digest,
            beacon,
            size,
            locations,
        }
    }
}

#[typetag::serde]
impl Artifact for Snapshot {
    fn get_id(&self) -> String {
        self.digest.clone()
    }
}
