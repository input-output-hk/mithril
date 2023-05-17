use crate::{entities::Beacon, signable_builder::Artifact};
use serde::{Deserialize, Serialize};

/// Snapshot represents a snapshot file and its metadata
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Snapshot {
    /// Digest that is signed by the signer participants
    pub digest: String,

    /// Mithril beacon on the Cardano chain
    pub beacon: Beacon,

    /// Hash of the associated certificate
    pub certificate_hash: String,

    /// Size of the snapshot file in Bytes
    pub size: u64,

    /// Date and time at which the snapshot was created
    pub created_at: String,

    /// Locations where the binary content of the snapshot can be retrieved
    pub locations: Vec<String>,
}

impl Snapshot {
    /// Snapshot factory
    pub fn new(
        digest: String,
        beacon: Beacon,
        certificate_hash: String,
        size: u64,
        created_at: String,
        locations: Vec<String>,
    ) -> Snapshot {
        Snapshot {
            digest,
            beacon,
            certificate_hash,
            size,
            created_at,
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
