use crate::signable_builder::Artifact;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use super::Beacon;

/// Commitment of a set of Cardano transactions
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct CardanoTransactionsCommitment {
    merkle_root: String,
    beacon: Beacon,
}

impl CardanoTransactionsCommitment {
    /// Creates a new [CardanoTransactionsCommitment]
    pub fn new(merkle_root: String, beacon: Beacon) -> Self {
        Self {
            merkle_root,
            beacon,
        }
    }
}

#[typetag::serde]
impl Artifact for CardanoTransactionsCommitment {
    fn get_id(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.merkle_root.clone().as_bytes());
        hasher.update(self.beacon.compute_hash().as_bytes());
        hex::encode(hasher.finalize())
    }
}
