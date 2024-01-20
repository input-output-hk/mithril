use serde::{Deserialize, Serialize};

use crate::signable_builder::Artifact;

/// Commitment of a set of Cardano transactions
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct CardanoTransactionsCommitment {
    merkle_root: String,
}

impl CardanoTransactionsCommitment {
    /// Creates a new [CardanoTransactionsCommitment]
    pub fn new(merkle_root: String) -> Self {
        Self { merkle_root }
    }
}

#[typetag::serde]
impl Artifact for CardanoTransactionsCommitment {
    fn get_id(&self) -> String {
        self.merkle_root.clone()
    }
}
