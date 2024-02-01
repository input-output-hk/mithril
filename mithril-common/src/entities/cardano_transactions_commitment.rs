use crate::signable_builder::Artifact;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use super::Beacon;

/// Commitment of a set of Cardano transactions
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct CardanoTransactionsCommitment {
    /// Hash of the Cardano transactions set
    pub hash: String,

    /// Merkle root of the Cardano transactions set
    pub merkle_root: String,

    /// Beacon of the Cardano transactions set
    pub beacon: Beacon,
}

impl CardanoTransactionsCommitment {
    /// Creates a new [CardanoTransactionsCommitment]
    pub fn new(merkle_root: String, beacon: Beacon) -> Self {
        let mut cardano_transactions_commitment = Self {
            merkle_root,
            beacon,
            hash: "".to_string(),
        };
        cardano_transactions_commitment.hash = cardano_transactions_commitment.compute_hash();
        cardano_transactions_commitment
    }

    /// Cardano transactions commitment hash computation
    fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.merkle_root.clone().as_bytes());
        hasher.update(self.beacon.compute_hash().as_bytes());

        hex::encode(hasher.finalize())
    }
}

#[typetag::serde]
impl Artifact for CardanoTransactionsCommitment {
    fn get_id(&self) -> String {
        self.hash.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cardano_transactuions_commitment_compute_hash() {
        let hash_expected = "66a1d7aa3995e9a0dce15fae3f6b91640824ecd1f81991df5ce4ddff62b34df4";

        assert_eq!(
            hash_expected,
            CardanoTransactionsCommitment::new("mk-root-123".to_string(), Beacon::default())
                .compute_hash()
        );

        assert_ne!(
            hash_expected,
            CardanoTransactionsCommitment::new("mk-root-456".to_string(), Beacon::default())
                .compute_hash()
        );
    }
}
