use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::signable_builder::Artifact;

use super::BlockNumber;

/// Snapshot of a set of Cardano transactions
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CardanoTransactionsSnapshot {
    /// Hash of the Cardano transactions set
    pub hash: String,

    /// Merkle root of the Cardano transactions set
    pub merkle_root: String,

    /// Beacon of the Cardano transactions set
    pub block_number: BlockNumber,
}

impl CardanoTransactionsSnapshot {
    /// Creates a new [CardanoTransactionsSnapshot]
    pub fn new(merkle_root: String, block_number: BlockNumber) -> Self {
        let mut cardano_transactions_snapshot = Self {
            merkle_root,
            block_number,
            hash: "".to_string(),
        };
        cardano_transactions_snapshot.hash = cardano_transactions_snapshot.compute_hash();
        cardano_transactions_snapshot
    }

    /// Cardano transactions snapshot hash computation
    fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.merkle_root.clone().as_bytes());
        hasher.update(self.block_number.to_be_bytes());

        hex::encode(hasher.finalize())
    }
}

#[typetag::serde]
impl Artifact for CardanoTransactionsSnapshot {
    fn get_id(&self) -> String {
        self.hash.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cardano_transactions_snapshot_compute_hash() {
        let hash_expected = "fac28cf7aa07922258d7d3ad8b16d859d8a3b812822b90050cddbc2e6671aab5";

        assert_eq!(
            hash_expected,
            CardanoTransactionsSnapshot::new("mk-root-123".to_string(), 50).compute_hash()
        );

        assert_ne!(
            hash_expected,
            CardanoTransactionsSnapshot::new("mk-root-456".to_string(), 50).compute_hash()
        );

        assert_ne!(
            hash_expected,
            CardanoTransactionsSnapshot::new("mk-root-123".to_string(), 47).compute_hash()
        );
    }
}
