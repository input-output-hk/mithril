use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::signable_builder::Artifact;

use super::ChainPoint;

/// Snapshot of a set of Cardano transactions
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CardanoTransactionsSnapshot {
    /// Hash of the Cardano transactions set
    pub hash: String,

    /// Merkle root of the Cardano transactions set
    pub merkle_root: String,

    /// Beacon of the Cardano transactions set
    pub chain_point: ChainPoint,
}

impl CardanoTransactionsSnapshot {
    /// Creates a new [CardanoTransactionsSnapshot]
    pub fn new(merkle_root: String, chain_point: ChainPoint) -> Self {
        let mut cardano_transactions_snapshot = Self {
            merkle_root,
            chain_point,
            hash: "".to_string(),
        };
        cardano_transactions_snapshot.hash = cardano_transactions_snapshot.compute_hash();
        cardano_transactions_snapshot
    }

    /// Cardano transactions snapshot hash computation
    fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.merkle_root.clone().as_bytes());
        self.chain_point.feed_hash(&mut hasher);

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
        let hash_expected = "701a0fec8e7c6afca90f3f94b1f912ba6e179d891023465858abcad7db108730";

        assert_eq!(
            hash_expected,
            CardanoTransactionsSnapshot::new(
                "mk-root-123".to_string(),
                ChainPoint::new(100, 50, "block_hash-50")
            )
            .compute_hash()
        );

        assert_ne!(
            hash_expected,
            CardanoTransactionsSnapshot::new(
                "mk-root-456".to_string(),
                ChainPoint::new(100, 50, "block_hash-50")
            )
            .compute_hash()
        );
    }
}
