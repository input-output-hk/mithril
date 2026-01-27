use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use super::BlockNumber;

/// Snapshot of a set of Cardano transactions
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CardanoBlocksTransactionsSnapshot {
    /// Hash of the Cardano transactions set
    pub hash: String,

    /// Merkle root of the Cardano transactions set
    pub merkle_root: String,

    /// Beacon of the Cardano transactions set
    pub block_number_signed: BlockNumber,

    /// Block number of the tip at snapshot time
    pub block_number_tip: BlockNumber,
}

impl CardanoBlocksTransactionsSnapshot {
    /// Creates a new [CardanoTransactionsSnapshot]
    pub fn new(merkle_root: String, block_number_signed: BlockNumber) -> Self {
        let mut snapshot = Self {
            merkle_root,
            block_number_signed,
            block_number_tip: BlockNumber(0),
            hash: "".to_string(),
        };
        snapshot.block_number_tip = snapshot.compute_block_number_tip();
        snapshot.hash = snapshot.compute_hash();

        snapshot
    }

    /// Cardano transactions snapshot hash computation
    fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.merkle_root.clone().as_bytes());
        hasher.update(self.block_number_signed.to_be_bytes());
        //TODO also hasher update the block number tip ?

        hex::encode(hasher.finalize())
    }

    fn compute_block_number_tip(&self) -> BlockNumber {
        //TODO sends offset_security_parameter & delta (or timepoint) throught the construction of CardanoBlocksTransactionsSnapshot ?
        self.block_number_signed + offset_security_parameter + delta // delta = current_tiempoint - block_number_signed ?
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
            CardanoBlocksTransactionsSnapshot::new("mk-root-123".to_string(), BlockNumber(50))
                .compute_hash()
        );

        assert_ne!(
            hash_expected,
            CardanoBlocksTransactionsSnapshot::new("mk-root-456".to_string(), BlockNumber(50))
                .compute_hash()
        );

        assert_ne!(
            hash_expected,
            CardanoBlocksTransactionsSnapshot::new("mk-root-123".to_string(), BlockNumber(47))
                .compute_hash()
        );
    }
}
