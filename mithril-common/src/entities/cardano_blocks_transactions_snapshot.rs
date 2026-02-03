use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use super::BlockNumber;

/// Snapshot of a set of Cardano blocks and transactions
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CardanoBlocksTransactionsSnapshot {
    /// Hash of the Cardano blocks and transactions set
    pub hash: String,

    /// Merkle root of the Cardano blocks and transactions set
    pub merkle_root: String,

    /// Block number at which the Cardano blocks and transactions set has been snapshotted
    pub block_number_signed: BlockNumber,

    /// Block number of the tip of the chain at snapshot time (approximate)
    pub block_number_tip: BlockNumber,
}

impl CardanoBlocksTransactionsSnapshot {
    /// Creates a new [CardanoBlocksTransactionsSnapshot]
    pub fn new(
        merkle_root: String,
        block_number_signed: BlockNumber,
        offset_security_parameter: BlockNumber,
    ) -> Self {
        let mut snapshot = Self {
            merkle_root,
            block_number_signed,
            block_number_tip: block_number_signed + offset_security_parameter,
            hash: "".to_string(),
        };
        snapshot.hash = snapshot.compute_hash();
        snapshot
    }

    /// Cardano blocks transactions snapshot hash computation
    fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.merkle_root.as_bytes());
        hasher.update(self.block_number_signed.to_be_bytes());
        hasher.update(self.block_number_tip.to_be_bytes());

        hex::encode(hasher.finalize())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cardano_blocks_transactions_snapshot_compute_hash() {
        let expected_mk_root_hash =
            "d99fe4ea053d416de6726deef785766e44970351e63172521f4783f23c8cb66c";

        assert_eq!(
            expected_mk_root_hash,
            CardanoBlocksTransactionsSnapshot::new(
                "mk-root-123".to_string(),
                BlockNumber(50),
                BlockNumber(15)
            )
            .compute_hash()
        );

        assert_ne!(
            expected_mk_root_hash,
            CardanoBlocksTransactionsSnapshot::new(
                "mk-root-456".to_string(),
                BlockNumber(50),
                BlockNumber(15)
            )
            .compute_hash()
        );

        assert_ne!(
            expected_mk_root_hash,
            CardanoBlocksTransactionsSnapshot::new(
                "mk-root-123".to_string(),
                BlockNumber(47),
                BlockNumber(15)
            )
            .compute_hash()
        );

        assert_ne!(
            expected_mk_root_hash,
            CardanoBlocksTransactionsSnapshot::new(
                "mk-root-123".to_string(),
                BlockNumber(50),
                BlockNumber(42)
            )
            .compute_hash()
        );
    }

    #[test]
    fn test_block_number_tip_is_the_addition_of_block_number_and_offset_security_parameter() {
        let block_number_signed_value = 100;
        let offset_security_parameter_value = 20;

        let snapshot = CardanoBlocksTransactionsSnapshot::new(
            "mk-root-123".to_string(),
            BlockNumber(block_number_signed_value),
            BlockNumber(offset_security_parameter_value),
        );

        assert_eq!(
            snapshot.block_number_tip,
            BlockNumber(block_number_signed_value + offset_security_parameter_value),
            "Block number tip should be the sum of block number signed and offset security parameter"
        );
    }
}
