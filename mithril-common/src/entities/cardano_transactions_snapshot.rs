use crate::signable_builder::Artifact;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use super::CardanoDbBeacon;

/// Snapshot of a set of Cardano transactions
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct CardanoTransactionsSnapshot {
    /// Hash of the Cardano transactions set
    pub hash: String,

    /// Merkle root of the Cardano transactions set
    pub merkle_root: String,

    /// Beacon of the Cardano transactions set
    pub beacon: CardanoDbBeacon,
}

impl CardanoTransactionsSnapshot {
    /// Creates a new [CardanoTransactionsSnapshot]
    pub fn new(merkle_root: String, beacon: CardanoDbBeacon) -> Self {
        let mut cardano_transactions_snapshot = Self {
            merkle_root,
            beacon,
            hash: "".to_string(),
        };
        cardano_transactions_snapshot.hash = cardano_transactions_snapshot.compute_hash();
        cardano_transactions_snapshot
    }

    /// Cardano transactions snapshot hash computation
    fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.merkle_root.clone().as_bytes());
        hasher.update(self.beacon.compute_hash().as_bytes());

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
        let hash_expected = "66a1d7aa3995e9a0dce15fae3f6b91640824ecd1f81991df5ce4ddff62b34df4";

        assert_eq!(
            hash_expected,
            CardanoTransactionsSnapshot::new("mk-root-123".to_string(), CardanoDbBeacon::default())
                .compute_hash()
        );

        assert_ne!(
            hash_expected,
            CardanoTransactionsSnapshot::new("mk-root-456".to_string(), CardanoDbBeacon::default())
                .compute_hash()
        );
    }
}
