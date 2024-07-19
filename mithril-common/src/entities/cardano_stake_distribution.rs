use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::signable_builder::Artifact;

use super::{Epoch, StakeDistribution};

/// Cardano Stake Distribution
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct CardanoStakeDistribution {
    /// Unique hash of the Cardano Stake Distribution
    pub hash: String,

    /// Epoch at which the Cardano Stake Distribution is created
    pub epoch: Epoch,

    /// List of pools with their stakes of the Cardano Stake Distribution
    pub stake_distribution: StakeDistribution,
}

impl CardanoStakeDistribution {
    /// Constructor
    pub fn new(epoch: Epoch, stake_distribution: StakeDistribution) -> CardanoStakeDistribution {
        let mut cardano_stake_distribution = CardanoStakeDistribution {
            hash: "".to_string(),
            epoch,
            stake_distribution,
        };
        cardano_stake_distribution.hash = cardano_stake_distribution.compute_hash();

        cardano_stake_distribution
    }

    /// Cardano stake distribution hash computation
    fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.epoch.to_be_bytes());
        self.stake_distribution.iter().for_each(|(k, v)| {
            hasher.update(k.as_bytes());
            hasher.update(v.to_be_bytes());
        });

        hex::encode(hasher.finalize())
    }
}

#[typetag::serde]
impl Artifact for CardanoStakeDistribution {
    fn get_id(&self) -> String {
        self.hash.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compute_hash() {
        assert_eq!(
            "7144dda132aa4fe758df8da5f85a774cef602ce85c8554baeeea006866710154",
            CardanoStakeDistribution::new(
                Epoch(1),
                StakeDistribution::from([("pool-1".to_string(), 100), ("pool-2".to_string(), 200)])
            )
            .compute_hash()
        );
    }

    #[test]
    fn compute_hash_returns_same_hash_with_same_cardano_stake_distribution() {
        let epoch = Epoch(1);

        assert_eq!(
            CardanoStakeDistribution::new(
                epoch,
                StakeDistribution::from([("pool-1".to_string(), 100), ("pool-2".to_string(), 200)])
            )
            .compute_hash(),
            CardanoStakeDistribution::new(
                epoch,
                StakeDistribution::from([("pool-1".to_string(), 100), ("pool-2".to_string(), 200)])
            )
            .compute_hash()
        );
    }

    #[test]
    fn compute_hash_returns_same_hash_whatever_the_stake_distribution_order() {
        let epoch = Epoch(1);

        assert_eq!(
            CardanoStakeDistribution::new(
                epoch,
                StakeDistribution::from([("pool-1".to_string(), 100), ("pool-2".to_string(), 200)])
            )
            .compute_hash(),
            CardanoStakeDistribution::new(
                epoch,
                StakeDistribution::from([("pool-2".to_string(), 200), ("pool-1".to_string(), 100)])
            )
            .compute_hash()
        );
    }

    #[test]
    fn compute_hash_returns_different_hash_with_different_epoch() {
        assert_ne!(
            CardanoStakeDistribution::new(
                Epoch(1),
                StakeDistribution::from([("pool-1".to_string(), 100)])
            )
            .compute_hash(),
            CardanoStakeDistribution::new(
                Epoch(2),
                StakeDistribution::from([("pool-1".to_string(), 100)])
            )
            .compute_hash()
        );
    }

    #[test]
    fn compute_hash_returns_different_hash_with_different_stake_distribution_pool_id() {
        let epoch = Epoch(1);

        assert_ne!(
            CardanoStakeDistribution::new(
                epoch,
                StakeDistribution::from([("pool-1".to_string(), 100)])
            )
            .compute_hash(),
            CardanoStakeDistribution::new(
                epoch,
                StakeDistribution::from([("pool-2".to_string(), 100)])
            )
            .compute_hash()
        );
    }

    #[test]
    fn compute_hash_returns_different_hash_with_different_stake_distribution_stakes() {
        let epoch = Epoch(1);

        assert_ne!(
            CardanoStakeDistribution::new(
                epoch,
                StakeDistribution::from([("pool-1".to_string(), 100)])
            )
            .compute_hash(),
            CardanoStakeDistribution::new(
                epoch,
                StakeDistribution::from([("pool-1".to_string(), 150)])
            )
            .compute_hash()
        );
    }

    #[test]
    fn compute_hash_returns_different_hash_with_different_stake_distribution_size() {
        let epoch = Epoch(1);

        assert_ne!(
            CardanoStakeDistribution::new(
                epoch,
                StakeDistribution::from([("pool-1".to_string(), 100)])
            )
            .compute_hash(),
            CardanoStakeDistribution::new(
                epoch,
                StakeDistribution::from([("pool-1".to_string(), 100), ("pool-2".to_string(), 150)])
            )
            .compute_hash()
        );
    }
}
