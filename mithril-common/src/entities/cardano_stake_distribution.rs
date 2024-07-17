use serde::{Deserialize, Serialize};

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
        let hash = "".to_string();
        CardanoStakeDistribution {
            hash,
            epoch,
            stake_distribution,
        }
    }
}

#[typetag::serde]
impl Artifact for CardanoStakeDistribution {
    fn get_id(&self) -> String {
        self.hash.clone()
    }
}
