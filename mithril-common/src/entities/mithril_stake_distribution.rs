use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::{
    entities::{Epoch, SignerWithStake},
    signable_builder::Artifact,
};

use super::ProtocolParameters;

/// Mithril Stake Distribution
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct MithrilStakeDistribution {
    /// Epoch at which the Mithril Stake Distribution is created
    pub epoch: Epoch,

    /// List of signers with stakes of the Mithril Stake Distribution
    pub signers_with_stake: Vec<SignerWithStake>,

    /// Hash of the Mithril Stake Distribution (different from the AVK).
    pub hash: String,

    /// Protocol parameters used to sign this stake distribution
    pub protocol_parameters: ProtocolParameters,
}

impl MithrilStakeDistribution {
    /// MithrilStakeDistribution artifact factory
    pub fn new(
        epoch: Epoch,
        signers_with_stake: Vec<SignerWithStake>,
        protocol_parameters: &ProtocolParameters,
    ) -> Self {
        let mut signers_with_stake_sorted = signers_with_stake;
        signers_with_stake_sorted.sort();
        let mut mithril_stake_distribution: MithrilStakeDistribution = Self {
            epoch,
            signers_with_stake: signers_with_stake_sorted,
            hash: "".to_string(),
            protocol_parameters: protocol_parameters.to_owned(),
        };
        mithril_stake_distribution.hash = mithril_stake_distribution.compute_hash();
        mithril_stake_distribution
    }

    /// Do not add other parameters to the compute hash.
    /// Mithril Stake Distribution is defined by the epoch and signers
    fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.epoch.to_be_bytes());

        for signer_with_stake in &self.signers_with_stake {
            hasher.update(signer_with_stake.compute_hash().as_bytes());
        }

        hex::encode(hasher.finalize())
    }
}

#[typetag::serde]
impl Artifact for MithrilStakeDistribution {
    fn get_id(&self) -> String {
        self.hash.clone()
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{fake_data, MithrilFixtureBuilder};

    use super::*;

    const EXPECTED_HASH: &str = "093b4b8044c9b0f33ed678ac34096f4d5071768c8804bf98e9f239af0bbbfeeb";

    #[test]
    fn test_compute_hash() {
        let fixtures = MithrilFixtureBuilder::default().with_signers(100).build();
        let stake_distribution = MithrilStakeDistribution::new(
            Epoch(1),
            fixtures.signers_with_stake(),
            &fake_data::protocol_parameters(),
        );

        assert_eq!(EXPECTED_HASH.to_owned(), stake_distribution.compute_hash());
    }

    #[test]
    fn test_hash_fail_for_different_stake() {
        let fixtures = MithrilFixtureBuilder::default().with_signers(100).build();
        let mut signers = fixtures.signers_with_stake();
        signers[0].stake += 1;
        let stake_distribution =
            MithrilStakeDistribution::new(Epoch(1), signers, &fake_data::protocol_parameters());

        assert_ne!(EXPECTED_HASH.to_owned(), stake_distribution.compute_hash());
    }

    #[test]
    fn test_hash_fail_for_different_epoch() {
        let fixtures = MithrilFixtureBuilder::default().with_signers(100).build();
        let stake_distribution = MithrilStakeDistribution::new(
            Epoch(2),
            fixtures.signers_with_stake(),
            &fake_data::protocol_parameters(),
        );

        assert_ne!(EXPECTED_HASH.to_owned(), stake_distribution.compute_hash());
    }

    #[test]
    fn test_independence_protocol_parameters() {
        let signers = MithrilFixtureBuilder::default()
            .with_signers(100)
            .build()
            .signers_with_stake();
        let protocol_parameters = ProtocolParameters {
            k: 100,
            ..Default::default()
        };
        let stake_distribution =
            MithrilStakeDistribution::new(Epoch(1), signers, &protocol_parameters);

        assert_eq!(EXPECTED_HASH.to_owned(), stake_distribution.compute_hash());
    }
}
