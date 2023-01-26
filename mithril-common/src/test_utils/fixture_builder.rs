use kes_summed_ed25519::{kes::Sum6Kes, traits::KesSk};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

use crate::{
    crypto_helper::{
        tests_setup, tests_setup::setup_temp_directory_for_signer, ColdKeyGenerator, OpCert,
        ProtocolStakeDistribution, SerDeShelleyFileFormat,
    },
    entities::{PartyId, ProtocolParameters, StakeDistribution},
    test_utils::{fake_data, mithril_fixture::MithrilFixture},
};

/// A builder of mithril types.
pub struct MithrilFixtureBuilder {
    protocol_parameters: ProtocolParameters,
    enable_signers_certification: bool,
    number_of_signers: usize,
    stake_distribution_generation_method: StakeDistributionGenerationMethod,
}

impl Default for MithrilFixtureBuilder {
    fn default() -> Self {
        Self {
            protocol_parameters: fake_data::protocol_parameters(),
            enable_signers_certification: true,
            number_of_signers: 5,
            stake_distribution_generation_method:
                StakeDistributionGenerationMethod::RandomDistribution { seed: [0u8; 32] },
        }
    }
}

/// Methods that can be used to generate the stake distribution.
pub enum StakeDistributionGenerationMethod {
    /// Each party will have a random stake.
    RandomDistribution {
        /// The randomizer seed
        seed: [u8; 32],
    },

    /// Use a custom stake distribution
    Custom(StakeDistribution),
}

impl MithrilFixtureBuilder {
    /// Set the protocol_parameters.
    pub fn with_protocol_parameters(mut self, protocol_parameters: ProtocolParameters) -> Self {
        self.protocol_parameters = protocol_parameters;
        self
    }

    /// Set the number of signers that will be generated.
    pub fn with_signers(mut self, number_of_signers: usize) -> Self {
        self.number_of_signers = number_of_signers;
        self
    }

    /// If set the generated signers won't be certified (meaning that they won't
    /// have a operational certificate).
    pub fn disable_signers_certification(mut self) -> Self {
        self.enable_signers_certification = false;
        self
    }

    /// Set the generation method used to compute the stake distribution.
    pub fn with_stake_distribution(
        mut self,
        stake_distribution_generation_method: StakeDistributionGenerationMethod,
    ) -> Self {
        self.stake_distribution_generation_method = stake_distribution_generation_method;
        self
    }

    /// Transform the specified parameters to a [MithrilFixture].
    pub fn build(self) -> MithrilFixture {
        let protocol_stake_distribution = self.generate_stake_distribution();
        let signers = tests_setup::setup_signers_from_stake_distribution(
            &protocol_stake_distribution,
            &self.protocol_parameters.clone().into(),
        );

        MithrilFixture::new(
            self.protocol_parameters,
            signers,
            protocol_stake_distribution,
        )
    }

    fn generate_stake_distribution(&self) -> ProtocolStakeDistribution {
        let mut kes_keys_seed = [0u8; 32];
        let signers_party_ids = (0..self.number_of_signers).into_iter().map(|party_index| {
            if self.enable_signers_certification {
                build_party_with_operational_certificate(party_index, &mut kes_keys_seed)
            } else {
                format!("{party_index:<032}")
            }
        });

        match &self.stake_distribution_generation_method {
            StakeDistributionGenerationMethod::RandomDistribution { seed } => {
                let mut stake_rng = ChaCha20Rng::from_seed(*seed);

                signers_party_ids
                    .map(|party_id| {
                        let stake = 1 + stake_rng.next_u64() % 999;
                        (party_id, stake)
                    })
                    .collect::<Vec<_>>()
            }
            StakeDistributionGenerationMethod::Custom(stake_distribution) => stake_distribution
                .clone()
                .into_iter()
                .collect::<ProtocolStakeDistribution>(),
        }
    }
}

fn build_party_with_operational_certificate(
    party_index: usize,
    kes_key_seed: &mut [u8],
) -> PartyId {
    let keypair = ColdKeyGenerator::create_deterministic_keypair([party_index as u8; 32]);
    let (kes_secret_key, kes_verification_key) = Sum6Kes::keygen(kes_key_seed);
    let operational_certificate = OpCert::new(kes_verification_key, 0, 0, keypair);
    let party_id = operational_certificate
        .compute_protocol_party_id()
        .expect("compute protocol party id should not fail");
    let temp_dir = setup_temp_directory_for_signer(&party_id, true)
        .expect("setup temp directory should return a value");
    if !temp_dir.join("kes.sk").exists() {
        kes_secret_key
            .to_file(temp_dir.join("kes.sk"))
            .expect("KES secret key file export should not fail");
    }
    if !temp_dir.join("opcert.cert").exists() {
        operational_certificate
            .to_file(temp_dir.join("opcert.cert"))
            .expect("operational certificate file export should not fail");
    }
    party_id
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeSet;

    #[test]
    fn with_protocol_params() {
        let protocol_parameters = ProtocolParameters::new(1, 10, 0.56);
        let result = MithrilFixtureBuilder::default()
            .with_protocol_parameters(protocol_parameters.clone())
            .build();

        assert_eq!(protocol_parameters, result.protocol_parameters());
    }

    #[test]
    fn with_signers() {
        let result = MithrilFixtureBuilder::default().with_signers(4).build();

        assert_eq!(4, result.signers_with_stake().len());
    }

    #[test]
    fn random_stake_distribution_generates_as_many_signers_as_parties() {
        let result = MithrilFixtureBuilder::default()
            .with_stake_distribution(StakeDistributionGenerationMethod::RandomDistribution {
                seed: [0u8; 32],
            })
            .with_signers(4)
            .build();

        assert_eq!(4, result.stake_distribution().len());
    }

    #[test]
    fn each_parties_generated_with_random_stake_distribution_have_different_stakes() {
        let result = MithrilFixtureBuilder::default()
            .with_stake_distribution(StakeDistributionGenerationMethod::RandomDistribution {
                seed: [0u8; 32],
            })
            .with_signers(5)
            .build();
        let stakes = result.stake_distribution();

        // BtreeSet dedup values
        assert_eq!(stakes.len(), BTreeSet::from_iter(stakes.values()).len());
    }
}
