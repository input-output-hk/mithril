use crate::{
    crypto_helper::{ProtocolInitializer, ProtocolSigner, ProtocolStakeDistribution},
    entities::{ProtocolParameters, Signer, SignerWithStake, StakeDistribution},
};
use std::collections::HashMap;

/// A fixture of Mithril data types.
#[derive(Debug, Clone)]
pub struct MithrilFixture {
    protocol_parameters: ProtocolParameters,
    signers: Vec<SignerFixture>,
    stake_distribution: ProtocolStakeDistribution,
}

/// A signer fixture, containing a [signer entity][SignerWithStake] with its
/// corresponding protocol [signer][ProtocolSigner] and
/// [initializer][ProtocolInitializer]
#[derive(Debug, Clone)]
pub struct SignerFixture {
    /// A [SignerWithStake].
    pub signer_with_stake: SignerWithStake,
    /// A [ProtocolSigner].
    pub protocol_signer: ProtocolSigner,
    /// A [ProtocolSigner].
    pub protocol_initializer: ProtocolInitializer,
}

impl MithrilFixture {
    /// [MithrilFixture] factory.
    #[cfg(any(test, feature = "test_only"))]
    pub fn new(
        protocol_parameters: ProtocolParameters,
        signers: Vec<SignerFixture>,
        stake_distribution: ProtocolStakeDistribution,
    ) -> Self {
        Self {
            protocol_parameters,
            signers,
            stake_distribution,
        }
    }

    /// Get the fixture protocol parameters.
    pub fn protocol_parameters(&self) -> ProtocolParameters {
        self.protocol_parameters.clone()
    }

    /// Get the fixture signers.
    pub fn signers_fixture(&self) -> Vec<SignerFixture> {
        self.signers.clone()
    }

    /// Get the fixture signers.
    pub fn signers(&self) -> Vec<Signer> {
        self.signers
            .clone()
            .into_iter()
            .map(|s| s.signer_with_stake.into())
            .collect()
    }

    /// Get the fixture signers with stake.
    pub fn signers_with_stake(&self) -> Vec<SignerWithStake> {
        self.signers
            .iter()
            .map(|s| &s.signer_with_stake)
            .cloned()
            .collect()
    }

    /// Get the fixture stake distribution.
    pub fn stake_distribution(&self) -> StakeDistribution {
        HashMap::from_iter(self.stake_distribution.clone())
    }

    /// Get the fixture protocol stake distribution.
    pub fn protocol_stake_distribution(&self) -> ProtocolStakeDistribution {
        self.stake_distribution.clone()
    }
}
