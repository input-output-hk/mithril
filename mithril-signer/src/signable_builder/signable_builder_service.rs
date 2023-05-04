use mithril_common::{
    entities::{ProtocolMessage, SignedEntityType},
    signable_builder::{
        CardanoImmutableFilesFullSignableBuilder, MithrilStakeDistributionSignableBuilder,
        SignableBuilder,
    },
    StdResult,
};

/// SignableBuilder Service
pub struct SignableBuilderService {
    immutable_signable_builder: CardanoImmutableFilesFullSignableBuilder,
    mithril_stake_distribution_builder: MithrilStakeDistributionSignableBuilder,
}

impl SignableBuilderService {
    /// SignableBuilderService factory
    pub fn new(
        immutable_signable_builder: CardanoImmutableFilesFullSignableBuilder,
        mithril_stake_distribution_builder: MithrilStakeDistributionSignableBuilder,
    ) -> Self {
        Self {
            immutable_signable_builder,
            mithril_stake_distribution_builder,
        }
    }
}

impl SignableBuilderService {
    #[allow(dead_code)]
    async fn compute_signable(
        &self,
        signed_entity_type: SignedEntityType,
    ) -> StdResult<ProtocolMessage> {
        let protocol_message = match signed_entity_type {
            SignedEntityType::CardanoImmutableFilesFull(beacon) => {
                self.immutable_signable_builder
                    .compute_protocol_message(beacon)
                    .await?
            }
            SignedEntityType::MithrilStakeDistribution(epoch) => {
                self.mithril_stake_distribution_builder
                    .compute_protocol_message(epoch)
                    .await?
            }
            SignedEntityType::CardanoStakeDistribution(_) => todo!(),
        };

        Ok(protocol_message)
    }
}

#[cfg(test)]
mod tests {}
