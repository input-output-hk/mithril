use mithril_common::entities::{MithrilStakeDistribution, SignedEntity};
use mithril_common::messages::{
    MithrilStakeDistributionMessage, SignerWithStakeMessagePart, ToMessageAdapter,
};

/// Adapter to convert [MithrilStakeDistribution] to [MithrilStakeDistributionMessage] instances
#[allow(dead_code)]
pub struct ToMithrilStakeDistributionMessageAdapter;

impl ToMessageAdapter<SignedEntity<MithrilStakeDistribution>, MithrilStakeDistributionMessage>
    for ToMithrilStakeDistributionMessageAdapter
{
    /// Method to trigger the conversion
    fn adapt(from: SignedEntity<MithrilStakeDistribution>) -> MithrilStakeDistributionMessage {
        MithrilStakeDistributionMessage {
            epoch: from.artifact.epoch,
            signers_with_stake: SignerWithStakeMessagePart::from_signers(
                from.artifact.signers_with_stake,
            ),
            hash: from.artifact.hash,
            certificate_hash: from.certificate_id,
            created_at: from.created_at,
            protocol_parameters: from.artifact.protocol_parameters,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn adapt_ok() {
        let signed_entity = SignedEntity::<MithrilStakeDistribution>::dummy();
        let mithril_stake_distribution_message_expected = MithrilStakeDistributionMessage {
            epoch: signed_entity.artifact.epoch,
            signers_with_stake: SignerWithStakeMessagePart::from_signers(
                signed_entity.artifact.signers_with_stake.clone(),
            ),
            hash: signed_entity.artifact.hash.clone(),
            certificate_hash: signed_entity.certificate_id.clone(),
            created_at: signed_entity.created_at,
            protocol_parameters: signed_entity.artifact.protocol_parameters.clone(),
        };

        let mithril_stake_distribution_message =
            ToMithrilStakeDistributionMessageAdapter::adapt(signed_entity);

        assert_eq!(
            mithril_stake_distribution_message_expected,
            mithril_stake_distribution_message
        );
    }
}
