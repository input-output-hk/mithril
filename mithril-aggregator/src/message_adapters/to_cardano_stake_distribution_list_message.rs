use mithril_common::entities::{CardanoStakeDistribution, SignedEntity};
use mithril_common::messages::{
    CardanoStakeDistributionListItemMessage, CardanoStakeDistributionListMessage, ToMessageAdapter,
};

/// Adapter to convert a list of [CardanoStakeDistribution] to [CardanoStakeDistributionListMessage] instances
#[allow(dead_code)]
pub struct ToCardanoStakeDistributionListMessageAdapter;

impl
    ToMessageAdapter<
        Vec<SignedEntity<CardanoStakeDistribution>>,
        CardanoStakeDistributionListMessage,
    > for ToCardanoStakeDistributionListMessageAdapter
{
    /// Method to trigger the conversion
    fn adapt(
        snapshots: Vec<SignedEntity<CardanoStakeDistribution>>,
    ) -> CardanoStakeDistributionListMessage {
        snapshots
            .into_iter()
            .map(|entity| CardanoStakeDistributionListItemMessage {
                // The epoch stored in the signed entity type beacon corresponds to epoch
                // at the end of which the Cardano stake distribution is computed by the Cardano node.
                epoch: entity.signed_entity_type.get_epoch(),
                hash: entity.artifact.hash,
                certificate_hash: entity.certificate_id,
                created_at: entity.created_at,
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn adapt_ok() {
        let signed_entity = SignedEntity::<CardanoStakeDistribution>::dummy();
        let cardano_stake_distribution_list_message_expected =
            vec![CardanoStakeDistributionListItemMessage {
                epoch: signed_entity.artifact.epoch,
                hash: signed_entity.artifact.hash.clone(),
                certificate_hash: signed_entity.certificate_id.clone(),
                created_at: signed_entity.created_at,
            }];

        let cardano_stake_distribution_list_message =
            ToCardanoStakeDistributionListMessageAdapter::adapt(vec![signed_entity]);

        assert_eq!(
            cardano_stake_distribution_list_message_expected,
            cardano_stake_distribution_list_message
        );
    }
}
