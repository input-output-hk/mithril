use mithril_common::entities::{MithrilStakeDistribution, SignedEntity};
use mithril_common::messages::{
    MithrilStakeDistributionListItemMessage, MithrilStakeDistributionListMessage, ToMessageAdapter,
};

/// Adapter to convert a list of [MithrilStakeDistribution] to [MithrilStakeDistributionListMessage] instances
#[allow(dead_code)]
pub struct ToMithrilStakeDistributionListMessageAdapter;

impl
    ToMessageAdapter<
        Vec<SignedEntity<MithrilStakeDistribution>>,
        MithrilStakeDistributionListMessage,
    > for ToMithrilStakeDistributionListMessageAdapter
{
    /// Method to trigger the conversion
    fn adapt(
        snapshots: Vec<SignedEntity<MithrilStakeDistribution>>,
    ) -> MithrilStakeDistributionListMessage {
        snapshots
            .into_iter()
            .map(|entity| MithrilStakeDistributionListItemMessage {
                epoch: entity.artifact.epoch,
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
        let signed_entity = SignedEntity::<MithrilStakeDistribution>::dummy();
        let mithril_stake_distribution_list_message_expected =
            vec![MithrilStakeDistributionListItemMessage {
                epoch: signed_entity.artifact.epoch,
                hash: signed_entity.artifact.hash.clone(),
                certificate_hash: signed_entity.certificate_id.clone(),
                created_at: signed_entity.created_at,
            }];

        let mithril_stake_distribution_list_message =
            ToMithrilStakeDistributionListMessageAdapter::adapt(vec![signed_entity]);

        assert_eq!(
            mithril_stake_distribution_list_message_expected,
            mithril_stake_distribution_list_message
        );
    }
}
