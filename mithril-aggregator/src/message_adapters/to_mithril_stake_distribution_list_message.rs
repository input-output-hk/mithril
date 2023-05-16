use mithril_common::entities::{MithrilStakeDistribution, SignedEntity};
use mithril_common::messages::{
    MessageAdapter, MithrilStakeDistributionListItemMessage, MithrilStakeDistributionListMessage,
};

/// Adapter to convert a list of [MithrilStakeDistribution] to [MithrilStakeDistributionListMessage] instances
pub struct ToMithrilStakeDistributionListMessageAdapter;

impl
    MessageAdapter<Vec<SignedEntity<MithrilStakeDistribution>>, MithrilStakeDistributionListMessage>
    for ToMithrilStakeDistributionListMessageAdapter
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
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::SignedEntityType;
    use mithril_common::{entities::Epoch, test_utils::fake_data::signers_with_stakes};

    use super::*;

    #[test]
    fn adapt_ok() {
        let mithril_stake_distribution = MithrilStakeDistribution {
            epoch: Epoch(1),
            signers_with_stake: signers_with_stakes(1),
            hash: "hash-123".to_string(),
            certificate_hash: "certificate-hash-123".to_string(),
        };
        let signed_entity = SignedEntity {
            signed_entity_id: "signed-entity-id-123".to_string(),
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(Epoch(0)),
            certificate_id: "certificate-hash-123".to_string(),
            artifact: mithril_stake_distribution,
            created_at: "date-123".to_string(),
        };
        let mithril_stake_distribution_list_message =
            ToMithrilStakeDistributionListMessageAdapter::adapt(vec![signed_entity]);
        let mithril_stake_distribution_list_message_expected =
            vec![MithrilStakeDistributionListItemMessage {
                epoch: Epoch(1),
                hash: "hash-123".to_string(),
                certificate_hash: "certificate-hash-123".to_string(),
            }];

        assert_eq!(
            mithril_stake_distribution_list_message_expected,
            mithril_stake_distribution_list_message
        );
    }
}
