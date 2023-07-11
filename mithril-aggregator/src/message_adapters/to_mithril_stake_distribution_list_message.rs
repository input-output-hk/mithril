use mithril_common::entities::{MithrilStakeDistribution, SignedEntity};
use mithril_common::messages::{
    MithrilStakeDistributionListItemMessage, MithrilStakeDistributionListMessage, ToMessageAdapter,
};

/// Adapter to convert a list of [MithrilStakeDistribution] to [MithrilStakeDistributionListMessage] instances
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
    use chrono::{DateTime, Utc};
    use mithril_common::{
        entities::{Epoch, SignedEntityType},
        test_utils::fake_data,
    };

    use super::*;

    #[test]
    fn adapt_ok() {
        let mithril_stake_distribution = MithrilStakeDistribution {
            epoch: Epoch(1),
            signers_with_stake: fake_data::signers_with_stakes(1),
            hash: "hash-123".to_string(),
            protocol_parameters: fake_data::protocol_parameters(),
        };
        let signed_entity = SignedEntity {
            signed_entity_id: "signed-entity-id-123".to_string(),
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(Epoch(0)),
            certificate_id: "certificate-hash-123".to_string(),
            artifact: mithril_stake_distribution,
            created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
        };
        let mithril_stake_distribution_list_message =
            ToMithrilStakeDistributionListMessageAdapter::adapt(vec![signed_entity]);
        let mithril_stake_distribution_list_message_expected =
            vec![MithrilStakeDistributionListItemMessage {
                epoch: Epoch(1),
                hash: "hash-123".to_string(),
                certificate_hash: "certificate-hash-123".to_string(),
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            }];

        assert_eq!(
            mithril_stake_distribution_list_message_expected,
            mithril_stake_distribution_list_message
        );
    }
}
