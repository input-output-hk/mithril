use mithril_common::entities::{MithrilStakeDistribution, SignedEntity};
use mithril_common::messages::{MessageAdapter, MithrilStakeDistributionMessage};

/// Adapter to convert [MithrilStakeDistribution] to [MithrilStakeDistributionMessage] instances
pub struct ToMithrilStakeDistributionMessageAdapter;

impl MessageAdapter<SignedEntity<MithrilStakeDistribution>, MithrilStakeDistributionMessage>
    for ToMithrilStakeDistributionMessageAdapter
{
    /// Method to trigger the conversion
    fn adapt(from: SignedEntity<MithrilStakeDistribution>) -> MithrilStakeDistributionMessage {
        MithrilStakeDistributionMessage {
            epoch: from.artifact.epoch,
            signers_with_stake: from.artifact.signers_with_stake,
            hash: from.artifact.hash,
            certificate_hash: from.certificate_id,
        }
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{
        entities::{Epoch, SignedEntityType},
        test_utils::fake_data,
    };

    use super::*;

    #[test]
    fn adapt_ok() {
        let mithril_stake_distribution = MithrilStakeDistribution {
            epoch: Epoch(1),
            signers_with_stake: fake_data::signers_with_stakes(2),
            hash: "hash-123".to_string(),
            certificate_hash: "cert-hash-123".to_string(),
        };
        let signed_entity = SignedEntity {
            signed_entity_id: "id-1234".to_string(),
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(Epoch(0)),
            certificate_id: "hash-123".to_string(),
            artifact: mithril_stake_distribution,
            created_at: "date-1234".to_string(),
        };
        let mithril_stake_distribution_message_expected = MithrilStakeDistributionMessage {
            epoch: Epoch(1),
            signers_with_stake: fake_data::signers_with_stakes(2),
            hash: "hash-123".to_string(),
            certificate_hash: "cert-hash-123".to_string(),
        };
        let mithril_stake_distribution_message =
            ToMithrilStakeDistributionMessageAdapter::adapt(signed_entity);

        assert_eq!(
            mithril_stake_distribution_message_expected,
            mithril_stake_distribution_message
        );
    }
}
