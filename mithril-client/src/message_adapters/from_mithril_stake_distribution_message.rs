use mithril_common::{
    entities::{MithrilStakeDistribution, SignedEntity, SignedEntityType},
    messages::{MessageAdapter, MithrilStakeDistributionMessage},
};

pub struct FromMithrilStakeDistributionMessageAdapter;

impl MessageAdapter<MithrilStakeDistributionMessage, SignedEntity<MithrilStakeDistribution>>
    for FromMithrilStakeDistributionMessageAdapter
{
    fn adapt(from: MithrilStakeDistributionMessage) -> SignedEntity<MithrilStakeDistribution> {
        let mithril_stake_distribution = MithrilStakeDistribution {
            epoch: from.epoch,
            signers_with_stake: from.signers_with_stake,
            hash: from.hash,
            protocol_parameters: from.protocol_parameters,
        };

        SignedEntity {
            signed_entity_id: mithril_stake_distribution.hash.clone(),
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(
                mithril_stake_distribution.epoch,
            ),
            certificate_id: from.certificate_hash,
            artifact: mithril_stake_distribution,
            created_at: from.created_at,
        }
    }
}

#[cfg(test)]
mod tests {
    use chrono::{DateTime, Utc};
    use mithril_common::{entities::Epoch, test_utils::fake_data};

    use super::*;

    #[test]
    fn test_adapt() {
        let message = MithrilStakeDistributionMessage {
            epoch: Epoch(1),
            signers_with_stake: fake_data::signers_with_stakes(2),
            hash: "hash-123".to_string(),
            certificate_hash: "certificate-hash-123".to_string(),
            created_at: DateTime::<Utc>::default(),
            protocol_parameters: fake_data::protocol_parameters(),
        };

        let signed_entity = FromMithrilStakeDistributionMessageAdapter::adapt(message);

        assert_eq!(2, signed_entity.artifact.signers_with_stake.len());
        assert_eq!("hash-123".to_string(), signed_entity.artifact.hash);
    }
}
