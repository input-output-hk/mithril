use mithril_common::entities::{CardanoStakeDistribution, SignedEntity};
use mithril_common::messages::{CardanoStakeDistributionMessage, ToMessageAdapter};

/// Adapter to convert [CardanoStakeDistribution] to [CardanoStakeDistributionMessage] instances
#[allow(dead_code)]
pub struct ToCardanoStakeDistributionMessageAdapter;

impl ToMessageAdapter<SignedEntity<CardanoStakeDistribution>, CardanoStakeDistributionMessage>
    for ToCardanoStakeDistributionMessageAdapter
{
    /// Method to trigger the conversion
    fn adapt(from: SignedEntity<CardanoStakeDistribution>) -> CardanoStakeDistributionMessage {
        CardanoStakeDistributionMessage {
            epoch: from.artifact.epoch,
            hash: from.artifact.hash,
            certificate_hash: from.certificate_id,
            stake_distribution: from.artifact.stake_distribution,
            created_at: from.created_at,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn adapt_ok() {
        let signed_entity = SignedEntity::<CardanoStakeDistribution>::dummy();
        let cardano_stake_distribution_message_expected = CardanoStakeDistributionMessage {
            epoch: signed_entity.artifact.epoch,
            hash: signed_entity.artifact.hash.clone(),
            certificate_hash: signed_entity.certificate_id.clone(),
            stake_distribution: signed_entity.artifact.stake_distribution.clone(),
            created_at: signed_entity.created_at,
        };

        let cardano_stake_distribution_message =
            ToCardanoStakeDistributionMessageAdapter::adapt(signed_entity);

        assert_eq!(
            cardano_stake_distribution_message_expected,
            cardano_stake_distribution_message
        );
    }
}
