use mithril_common::entities::MithrilStakeDistribution;
use mithril_common::messages::{MessageAdapter, MithrilStakeDistributionMessage};

/// Adapter to convert [MithrilStakeDistribution] to [MithrilStakeDistributionMessage] instances
pub struct ToMithrilStakeDistributionMessageAdapter;

impl MessageAdapter<MithrilStakeDistribution, MithrilStakeDistributionMessage>
    for ToMithrilStakeDistributionMessageAdapter
{
    /// Method to trigger the conversion
    fn adapt(from: MithrilStakeDistribution) -> MithrilStakeDistributionMessage {
        MithrilStakeDistributionMessage {
            epoch: from.epoch,
            signers_with_stake: from.signers_with_stake,
            hash: from.hash,
            certificate_hash: from.certificate_hash,
        }
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{entities::Epoch, test_utils::fake_data};

    use super::*;

    #[test]
    fn adapt_ok() {
        let mithril_stake_distribution = MithrilStakeDistribution {
            epoch: Epoch(1),
            signers_with_stake: fake_data::signers_with_stakes(2),
            hash: "hash-123".to_string(),
            certificate_hash: "cert-hash-123".to_string(),
        };
        let mithril_stake_distribution_message_expected = MithrilStakeDistributionMessage {
            epoch: Epoch(1),
            signers_with_stake: fake_data::signers_with_stakes(2),
            hash: "hash-123".to_string(),
            certificate_hash: "cert-hash-123".to_string(),
        };
        let mithril_stake_distribution_message =
            ToMithrilStakeDistributionMessageAdapter::adapt(mithril_stake_distribution);

        assert_eq!(
            mithril_stake_distribution_message_expected,
            mithril_stake_distribution_message
        );
    }
}
