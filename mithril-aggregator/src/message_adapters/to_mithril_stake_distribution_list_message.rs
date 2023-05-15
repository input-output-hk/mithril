use mithril_common::entities::MithrilStakeDistribution;
use mithril_common::messages::{
    MessageAdapter, MithrilStakeDistributionListItemMessage, MithrilStakeDistributionListMessage,
};

/// Adapter to convert a list of [MithrilStakeDistribution] to [MithrilStakeDistributionListMessage] instances
pub struct ToMithrilStakeDistributionListMessageAdapter;

impl MessageAdapter<Vec<MithrilStakeDistribution>, MithrilStakeDistributionListMessage>
    for ToMithrilStakeDistributionListMessageAdapter
{
    /// Method to trigger the conversion
    fn adapt(snapshots: Vec<MithrilStakeDistribution>) -> MithrilStakeDistributionListMessage {
        snapshots
            .into_iter()
            .map(
                |stake_distribution| MithrilStakeDistributionListItemMessage {
                    epoch: stake_distribution.epoch,
                    hash: stake_distribution.hash,
                    certificate_hash: stake_distribution.certificate_hash,
                },
            )
            .collect()
    }
}

#[cfg(test)]
mod tests {
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
        let mithril_stake_distribution_list_message =
            ToMithrilStakeDistributionListMessageAdapter::adapt(vec![mithril_stake_distribution]);
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
