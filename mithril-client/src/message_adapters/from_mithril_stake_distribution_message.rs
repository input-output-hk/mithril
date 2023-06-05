use mithril_common::{
    entities::MithrilStakeDistribution,
    messages::{MessageAdapter, MithrilStakeDistributionMessage},
};

pub struct FromMithrilStakeDistributionMessageAdapter;

impl MessageAdapter<MithrilStakeDistributionMessage, MithrilStakeDistribution>
    for FromMithrilStakeDistributionMessageAdapter
{
    fn adapt(from: MithrilStakeDistributionMessage) -> MithrilStakeDistribution {
        MithrilStakeDistribution {
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
    fn test_adapt() {
        let message = MithrilStakeDistributionMessage {
            epoch: Epoch(1),
            signers_with_stake: fake_data::signers_with_stakes(2),
            hash: "hash-123".to_string(),
            certificate_hash: "certificate-hash-123".to_string(),
        };

        let stake_distribution = FromMithrilStakeDistributionMessageAdapter::adapt(message);

        assert_eq!(2, stake_distribution.signers_with_stake.len());
        assert_eq!("hash-123".to_string(), stake_distribution.hash);
    }
}
