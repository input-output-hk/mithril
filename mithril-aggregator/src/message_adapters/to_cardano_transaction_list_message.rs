use mithril_common::entities::{CardanoTransactionsCommitment, SignedEntity};
use mithril_common::messages::{
    CardanoTransactionCommitmentListItemMessage, CardanoTransactionCommitmentListMessage,
    ToMessageAdapter,
};

/// Adapter to convert a list of [CardanoTransaction] to [CardanoTransactionCommitmentListMessage] instances
pub struct ToCardanoTransactionListMessageAdapter;

impl
    ToMessageAdapter<
        Vec<SignedEntity<CardanoTransactionsCommitment>>,
        CardanoTransactionCommitmentListMessage,
    > for ToCardanoTransactionListMessageAdapter
{
    /// Method to trigger the conversion
    fn adapt(
        snapshots: Vec<SignedEntity<CardanoTransactionsCommitment>>,
    ) -> CardanoTransactionCommitmentListMessage {
        snapshots
            .into_iter()
            .map(|entity| CardanoTransactionCommitmentListItemMessage {
                merkle_root: entity.artifact.merkle_root.clone(),
                beacon: entity.artifact.beacon.clone(),
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
        let signed_entity = SignedEntity::<CardanoTransactionsCommitment>::dummy();
        let mithril_stake_distribution_list_message_expected =
            vec![CardanoTransactionCommitmentListItemMessage {
                merkle_root: signed_entity.artifact.merkle_root.clone(),
                beacon: signed_entity.artifact.beacon.clone(),
                hash: signed_entity.artifact.hash.clone(),
                certificate_hash: signed_entity.certificate_id.clone(),
                created_at: signed_entity.created_at,
            }];

        let mithril_stake_distribution_list_message =
            ToCardanoTransactionListMessageAdapter::adapt(vec![signed_entity]);

        assert_eq!(
            mithril_stake_distribution_list_message_expected,
            mithril_stake_distribution_list_message
        );
    }
}
