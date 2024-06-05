use mithril_common::entities::{CardanoTransactionsSnapshot, SignedEntity};
use mithril_common::messages::{
    CardanoTransactionSnapshotListItemMessage, CardanoTransactionSnapshotListMessage,
    ToMessageAdapter,
};

/// Adapter to convert a list of [CardanoTransaction] to [CardanoTransactionSnapshotListMessage] instances
#[allow(dead_code)]
pub struct ToCardanoTransactionListMessageAdapter;

impl
    ToMessageAdapter<
        Vec<SignedEntity<CardanoTransactionsSnapshot>>,
        CardanoTransactionSnapshotListMessage,
    > for ToCardanoTransactionListMessageAdapter
{
    /// Method to trigger the conversion
    fn adapt(
        snapshots: Vec<SignedEntity<CardanoTransactionsSnapshot>>,
    ) -> CardanoTransactionSnapshotListMessage {
        snapshots
            .into_iter()
            .map(|entity| CardanoTransactionSnapshotListItemMessage {
                merkle_root: entity.artifact.merkle_root,
                epoch: entity.signed_entity_type.get_epoch(),
                block_number: entity.artifact.block_number,
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
        let signed_entity = SignedEntity::<CardanoTransactionsSnapshot>::dummy();
        let mithril_stake_distribution_list_message_expected =
            vec![CardanoTransactionSnapshotListItemMessage {
                merkle_root: signed_entity.artifact.merkle_root.clone(),
                epoch: signed_entity.signed_entity_type.get_epoch(),
                block_number: signed_entity.artifact.block_number,
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
