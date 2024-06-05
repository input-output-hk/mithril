use mithril_common::entities::{CardanoTransactionsSnapshot, SignedEntity};
use mithril_common::messages::{CardanoTransactionSnapshotMessage, ToMessageAdapter};

/// Adapter to convert [CardanoTransaction] to [CardanoTransactionSnapshotMessage] instances
#[allow(dead_code)]
pub struct ToCardanoTransactionMessageAdapter;

impl ToMessageAdapter<SignedEntity<CardanoTransactionsSnapshot>, CardanoTransactionSnapshotMessage>
    for ToCardanoTransactionMessageAdapter
{
    /// Method to trigger the conversion
    fn adapt(from: SignedEntity<CardanoTransactionsSnapshot>) -> CardanoTransactionSnapshotMessage {
        CardanoTransactionSnapshotMessage {
            merkle_root: from.artifact.merkle_root,
            epoch: from.signed_entity_type.get_epoch(),
            block_number: from.artifact.block_number,
            hash: from.artifact.hash,
            certificate_hash: from.certificate_id,
            created_at: from.created_at,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn adapt_ok() {
        let signed_entity = SignedEntity::<CardanoTransactionsSnapshot>::dummy();
        let cardano_stake_distribution_message_expected = CardanoTransactionSnapshotMessage {
            merkle_root: signed_entity.artifact.merkle_root.clone(),
            epoch: signed_entity.signed_entity_type.get_epoch(),
            block_number: signed_entity.artifact.block_number,
            hash: signed_entity.artifact.hash.clone(),
            certificate_hash: signed_entity.certificate_id.clone(),
            created_at: signed_entity.created_at,
        };

        let cardano_stake_distribution_message =
            ToCardanoTransactionMessageAdapter::adapt(signed_entity);

        assert_eq!(
            cardano_stake_distribution_message_expected,
            cardano_stake_distribution_message
        );
    }
}
