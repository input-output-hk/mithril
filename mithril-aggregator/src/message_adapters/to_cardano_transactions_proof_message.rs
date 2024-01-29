use mithril_common::{
    entities::{CardanoTransactionsSetProof, TransactionHash},
    messages::CardanoTransactionsProofsMessage,
};

/// Adapter to spawn [CardanoTransactionsProofsMessage] from [CardanoTransactionsProofs] instances.
pub struct ToCardanoTransactionsProofsMessageAdapter;

impl ToCardanoTransactionsProofsMessageAdapter {
    /// Turn an entity instance into message.
    pub fn adapt(
        transactions_set_proofs: Vec<CardanoTransactionsSetProof>,
        transaction_hashes_to_prove: Vec<TransactionHash>,
    ) -> CardanoTransactionsProofsMessage {
        todo!("Implement ToCardanoTransactionsProofsMessageAdapter")
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;

    use super::*;

    #[test]
    fn test_simple_message() {
        /* let epoch_settings = fake_data::epoch_settings();
        let message = ToCardanoTransactionsProofsMessageAdapter::adapt(epoch_settings.clone());

        assert_eq!(epoch_settings.epoch, message.epoch); */
        todo!();
    }
}
