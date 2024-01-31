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
        transaction_hashes_to_certify: Vec<TransactionHash>,
    ) -> CardanoTransactionsProofsMessage {
        let transactions_hashes_certified = transactions_set_proofs
            .iter()
            .flat_map(|proof| proof.transactions_hashes().to_vec())
            .collect::<Vec<_>>();
        let transactions_hashes_not_certified = transaction_hashes_to_certify
            .iter()
            .filter(|hash| !transactions_hashes_certified.contains(hash))
            .cloned()
            .collect::<Vec<_>>();
        CardanoTransactionsProofsMessage::new(
            transactions_set_proofs,
            transactions_hashes_not_certified,
        )
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Context;
    use mithril_common::{
        crypto_helper::{MKProof, MKTree, MKTreeNode, MKTreeStore},
        StdResult,
    };

    use super::*;

    fn build_proof(leaves: &[MKTreeNode]) -> StdResult<MKProof> {
        let store = MKTreeStore::default();
        let mktree =
            MKTree::new(leaves, &store).with_context(|| "MKTree creation should not fail")?;
        mktree.compute_proof(leaves)
    }

    #[test]
    fn test_simple_message() {
        let transaction_hashes = &[
            "tx-1".to_string(),
            "tx-2".to_string(),
            "tx-3".to_string(),
            "tx-4".to_string(),
            "tx-5".to_string(),
            "tx-6".to_string(),
            "tx-7".to_string(),
        ];
        let transactions_hashes_certified = &transaction_hashes[0..5];
        let transactions_hashes_non_certified = &transaction_hashes[5..];

        let mut transactions_set_proofs = Vec::new();
        for transaction_hashes_in_chunk in transactions_hashes_certified.chunks(2) {
            let mk_proof = build_proof(
                transaction_hashes_in_chunk
                    .iter()
                    .map(|h| h.to_owned().into())
                    .collect::<Vec<_>>()
                    .as_slice(),
            )
            .unwrap();
            transactions_set_proofs.push(CardanoTransactionsSetProof::new(
                transaction_hashes_in_chunk.to_vec(),
                mk_proof,
            ))
        }

        let message = ToCardanoTransactionsProofsMessageAdapter::adapt(
            transactions_set_proofs.clone(),
            transaction_hashes.to_vec(),
        );
        let expected_message = CardanoTransactionsProofsMessage::new(
            transactions_set_proofs,
            transactions_hashes_non_certified.to_vec(),
        );
        assert_eq!(expected_message, message);
    }
}
