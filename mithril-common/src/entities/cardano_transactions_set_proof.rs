use crate::crypto_helper::{MKProof, MKTree, MKTreeNode, MKTreeStore, ProtocolMkProof};
use crate::entities::TransactionHash;
use crate::messages::CardanoTransactionsSetProofMessagePart;
use crate::StdResult;

/// A cryptographic proof of a set of Cardano transactions is included in the global Cardano transactions set
#[derive(Clone, Debug, PartialEq)]
pub struct CardanoTransactionsSetProof {
    /// Hashes of the certified transactions
    transactions_hashes: Vec<TransactionHash>,

    /// Proof of the transactions
    transactions_proof: ProtocolMkProof,
}

impl CardanoTransactionsSetProof {
    /// CardanoTransactionsSetProof factory
    pub fn new(transactions_hashes: Vec<TransactionHash>, transactions_proof: MKProof) -> Self {
        Self {
            transactions_hashes,
            transactions_proof: ProtocolMkProof::new(transactions_proof),
        }
    }

    /// Get the hashes of the transactions certified by this proof
    pub fn transactions_hashes(&self) -> &[TransactionHash] {
        &self.transactions_hashes
    }

    /// Verify that transactions set proof is valid
    pub fn verify(&self) -> StdResult<()> {
        self.transactions_proof.verify()?;
        self.transactions_proof.contains(
            self.transactions_hashes
                .iter()
                .map(|h| h.to_owned().into())
                .collect::<Vec<_>>()
                .as_slice(),
        )?;

        Ok(())
    }

    #[cfg(feature = "test_tools")]
    /// Retrieve a dummy proof (for test only)
    pub fn dummy() -> Self {
        let transactions_hashes = vec![
            "tx-1".to_string(),
            "tx-2".to_string(),
            "tx-3".to_string(),
            "tx-4".to_string(),
            "tx-5".to_string(),
        ];
        let leaves: Vec<MKTreeNode> = transactions_hashes
            .iter()
            .cloned()
            .map(|h| h.into())
            .collect();
        let store = MKTreeStore::default();
        let mktree = MKTree::new(&leaves, &store).unwrap();

        Self::new(transactions_hashes, mktree.compute_proof(&leaves).unwrap())
    }
}

impl From<CardanoTransactionsSetProof> for CardanoTransactionsSetProofMessagePart {
    fn from(proof: CardanoTransactionsSetProof) -> Self {
        Self {
            transactions_hashes: proof.transactions_hashes,
            proof: proof.transactions_proof.to_json_hex().unwrap(),
        }
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Context;

    use crate::crypto_helper::{MKTree, MKTreeNode, MKTreeStore};

    use super::*;

    fn build_proof<T: Into<MKTreeNode> + Clone>(
        leaves: &[T],
        leaves_to_verify: &[T],
    ) -> StdResult<MKProof> {
        let leaves = to_mknode(leaves);
        let leaves_to_verify = to_mknode(leaves_to_verify);

        let store = MKTreeStore::default();
        let mktree =
            MKTree::new(&leaves, &store).with_context(|| "MKTree creation should not fail")?;
        mktree.compute_proof(&leaves_to_verify)
    }

    fn to_mknode<T: Into<MKTreeNode> + Clone>(hashes: &[T]) -> Vec<MKTreeNode> {
        hashes.iter().map(|h| h.clone().into()).collect()
    }

    #[test]
    fn should_verify_where_all_hashes_are_contained_in_the_proof() {
        let transaction_hashes = vec![
            "tx-1".to_string(),
            "tx-2".to_string(),
            "tx-3".to_string(),
            "tx-4".to_string(),
            "tx-5".to_string(),
        ];
        let transaction_hashes_to_verify = &transaction_hashes[0..2];
        let transactions_proof =
            build_proof(&transaction_hashes, transaction_hashes_to_verify).unwrap();

        let proof = CardanoTransactionsSetProof::new(
            transaction_hashes_to_verify.to_vec(),
            transactions_proof,
        );
        proof.verify().expect("The proof should be valid");
    }

    #[test]
    fn shouldnt_verify_where_at_least_one_hash_is_not_contained_in_the_proof() {
        let transaction_hashes = vec![
            "tx-1".to_string(),
            "tx-2".to_string(),
            "tx-3".to_string(),
            "tx-4".to_string(),
            "tx-5".to_string(),
        ];
        let transactions_proof =
            build_proof(&transaction_hashes, &transaction_hashes[0..2]).unwrap();
        let transaction_hashes_not_verified = &transaction_hashes[1..3];

        let proof = CardanoTransactionsSetProof::new(
            transaction_hashes_not_verified.to_vec(),
            transactions_proof,
        );
        proof.verify().expect_err("The proof should be invalid");
    }
}
