use crate::crypto_helper::{MKMapProof, ProtocolMkProof};
use crate::entities::TransactionHash;
use crate::messages::CardanoTransactionsSetProofMessagePart;
use crate::{StdError, StdResult};

use super::BlockRange;

cfg_test_tools! {
    use crate::crypto_helper::{MKMap, MKTree, MKTreeNode, MKMapNode};
    use crate::entities::BlockNumber;
    use std::collections::HashMap;
}

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
    pub fn new<T: Into<MKMapProof<BlockRange>>>(
        transactions_hashes: Vec<TransactionHash>,
        transactions_proof: T,
    ) -> Self {
        Self {
            transactions_hashes,
            transactions_proof: ProtocolMkProof::new(transactions_proof.into()),
        }
    }

    /// Return the hex encoded merkle root of this proof
    pub fn merkle_root(&self) -> String {
        self.transactions_proof.compute_root().to_hex()
    }

    /// Get the hashes of the transactions certified by this proof
    pub fn transactions_hashes(&self) -> &[TransactionHash] {
        &self.transactions_hashes
    }

    /// Verify that transactions set proof is valid
    pub fn verify(&self) -> StdResult<()> {
        self.transactions_proof.verify()?;
        for hash in &self.transactions_hashes {
            self.transactions_proof.contains(&hash.to_owned().into())?;
        }

        Ok(())
    }

    cfg_test_tools! {
        /// Retrieve a dummy proof (for test only)
        pub fn dummy() -> Self {
            let leaves = vec![
                (BlockNumber(0), "tx-1".to_string()),
                (BlockNumber(1), "tx-2".to_string()),
                (BlockNumber(1), "tx-3".to_string()),
                (BlockNumber(10), "tx-4".to_string()),
                (BlockNumber(20), "tx-5".to_string()),
                (BlockNumber(22), "tx-6".to_string()),
            ];

            Self::from_leaves(&leaves).unwrap()
        }

        /// Helper to create a proof from a list of leaves
        pub fn from_leaves(leaves: &[(BlockNumber, TransactionHash)]) -> StdResult<Self> {
            let transactions_hashes: Vec<TransactionHash> =
                leaves.iter().map(|(_, t)| t.into()).collect();
            let mut transactions_by_block_ranges: HashMap<BlockRange, Vec<TransactionHash>> =
                HashMap::new();
            for (block_number, transaction_hash) in leaves {
                let block_range = BlockRange::from_block_number(*block_number);
                transactions_by_block_ranges
                    .entry(block_range)
                    .or_default()
                    .push(transaction_hash.to_owned());
            }
            let mk_map = MKMap::new(
                transactions_by_block_ranges
                    .into_iter()
                    .try_fold(
                        vec![],
                        |mut acc, (block_range, transactions)| -> StdResult<Vec<(_, MKMapNode<_>)>> {
                            acc.push((block_range, MKTree::new(&transactions)?.into()));
                            Ok(acc)
                        },
                    )?
                    .as_slice(),
            )?;
            let mk_leaves: Vec<MKTreeNode> = transactions_hashes
                .iter()
                .map(|h| h.to_owned().into())
                .collect();
            let mk_proof = mk_map.compute_proof(&mk_leaves)?;
            Ok(Self::new(transactions_hashes, mk_proof))
        }

    }
}

impl TryFrom<CardanoTransactionsSetProof> for CardanoTransactionsSetProofMessagePart {
    type Error = StdError;

    fn try_from(proof: CardanoTransactionsSetProof) -> Result<Self, Self::Error> {
        Ok(Self {
            transactions_hashes: proof.transactions_hashes,
            proof: proof.transactions_proof.to_json_hex()?,
        })
    }
}

impl TryFrom<CardanoTransactionsSetProofMessagePart> for CardanoTransactionsSetProof {
    type Error = StdError;

    fn try_from(proof: CardanoTransactionsSetProofMessagePart) -> Result<Self, Self::Error> {
        Ok(Self {
            transactions_hashes: proof.transactions_hashes,
            transactions_proof: ProtocolMkProof::from_json_hex(&proof.proof)?,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_verify_where_all_hashes_are_contained_in_the_proof() {
        let leaves = vec![
            (BlockNumber(0), "tx-1".to_string()),
            (BlockNumber(1), "tx-2".to_string()),
            (BlockNumber(1), "tx-3".to_string()),
            (BlockNumber(10), "tx-4".to_string()),
            (BlockNumber(20), "tx-5".to_string()),
            (BlockNumber(22), "tx-6".to_string()),
        ];
        let proof = CardanoTransactionsSetProof::from_leaves(&leaves).unwrap();

        proof.verify().expect("The proof should be valid");
    }

    #[test]
    fn shouldnt_verify_where_at_least_one_hash_is_not_contained_in_the_proof() {
        let leaves = vec![
            (BlockNumber(0), "tx-1".to_string()),
            (BlockNumber(1), "tx-2".to_string()),
            (BlockNumber(1), "tx-3".to_string()),
            (BlockNumber(10), "tx-4".to_string()),
            (BlockNumber(20), "tx-5".to_string()),
            (BlockNumber(22), "tx-6".to_string()),
        ];
        let proof = CardanoTransactionsSetProof::from_leaves(&leaves).unwrap();
        let mut transactions_hashes_tampered = proof.transactions_hashes().to_vec();
        transactions_hashes_tampered.push("tx-123".to_string());
        let proof = CardanoTransactionsSetProof {
            transactions_hashes: transactions_hashes_tampered,
            ..proof
        };

        proof.verify().expect_err("The proof should be invalid");
    }
}
