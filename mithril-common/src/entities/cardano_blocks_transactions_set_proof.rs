use crate::StdResult;
use crate::crypto_helper::{MKMapProof, ProtocolMkProof};
use crate::entities::{BlockHash, CardanoBlockWithTransactions, TransactionHash};

use super::BlockRange;

/// A cryptographic proof of a set of Cardano blocks with transactions is included in the global Cardano blocks and transactions set
#[derive(Clone, Debug, PartialEq)]
pub struct CardanoBlocksTransactionsSetProof {
    /// Certified blocks with their transactions hashes included
    pub(crate) blocks: Vec<CardanoBlockWithTransactions>,

    /// Proof of inclusion of the Blocks and transactions
    pub(crate) proof: ProtocolMkProof,
}

impl CardanoBlocksTransactionsSetProof {
    /// CardanoBlocksTransactionsSetProof factory
    pub fn new<T: Into<MKMapProof<BlockRange>>>(
        blocks: Vec<CardanoBlockWithTransactions>,
        proof: T,
    ) -> Self {
        Self {
            blocks,
            proof: ProtocolMkProof::new(proof.into()),
        }
    }

    /// Return the hex encoded merkle root of this proof
    pub fn merkle_root(&self) -> String {
        self.proof.compute_root().to_hex()
    }

    /// Get the hashes of the blocks certified by this proof
    pub fn blocks_hashes(&self) -> impl Iterator<Item = &BlockHash> + '_ {
        self.blocks.iter().map(|b| &b.block_hash)
    }

    /// Get the hashes of the transactions certified by this proof
    pub fn transactions_hashes(&self) -> impl Iterator<Item = &TransactionHash> + '_ {
        self.blocks.iter().flat_map(|b| &b.transactions_hashes)
    }

    /// Verify that blocks and transactions set proof is valid
    pub fn verify(&self) -> StdResult<()> {
        self.proof.verify()?;
        for node in self.blocks.iter().cloned().flat_map(|b| b.into_mk_tree_node()) {
            self.proof.contains(&node.into())?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::crypto_helper::MKTreeStoreInMemory;
    use crate::entities::{BlockNumber, SlotNumber};
    use crate::test::entities_extensions::CardanoBlocksTransactionsSetProofTestExtension;

    use super::*;

    fn test_data_set() -> [CardanoBlockWithTransactions; 2] {
        [
            CardanoBlockWithTransactions::new(
                "block_hash-1",
                BlockNumber(10),
                SlotNumber(30),
                vec!["tx-1", "tx-2"],
            ),
            CardanoBlockWithTransactions::new(
                "block_hash-2",
                BlockNumber(20),
                SlotNumber(40),
                vec!["tx-3", "tx-5"],
            ),
        ]
    }

    fn tamper<T: Clone>(item: &T, f: fn(&mut T)) -> T {
        let mut item = item.clone();
        f(&mut item);
        item
    }

    #[test]
    fn should_verify_where_all_block_and_transactions_are_contained_in_the_proof() {
        let proof =
            CardanoBlocksTransactionsSetProof::from_blocks::<MKTreeStoreInMemory>(&test_data_set())
                .unwrap();

        proof.verify().expect("The proof should be valid");
    }

    #[test]
    fn should_not_verify_where_at_least_one_block_hash_is_not_contained_in_the_proof() {
        let proof =
            CardanoBlocksTransactionsSetProof::from_blocks::<MKTreeStoreInMemory>(&test_data_set())
                .unwrap();
        let proof = CardanoBlocksTransactionsSetProof {
            blocks: vec![
                test_data_set()[0].clone(),
                tamper(&test_data_set()[1], |block| {
                    block.block_hash = "block_hash-not-included".to_string();
                }),
            ],
            ..proof
        };

        proof.verify().expect_err("The proof should be invalid");
    }

    #[test]
    fn should_not_verify_where_at_least_one_block_number_is_not_contained_in_the_proof() {
        let proof =
            CardanoBlocksTransactionsSetProof::from_blocks::<MKTreeStoreInMemory>(&test_data_set())
                .unwrap();
        let proof = CardanoBlocksTransactionsSetProof {
            blocks: vec![
                test_data_set()[0].clone(),
                tamper(&test_data_set()[1], |block| {
                    block.slot_number += 1;
                }),
            ],
            ..proof
        };

        proof.verify().expect_err("The proof should be invalid");
    }

    #[test]
    fn should_not_verify_where_at_least_one_slot_number_is_not_contained_in_the_proof() {
        let proof =
            CardanoBlocksTransactionsSetProof::from_blocks::<MKTreeStoreInMemory>(&test_data_set())
                .unwrap();
        let proof = CardanoBlocksTransactionsSetProof {
            blocks: vec![
                test_data_set()[0].clone(),
                tamper(&test_data_set()[1], |block| {
                    block.slot_number += 1;
                }),
            ],
            ..proof
        };

        proof.verify().expect_err("The proof should be invalid");
    }
}
