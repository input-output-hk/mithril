use anyhow::Context;

use crate::StdResult;
use crate::crypto_helper::{
    MKMap, MKMapNode, MKProof, MKTree, MKTreeNode, MKTreeStoreInMemory, MKTreeStorer,
};
use crate::entities::{
    BlockNumber, BlockRange, CardanoBlock, CardanoBlockWithTransactions, CardanoTransaction,
    IntoMKTreeNode, MkSetProof,
};
use crate::messages::{CardanoBlocksProofsMessage, CardanoTransactionsProofsV2Message};
use crate::test::crypto_helper::mkmap_helpers;
use crate::test::entities_extensions::BlockNumberTestExtension;

/// Extension trait adding test utilities to [MKProof]
pub trait MKProofTestExtension {
    /// `TEST ONLY` - Build a [MKProof] based on the given leaves
    fn from_leaves<T: Into<MKTreeNode> + Clone>(leaves: &[T]) -> StdResult<MKProof>;

    /// `TEST ONLY` - Build a [MKProof] based on the given leaves
    fn from_subset_of_leaves<T: Into<MKTreeNode> + Clone>(
        leaves: &[T],
        leaves_to_verify: &[T],
    ) -> StdResult<MKProof>;
}

impl MKProofTestExtension for MKProof {
    fn from_leaves<T: Into<MKTreeNode> + Clone>(leaves: &[T]) -> StdResult<MKProof> {
        Self::from_subset_of_leaves(leaves, leaves)
    }

    fn from_subset_of_leaves<T: Into<MKTreeNode> + Clone>(
        leaves: &[T],
        leaves_to_verify: &[T],
    ) -> StdResult<MKProof> {
        fn list_to_mknode<T: Into<MKTreeNode> + Clone>(hashes: &[T]) -> Vec<MKTreeNode> {
            hashes.iter().map(|h| h.clone().into()).collect()
        }

        let leaves = list_to_mknode(leaves);
        let leaves_to_verify = list_to_mknode(leaves_to_verify);

        let mktree = MKTree::<MKTreeStoreInMemory>::new(&leaves)
            .with_context(|| "MKTree creation should not fail")?;
        mktree.compute_proof(&leaves_to_verify)
    }
}

/// Extension trait adding test utilities to [MKMap]
pub trait MKMapTestExtension<S: MKTreeStorer> {
    /// `TEST ONLY` - Helper to create a MKMap from a list of cardano blocks with transactions
    fn from_blocks_with_transactions(
        leaves: &[CardanoBlockWithTransactions],
    ) -> StdResult<MKMap<BlockRange, MKMapNode<BlockRange, S>, S>>;

    /// `TEST ONLY` - Helper to create a proof for a list of transactions hashes
    ///
    /// IMPORTANT: the `all_leaves` list must contains all the leaves used to create the MKMap
    fn compute_proof_for_transactions_hashes<H: AsRef<str>>(
        &self,
        transactions_hashes_to_prove: &[H],
        all_leaves: &[CardanoBlockWithTransactions],
    ) -> StdResult<MkSetProof<CardanoTransaction>>;

    /// `TEST ONLY` - Helper to create a proof message for a list of transactions hashes
    ///
    /// IMPORTANT: the `all_leaves` list must contains all the leaves used to create the MKMap
    fn compute_proof_message_for_transactions_hashes<H: AsRef<str>>(
        &self,
        certificate_hash: &str,
        transactions_hashes_to_prove: &[H],
        all_leaves: &[CardanoBlockWithTransactions],
    ) -> StdResult<CardanoTransactionsProofsV2Message> {
        let proof =
            self.compute_proof_for_transactions_hashes(transactions_hashes_to_prove, all_leaves)?;

        let message = CardanoTransactionsProofsV2Message::new(
            certificate_hash,
            Some(proof.try_into()?),
            Vec::new(),
            BlockNumber(9999),
        );

        Ok(message)
    }

    /// `TEST ONLY` - Helper to create a proof for a list of blocks hashes
    ///
    /// IMPORTANT: the `all_leaves` list must contains all the leaves used to create the MKMap
    fn compute_proof_for_blocks_hashes<H: AsRef<str>>(
        &self,
        blocks_hashes_to_prove: &[H],
        all_leaves: &[CardanoBlockWithTransactions],
    ) -> StdResult<MkSetProof<CardanoBlock>>;

    /// `TEST ONLY` - Helper to create a proof message for a list of blocks hashes
    ///
    /// IMPORTANT: the `all_leaves` list must contains all the leaves used to create the MKMap
    fn compute_proof_message_for_blocks_hashes<H: AsRef<str>>(
        &self,
        certificate_hash: &str,
        blocks_hashes_to_prove: &[H],
        all_leaves: &[CardanoBlockWithTransactions],
    ) -> StdResult<CardanoBlocksProofsMessage> {
        let proof = self.compute_proof_for_blocks_hashes(blocks_hashes_to_prove, all_leaves)?;

        let message = CardanoBlocksProofsMessage::new(
            certificate_hash,
            Some(proof.try_into()?),
            Vec::new(),
            BlockNumber(9999),
        );

        Ok(message)
    }
}

impl<S: MKTreeStorer> MKMapTestExtension<S> for MKMap<BlockRange, MKMapNode<BlockRange, S>, S> {
    fn from_blocks_with_transactions(
        leaves: &[CardanoBlockWithTransactions],
    ) -> StdResult<MKMap<BlockRange, MKMapNode<BlockRange, S>, S>> {
        let node_per_block_range = BlockNumber::group_items_by_block_range(
            leaves
                .iter()
                .flat_map(|l| l.clone().into_mk_tree_node())
                .map(|n| (n.block_number(), n)),
        );

        mkmap_helpers::fold_nodes_per_block_range_into_mkmap::<_, _, S>(node_per_block_range)
    }

    fn compute_proof_for_transactions_hashes<H: AsRef<str>>(
        &self,
        transactions_hashes_to_prove: &[H],
        all_leaves: &[CardanoBlockWithTransactions],
    ) -> StdResult<MkSetProof<CardanoTransaction>> {
        let hashes_to_prove: Vec<_> =
            transactions_hashes_to_prove.iter().map(AsRef::as_ref).collect();
        let leaves_to_prove: Vec<CardanoTransaction> = all_leaves
            .iter()
            .flat_map(|l| l.clone().into_transactions())
            .filter(|tx| hashes_to_prove.contains(&tx.transaction_hash.as_str()))
            .collect();
        let mk_tree_nodes_to_prove: Vec<MKTreeNode> = leaves_to_prove
            .iter()
            .cloned()
            .map(|l| l.into_mk_tree_node())
            .collect();

        let proof = self.compute_proof(&mk_tree_nodes_to_prove)?;
        Ok(MkSetProof::new(leaves_to_prove, proof))
    }

    fn compute_proof_for_blocks_hashes<H: AsRef<str>>(
        &self,
        blocks_hashes_to_prove: &[H],
        all_leaves: &[CardanoBlockWithTransactions],
    ) -> StdResult<MkSetProof<CardanoBlock>> {
        let hashes_to_prove: Vec<_> = blocks_hashes_to_prove.iter().map(AsRef::as_ref).collect();
        let leaves_to_prove: Vec<CardanoBlock> = all_leaves
            .iter()
            .filter(|b| hashes_to_prove.contains(&b.block_hash.as_ref()))
            .cloned()
            .map(Into::into)
            .collect();
        let mk_tree_nodes_to_prove: Vec<MKTreeNode> = leaves_to_prove
            .iter()
            .cloned()
            .map(|l| l.into_mk_tree_node())
            .collect();

        let proof = self.compute_proof(&mk_tree_nodes_to_prove)?;
        Ok(MkSetProof::new(leaves_to_prove, proof))
    }
}

#[cfg(test)]
mod tests {
    use crate::entities::SlotNumber;

    use super::*;

    #[test]
    fn compute_proofs_for_blocks_and_txs_from_the_same_mkmap() {
        let blocks_with_txs = [
            CardanoBlockWithTransactions::new(
                "block_hash-10",
                BlockNumber(10),
                SlotNumber(100),
                vec!["tx_hash-1", "tx_hash-2"],
            ),
            CardanoBlockWithTransactions::new(
                "block_hash-15",
                BlockNumber(15),
                SlotNumber(150),
                vec!["tx_hash-4"],
            ),
            CardanoBlockWithTransactions::new(
                "block_hash-16",
                BlockNumber(16),
                SlotNumber(160),
                vec!["tx_hash-5", "tx_hash-6", "tx_hash-7"],
            ),
        ];

        let mk_map =
            MKMap::<_, _, MKTreeStoreInMemory>::from_blocks_with_transactions(&blocks_with_txs)
                .unwrap();

        let proof_for_blocks_subset = mk_map
            .compute_proof_for_blocks_hashes(&["block_hash-10", "block_hash-16"], &blocks_with_txs)
            .unwrap();
        proof_for_blocks_subset.verify().unwrap();

        let proof_for_txs_subset = mk_map
            .compute_proof_for_transactions_hashes(&["tx_hash-1", "tx_hash-4"], &blocks_with_txs)
            .unwrap();
        proof_for_txs_subset.verify().unwrap();

        assert_eq!(
            proof_for_blocks_subset.merkle_root(),
            proof_for_txs_subset.merkle_root()
        );
    }
}
