//! A set of extension traits to add test utilities to this crate `entities`

use std::collections::{BTreeSet, HashMap};

use strum::IntoEnumIterator;

use crate::StdResult;
use crate::crypto_helper::MKTreeStorer;
use crate::entities::{
    BlockNumber, BlockRange, CardanoBlock, CardanoTransaction, CardanoTransactionsSetProof,
    IntoMKTreeNode, MkSetProof, ProtocolParameters, SignedEntityTypeDiscriminants, SingleSignature,
    SingleSignatureAuthenticationStatus, TransactionHash,
};
use crate::test::builder::{MithrilFixtureBuilder, StakeDistributionGenerationMethod};
use crate::test::crypto_helper::mkmap_helpers;

/// Extension trait adding test utilities to [BlockRange]
pub trait BlockRangeTestExtension {
    /// `TEST ONLY` - BlockRange factory
    fn new(start: u64, end: u64) -> Self;

    /// `TEST ONLY` - Try to add two BlockRanges
    fn try_add(&self, other: &BlockRange) -> StdResult<BlockRange>;
}

/// Extension trait adding test utilities to [BlockNumber]
pub trait BlockNumberTestExtension {
    /// `TEST ONLY` - Group an iterator over a tuple of block number and item
    fn group_items_by_block_range<N, I>(iter: I) -> HashMap<BlockRange, Vec<N>>
    where
        I: Iterator<Item = (BlockNumber, N)>;
}

impl BlockNumberTestExtension for BlockNumber {
    fn group_items_by_block_range<N, I>(iter: I) -> HashMap<BlockRange, Vec<N>>
    where
        I: Iterator<Item = (BlockNumber, N)>,
    {
        let mut result: HashMap<BlockRange, Vec<N>> = HashMap::new();

        for (block_number, item) in iter.into_iter() {
            let block_range = BlockRange::from_block_number(block_number);
            result.entry(block_range).or_default().push(item);
        }

        result
    }
}

/// Extension trait adding test utilities to [CardanoTransactionsSetProof]
pub trait CardanoTransactionsSetProofTestExtension {
    /// `TEST ONLY` - Helper to create a proof from a list of leaves
    fn from_leaves<S: MKTreeStorer>(
        leaves: &[(BlockNumber, TransactionHash)],
    ) -> StdResult<CardanoTransactionsSetProof>;
}

impl CardanoTransactionsSetProofTestExtension for CardanoTransactionsSetProof {
    fn from_leaves<S: MKTreeStorer>(
        leaves: &[(BlockNumber, TransactionHash)],
    ) -> StdResult<CardanoTransactionsSetProof> {
        let transactions_hashes: Vec<TransactionHash> =
            leaves.iter().map(|(_, t)| t.into()).collect();
        let transactions_by_block_ranges =
            BlockNumber::group_items_by_block_range(leaves.iter().cloned());
        let mk_map = mkmap_helpers::fold_nodes_per_block_range_into_mkmap::<_, _, S>(
            transactions_by_block_ranges,
        )?;
        let mk_proof = mk_map.compute_proof(&transactions_hashes)?;
        Ok(Self::new(transactions_hashes, mk_proof))
    }
}

/// Extension trait adding test utilities to [MkSetProof]
pub trait MkSetProofTestExtension<L: IntoMKTreeNode + Clone> {
    /// `TEST ONLY` - Helper to create a proof from a list of leaves
    fn from_leaves<S: MKTreeStorer>(leaves: &[L]) -> StdResult<MkSetProof<L>>;
}

impl MkSetProofTestExtension<CardanoBlock> for MkSetProof<CardanoBlock> {
    fn from_leaves<S: MKTreeStorer>(
        leaves: &[CardanoBlock],
    ) -> StdResult<MkSetProof<CardanoBlock>> {
        let node_per_block_range = BlockNumber::group_items_by_block_range(
            leaves.iter().map(|l| (l.block_number, l.clone().into_mk_tree_node())),
        );
        let all_nodes = node_per_block_range.values().flatten().cloned().collect::<Vec<_>>();
        let mk_map =
            mkmap_helpers::fold_nodes_per_block_range_into_mkmap::<_, _, S>(node_per_block_range)?;
        let proof = mk_map.compute_proof(&all_nodes)?;

        Ok(MkSetProof::new(leaves.to_vec(), proof))
    }
}

impl MkSetProofTestExtension<CardanoTransaction> for MkSetProof<CardanoTransaction> {
    fn from_leaves<S: MKTreeStorer>(
        leaves: &[CardanoTransaction],
    ) -> StdResult<MkSetProof<CardanoTransaction>> {
        let node_per_block_range = BlockNumber::group_items_by_block_range(
            leaves.iter().map(|l| (l.block_number, l.clone().into_mk_tree_node())),
        );
        let all_nodes = node_per_block_range.values().flatten().cloned().collect::<Vec<_>>();
        let mk_map =
            mkmap_helpers::fold_nodes_per_block_range_into_mkmap::<_, _, S>(node_per_block_range)?;
        let proof = mk_map.compute_proof(&all_nodes)?;

        Ok(MkSetProof::new(leaves.to_vec(), proof))
    }
}

/// Extension trait adding test utilities to [SignedEntityTypeDiscriminants]
pub trait SignedEntityTypeDiscriminantsTestExtension {
    /// `TEST ONLY` - Get all the discriminants with unstable values
    fn all_with_unstable() -> BTreeSet<SignedEntityTypeDiscriminants>;

    /// `TEST ONLY` - Get all the discriminants with unstable values as a Vec
    fn all_with_unstable_vec() -> Vec<SignedEntityTypeDiscriminants> {
        Self::all_with_unstable().into_iter().collect()
    }

    /// `TEST ONLY` - Get all the discriminants with unstable values as a String, separated by the given separator
    fn all_with_unstable_string(separator: &str) -> String {
        Self::all_with_unstable()
            .into_iter()
            .map(|d| d.to_string())
            .collect::<Vec<_>>()
            .join(separator)
    }
}

impl SignedEntityTypeDiscriminantsTestExtension for SignedEntityTypeDiscriminants {
    fn all_with_unstable() -> BTreeSet<SignedEntityTypeDiscriminants> {
        SignedEntityTypeDiscriminants::iter().collect()
    }
}

/// Extension trait adding test utilities to [SingleSignature]
pub trait SingleSignatureTestExtension {
    /// `TEST ONLY` - Create a fake [SingleSignature] with valid cryptographic data for testing purposes.
    fn fake<TPartyId: Into<String>, TMessage: Into<String>>(
        party_id: TPartyId,
        message: TMessage,
    ) -> SingleSignature;
}

impl SingleSignatureTestExtension for SingleSignature {
    fn fake<TPartyId: Into<String>, TMessage: Into<String>>(
        party_id: TPartyId,
        message: TMessage,
    ) -> SingleSignature {
        let party_id = party_id.into();
        let message = message.into();

        let fixture = MithrilFixtureBuilder::default()
            .with_stake_distribution(StakeDistributionGenerationMethod::Custom(
                std::collections::BTreeMap::from([(party_id.to_string(), 100)]),
            ))
            .with_protocol_parameters(ProtocolParameters::new(1, 1, 1.0))
            .build();
        let signature = fixture.signers_fixture()[0].sign(&message).unwrap();

        Self {
            party_id,
            signature: signature.signature,
            won_indexes: vec![10, 15],
            authentication_status: SingleSignatureAuthenticationStatus::Unauthenticated,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn group_list_of_blocknumber_and_string_tuple_by_block_range() {
        let input = [
            (BlockNumber(1), "item_1"),
            (BlockNumber(2), "item_2"),
            (BlockNumber(3), "item_3"),
            (BlockNumber(16), "item_16"),
            (BlockNumber(17), "item_17"),
        ];

        let grouped_items = BlockNumber::group_items_by_block_range(input.iter().cloned());

        assert_eq!(
            HashMap::from([
                (
                    BlockRange::from_block_number(BlockNumber(1)),
                    vec!["item_1", "item_2", "item_3"]
                ),
                (
                    BlockRange::from_block_number(BlockNumber(16)),
                    vec!["item_16", "item_17"]
                ),
            ]),
            grouped_items
        );
    }
}
