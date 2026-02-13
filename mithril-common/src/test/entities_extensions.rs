//! A set of extension traits to add test utilities to this crate `entities`

use std::collections::HashMap;

use crate::StdResult;
use crate::crypto_helper::{MKMap, MKMapNode, MKTree, MKTreeNode, MKTreeStorer};
use crate::entities::{
    BlockNumber, BlockRange, CardanoBlock, CardanoTransaction, CardanoTransactionsSetProof,
    IntoMKTreeNode, MkSetProof, ProtocolParameters, SingleSignature,
    SingleSignatureAuthenticationStatus, TransactionHash,
};
use crate::test::builder::{MithrilFixtureBuilder, StakeDistributionGenerationMethod};

/// Extension trait adding test utilities to [BlockRange]
pub trait BlockRangeTestExtension {
    /// `TEST ONLY` - BlockRange factory
    fn new(start: u64, end: u64) -> Self;

    /// `TEST ONLY` - Try to add two BlockRanges
    fn try_add(&self, other: &BlockRange) -> StdResult<BlockRange>;
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
        let transactions_by_block_ranges = private::group_by_block_range(leaves.iter().cloned());
        let mk_map = private::fold_nodes_per_block_range_into_mkmap::<_, _, S>(
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
        let node_per_block_range = private::group_by_block_range(
            leaves.iter().map(|l| (l.block_number, l.clone().into_mk_tree_node())),
        );
        let all_nodes = node_per_block_range.values().flatten().cloned().collect::<Vec<_>>();
        let mk_map =
            private::fold_nodes_per_block_range_into_mkmap::<_, _, S>(node_per_block_range)?;
        let proof = mk_map.compute_proof(&all_nodes)?;

        Ok(MkSetProof::new(leaves.to_vec(), proof))
    }
}

impl MkSetProofTestExtension<CardanoTransaction> for MkSetProof<CardanoBlock> {
    fn from_leaves<S: MKTreeStorer>(
        leaves: &[CardanoTransaction],
    ) -> StdResult<MkSetProof<CardanoTransaction>> {
        let node_per_block_range = private::group_by_block_range(
            leaves.iter().map(|l| (l.block_number, l.clone().into_mk_tree_node())),
        );
        let all_nodes = node_per_block_range.values().flatten().cloned().collect::<Vec<_>>();
        let mk_map =
            private::fold_nodes_per_block_range_into_mkmap::<_, _, S>(node_per_block_range)?;
        let proof = mk_map.compute_proof(&all_nodes)?;

        Ok(MkSetProof::new(leaves.to_vec(), proof))
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

mod private {
    use super::*;

    pub(super) fn group_by_block_range<N, I>(iter: I) -> HashMap<BlockRange, Vec<N>>
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

    pub(super) fn fold_nodes_per_block_range_into_mkmap<N, I, S>(
        nodes_per_block_range: I,
    ) -> StdResult<MKMap<BlockRange, MKMapNode<BlockRange, S>, S>>
    where
        N: Into<MKTreeNode> + Clone,
        I: IntoIterator<Item = (BlockRange, Vec<N>)>,
        S: MKTreeStorer,
    {
        let mk_map = MKMap::<_, _, S>::new(
            nodes_per_block_range
                .into_iter()
                .try_fold(
                    vec![],
                    |mut acc, (block_range, nodes)| -> StdResult<Vec<(_, MKMapNode<_, S>)>> {
                        acc.push((block_range, MKTree::<S>::new(&nodes)?.into()));
                        Ok(acc)
                    },
                )?
                .as_slice(),
        )?;

        Ok(mk_map)
    }
}
