//! A set of extension traits to add test utilities to this crate `entities`

use std::collections::HashMap;
use std::ops::Range;

use anyhow::anyhow;

use crate::StdResult;
use crate::crypto_helper::{
    MKMap, MKMapNode, MKTree, MKTreeNode, MKTreeStoreInMemory, MKTreeStorer,
};
use crate::entities::{
    BlockNumber, BlockRange, CardanoTransactionsSetProof, ProtocolParameters, SingleSignature,
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

impl BlockRangeTestExtension for BlockRange {
    fn new(start: u64, end: u64) -> Self {
        Self {
            inner_range: BlockNumber(start)..BlockNumber(end),
        }
    }

    fn try_add(&self, other: &BlockRange) -> StdResult<BlockRange> {
        if self.inner_range.end.max(other.inner_range.end)
            < self.inner_range.start.min(other.inner_range.start)
        {
            return Err(anyhow!(
                "BlockRange cannot be added as they don't strictly overlap"
            ));
        }

        Ok(Self {
            inner_range: Range {
                start: self.inner_range.start.min(other.inner_range.start),
                end: self.inner_range.end.max(other.inner_range.end),
            },
        })
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
        let mut transactions_by_block_ranges: HashMap<BlockRange, Vec<TransactionHash>> =
            HashMap::new();
        for (block_number, transaction_hash) in leaves {
            let block_range = BlockRange::from_block_number(*block_number);
            transactions_by_block_ranges
                .entry(block_range)
                .or_default()
                .push(transaction_hash.to_owned());
        }
        let mk_map = MKMap::<_, _, MKTreeStoreInMemory>::new(
            transactions_by_block_ranges
                .into_iter()
                .try_fold(
                    vec![],
                    |mut acc,
                     (block_range, transactions)|
                     -> StdResult<Vec<(_, MKMapNode<_, S>)>> {
                        acc.push((block_range, MKTree::<S>::new(&transactions)?.into()));
                        Ok(acc)
                    },
                )?
                .as_slice(),
        )?;
        let mk_leaves: Vec<MKTreeNode> =
            transactions_hashes.iter().map(|h| h.to_owned().into()).collect();
        let mk_proof = mk_map.compute_proof(&mk_leaves)?;
        Ok(Self::new(transactions_hashes, mk_proof))
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
    fn test_block_range_try_add() {
        assert_eq!(
            BlockRange::new(1, 10).try_add(&BlockRange::new(1, 10)).unwrap(),
            BlockRange::new(1, 10)
        );
        assert_eq!(
            BlockRange::new(1, 10).try_add(&BlockRange::new(1, 11)).unwrap(),
            BlockRange::new(1, 11)
        );
        assert_eq!(
            BlockRange::new(1, 10).try_add(&BlockRange::new(2, 10)).unwrap(),
            BlockRange::new(1, 10)
        );
    }
}
