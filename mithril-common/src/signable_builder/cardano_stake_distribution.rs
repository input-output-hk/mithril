use anyhow::anyhow;
use std::sync::Arc;

use async_trait::async_trait;

use crate::{
    crypto_helper::{MKTree, MKTreeNode, MKTreeStoreInMemory},
    entities::{Epoch, ProtocolMessage, ProtocolMessagePartKey, StakeDistribution},
    signable_builder::SignableBuilder,
    StdResult,
};

#[cfg(test)]
use mockall::automock;

/// Stake Distribution Retriever
#[cfg_attr(test, automock)]
#[async_trait]
pub trait StakeDistributionRetriever: Send + Sync {
    /// Retrieve the [StakeDistribution] for a given epoch
    async fn retrieve(&self, epoch: Epoch) -> StdResult<Option<StakeDistribution>>;
}

struct StakeDistributionEntry(String, u64);

impl StakeDistributionEntry {
    pub fn new(pool_id: &str, stake: u64) -> Self {
        Self(pool_id.to_string(), stake)
    }
}

impl From<StakeDistributionEntry> for MKTreeNode {
    fn from(entry: StakeDistributionEntry) -> Self {
        MKTreeNode::new(format!("{}{}", entry.0, entry.1).into())
    }
}

/// A [CardanoStakeDistributionSignableBuilder] builder
pub struct CardanoStakeDistributionSignableBuilder {
    cardano_stake_distribution_retriever: Arc<dyn StakeDistributionRetriever>,
}

impl CardanoStakeDistributionSignableBuilder {
    /// Constructor
    pub fn new(cardano_stake_distribution_retriever: Arc<dyn StakeDistributionRetriever>) -> Self {
        Self {
            cardano_stake_distribution_retriever,
        }
    }

    /// Compute the Merkle tree of a given [StakeDistribution]
    pub fn compute_merkle_tree_from_stake_distribution(
        pools_with_stake: StakeDistribution,
    ) -> StdResult<MKTree<MKTreeStoreInMemory>> {
        let leaves: Vec<MKTreeNode> = pools_with_stake
            .iter()
            .map(|(k, v)| StakeDistributionEntry::new(k, *v).into())
            .collect();

        MKTree::new(&leaves)
    }
}

#[async_trait]
impl SignableBuilder<Epoch> for CardanoStakeDistributionSignableBuilder {
    async fn compute_protocol_message(
        &self,
        epoch: Epoch,
        seed_protocol_message: ProtocolMessage,
    ) -> StdResult<ProtocolMessage> {
        let pools_with_stake = self
            .cardano_stake_distribution_retriever
            .retrieve(epoch.offset_to_cardano_stake_distribution_snapshot_epoch())
            .await?.ok_or(anyhow!(
                "CardanoStakeDistributionSignableBuilder could not find the stake distribution for epoch: '{epoch}'"
            ))?;

        let mk_tree = Self::compute_merkle_tree_from_stake_distribution(pools_with_stake)?;

        let mut protocol_message = seed_protocol_message;
        protocol_message.set_message_part(
            ProtocolMessagePartKey::CardanoStakeDistributionEpoch,
            epoch.to_string(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::CardanoStakeDistributionMerkleRoot,
            mk_tree.compute_root()?.to_hex(),
        );

        Ok(protocol_message)
    }
}

#[cfg(test)]
mod tests {
    use mockall::predicate::eq;

    use crate::entities::ProtocolMessagePartKey;

    use super::*;

    fn is_merkle_tree_equals(
        first_pools_with_stake: StakeDistribution,
        second_pools_with_stake: StakeDistribution,
    ) -> bool {
        let first_merkle_tree =
            CardanoStakeDistributionSignableBuilder::compute_merkle_tree_from_stake_distribution(
                first_pools_with_stake,
            )
            .unwrap();
        let second_merkle_tree =
            CardanoStakeDistributionSignableBuilder::compute_merkle_tree_from_stake_distribution(
                second_pools_with_stake,
            )
            .unwrap();

        first_merkle_tree.compute_root().unwrap() == second_merkle_tree.compute_root().unwrap()
    }

    #[test]
    fn compute_merkle_tree_equals() {
        assert!(is_merkle_tree_equals(
            StakeDistribution::from([("pool-123".to_string(), 100)]),
            StakeDistribution::from([("pool-123".to_string(), 100)]),
        ));

        assert!(is_merkle_tree_equals(
            StakeDistribution::from([("pool-123".to_string(), 100), ("pool-456".to_string(), 150)]),
            StakeDistribution::from([("pool-456".to_string(), 150), ("pool-123".to_string(), 100)])
        ));
    }

    #[test]
    fn compute_merkle_tree_not_equals() {
        assert!(!is_merkle_tree_equals(
            StakeDistribution::from([("pool-123".to_string(), 100)]),
            StakeDistribution::from([("pool-456".to_string(), 100)]),
        ));

        assert!(!is_merkle_tree_equals(
            StakeDistribution::from([("pool-123".to_string(), 100)]),
            StakeDistribution::from([("pool-123".to_string(), 999)]),
        ));
    }

    #[tokio::test]
    async fn compute_protocol_message_returns_error_when_no_cardano_stake_distribution_found() {
        let epoch = Epoch(1);
        let seed_protocol_message = ProtocolMessage::new();

        let mut cardano_stake_distribution_retriever = MockStakeDistributionRetriever::new();
        cardano_stake_distribution_retriever
            .expect_retrieve()
            .return_once(move |_| Ok(None));
        let cardano_stake_distribution_signable_builder =
            CardanoStakeDistributionSignableBuilder::new(Arc::new(
                cardano_stake_distribution_retriever,
            ));

        cardano_stake_distribution_signable_builder
            .compute_protocol_message(epoch, seed_protocol_message)
            .await
            .expect_err("Should return an error when no cardano stake distribution found");
    }

    #[tokio::test]
    async fn compute_protocol_message_returns_signable_and_retrieve_with_epoch_offset() {
        let epoch = Epoch(1);
        let epoch_to_retrieve = Epoch(3);
        let stake_distribution = StakeDistribution::from([("pool-123".to_string(), 100)]);
        let stake_distribution_clone = stake_distribution.clone();
        let seed_protocol_message = ProtocolMessage::new();

        let mut pools_with_stake_retriever = MockStakeDistributionRetriever::new();
        pools_with_stake_retriever
            .expect_retrieve()
            .with(eq(epoch_to_retrieve))
            .return_once(move |_| Ok(Some(stake_distribution)));
        let cardano_stake_distribution_signable_builder =
            CardanoStakeDistributionSignableBuilder::new(Arc::new(pools_with_stake_retriever));

        let signable = cardano_stake_distribution_signable_builder
            .compute_protocol_message(epoch, seed_protocol_message)
            .await
            .unwrap();

        let expected_mktree =
            CardanoStakeDistributionSignableBuilder::compute_merkle_tree_from_stake_distribution(
                stake_distribution_clone,
            )
            .unwrap();
        let mut signable_expected = ProtocolMessage::new();
        signable_expected.set_message_part(
            ProtocolMessagePartKey::CardanoStakeDistributionEpoch,
            epoch.to_string(),
        );
        signable_expected.set_message_part(
            ProtocolMessagePartKey::CardanoStakeDistributionMerkleRoot,
            expected_mktree.compute_root().unwrap().to_hex(),
        );
        assert_eq!(signable_expected, signable);
    }
}
