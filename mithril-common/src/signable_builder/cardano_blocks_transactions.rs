use std::collections::BTreeSet;
use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;

use crate::{
    StdResult,
    crypto_helper::{MKMap, MKMapNode, MKTree, MKTreeNode, MKTreeStorer},
    entities::{
        BlockNumber, BlockRange, CardanoBlockTransactionMkTreeNode, ProtocolMessage,
        ProtocolMessagePartKey,
    },
    signable_builder::SignableBuilder,
};

/// Cardano blocks and transactions importer
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait BlocksTransactionsImporter: Send + Sync {
    /// Import all transactions up to the given beacon into the system
    async fn import(&self, up_to_beacon: BlockNumber) -> StdResult<()>;
}

/// Block Range Merkle roots retriever
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait BlockRangeRootRetriever<S: MKTreeStorer>: Send + Sync {
    /// Returns the Merkle root of the block ranges up to a given beacon
    ///
    /// Only complete block ranges should be considered
    async fn retrieve_block_range_roots<'a>(
        &'a self,
        up_to_beacon: BlockNumber,
    ) -> StdResult<Box<dyn Iterator<Item = (BlockRange, MKTreeNode)> + 'a>>;

    /// Returns the all nodes constitutive of the given block range
    async fn retrieve_block_ranges_nodes(
        &self,
        block_range: BlockRange,
    ) -> StdResult<BTreeSet<CardanoBlockTransactionMkTreeNode>>;

    /// Returns a Merkle map of the block ranges roots up to a given beacon
    async fn compute_merkle_map_from_block_range_roots(
        &self,
        up_to_beacon: BlockNumber,
    ) -> StdResult<MKMap<BlockRange, MKMapNode<BlockRange, S>, S>> {
        let block_range_roots_iterator = self
            .retrieve_block_range_roots(up_to_beacon)
            .await?
            .map(|(block_range, root)| (block_range, root.into()));
        let mut mk_hash_map = MKMap::new_from_iter(block_range_roots_iterator)
            .with_context(|| "BlockRangeRootRetriever failed to compute the merkelized structure that proves ownership of the transaction")?;

        let latest_block_range = BlockRange::from_block_number(up_to_beacon);
        if !latest_block_range.is_complete_up_to(up_to_beacon) {
            let latest_partial_block_range_nodes =
                self.retrieve_block_ranges_nodes(latest_block_range.clone()).await?;

            if !latest_partial_block_range_nodes.is_empty() {
                let latest_partial_block_range_root =
                    MKTree::<S>::new_from_iter(latest_partial_block_range_nodes).with_context(
                        || "BlockRangeRootRetriever failed to compute partial latest block range",
                    )?;
                mk_hash_map
                    .insert(latest_block_range, latest_partial_block_range_root.into())
                    .with_context(
                        || "BlockRangeRootRetriever failed to insert partial latest block range",
                    )?;
            }
        }

        Ok(mk_hash_map)
    }
}

/// A [CardanoBlocksTransactionsSignableBuilder] builder
pub struct CardanoBlocksTransactionsSignableBuilder<S: MKTreeStorer> {
    blocks_transactions_importer: Arc<dyn BlocksTransactionsImporter>,
    block_range_root_retriever: Arc<dyn BlockRangeRootRetriever<S>>,
}

impl<S: MKTreeStorer> CardanoBlocksTransactionsSignableBuilder<S> {
    /// Constructor
    pub fn new(
        blocks_transactions_importer: Arc<dyn BlocksTransactionsImporter>,
        block_range_root_retriever: Arc<dyn BlockRangeRootRetriever<S>>,
    ) -> Self {
        Self {
            blocks_transactions_importer,
            block_range_root_retriever,
        }
    }
}

#[async_trait]
impl<S: MKTreeStorer> SignableBuilder<BlockNumber> for CardanoBlocksTransactionsSignableBuilder<S> {
    async fn compute_protocol_message(&self, beacon: BlockNumber) -> StdResult<ProtocolMessage> {
        self.blocks_transactions_importer.import(beacon).await?;

        let mk_root = self
            .block_range_root_retriever
            .compute_merkle_map_from_block_range_roots(beacon)
            .await?
            .compute_root()?;

        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::CardanoBlocksTransactionsMerkleRoot,
            mk_root.to_hex(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::LatestBlockNumber,
            beacon.to_string(),
        );

        Ok(protocol_message)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        crypto_helper::MKTreeStoreInMemory,
        entities::{CardanoTransaction, SlotNumber},
        test::builder::CardanoTransactionsBuilder,
    };

    use super::*;

    fn compute_mk_map_from_transactions(
        transactions: Vec<CardanoTransaction>,
    ) -> MKMap<BlockRange, MKMapNode<BlockRange, MKTreeStoreInMemory>, MKTreeStoreInMemory> {
        MKMap::new_from_iter(transactions.iter().map(|tx| {
            (
                BlockRange::from_block_number(tx.block_number),
                MKMapNode::TreeNode(tx.transaction_hash.clone().into()),
            )
        }))
        .unwrap()
    }

    #[tokio::test]
    async fn test_compute_signable() {
        // Arrange
        let block_number = BlockNumber(1453);
        let transactions = CardanoTransactionsBuilder::new().build_transactions(3);
        let mk_map = compute_mk_map_from_transactions(transactions.clone());
        let mut blocks_transactions_importer = MockBlocksTransactionsImporter::new();
        blocks_transactions_importer
            .expect_import()
            .return_once(move |_| Ok(()));
        let retrieved_transactions = transactions.clone();
        let mut block_range_root_retriever = MockBlockRangeRootRetriever::new();
        block_range_root_retriever
            .expect_compute_merkle_map_from_block_range_roots()
            .return_once(move |_| Ok(compute_mk_map_from_transactions(retrieved_transactions)));

        let signable_builder = CardanoBlocksTransactionsSignableBuilder::new(
            Arc::new(blocks_transactions_importer),
            Arc::new(block_range_root_retriever),
        );

        // Action
        let signable = signable_builder.compute_protocol_message(block_number).await.unwrap();

        // Assert
        let mut signable_expected = ProtocolMessage::new();
        signable_expected.set_message_part(
            ProtocolMessagePartKey::CardanoBlocksTransactionsMerkleRoot,
            mk_map.compute_root().unwrap().to_hex(),
        );
        signable_expected.set_message_part(
            ProtocolMessagePartKey::LatestBlockNumber,
            format!("{block_number}"),
        );
        assert_eq!(signable_expected, signable);
    }

    #[tokio::test]
    async fn test_compute_signable_with_no_block_range_root_return_error() {
        let block_number = BlockNumber(50);
        let mut blocks_transactions_importer = MockBlocksTransactionsImporter::new();
        blocks_transactions_importer.expect_import().return_once(|_| Ok(()));
        let mut block_range_root_retriever = MockBlockRangeRootRetriever::new();
        block_range_root_retriever
            .expect_compute_merkle_map_from_block_range_roots()
            .return_once(move |_| Ok(compute_mk_map_from_transactions(vec![])));
        let signable_builder = CardanoBlocksTransactionsSignableBuilder::new(
            Arc::new(blocks_transactions_importer),
            Arc::new(block_range_root_retriever),
        );

        let result = signable_builder.compute_protocol_message(block_number).await;

        assert!(result.is_err());
    }

    mod compute_merkle_map_from_block_range_roots {
        use super::*;

        /// A dummy [BlockRangeRootRetriever] that returns a fixed set of block ranges and ignores
        /// the beacons given.
        ///
        /// Most only be used to test the behavior of [BlockRangeRootRetriever::compute_merkle_map_from_block_range_roots]
        struct DumbBlockRangeRootRetriever<S: MKTreeStorer> {
            complete_block_ranges: Vec<(BlockRange, MKTreeNode)>,
            retrieve_block_ranges_nodes_result: BTreeSet<CardanoBlockTransactionMkTreeNode>,
            _phantom: std::marker::PhantomData<S>,
        }

        impl DumbBlockRangeRootRetriever<MKTreeStoreInMemory> {
            fn new(
                complete_block_ranges: Vec<(BlockRange, MKTreeNode)>,
                retrieve_block_ranges_nodes_result: BTreeSet<CardanoBlockTransactionMkTreeNode>,
            ) -> Self {
                Self {
                    complete_block_ranges,
                    retrieve_block_ranges_nodes_result,
                    _phantom: std::marker::PhantomData,
                }
            }
        }

        #[async_trait]
        impl<S: MKTreeStorer> BlockRangeRootRetriever<S> for DumbBlockRangeRootRetriever<S> {
            async fn retrieve_block_range_roots<'a>(
                &'a self,
                _up_to_beacon: BlockNumber,
            ) -> StdResult<Box<dyn Iterator<Item = (BlockRange, MKTreeNode)> + 'a>> {
                Ok(Box::new(self.complete_block_ranges.iter().cloned()))
            }

            async fn retrieve_block_ranges_nodes(
                &self,
                _block_range: BlockRange,
            ) -> StdResult<BTreeSet<CardanoBlockTransactionMkTreeNode>> {
                Ok(self.retrieve_block_ranges_nodes_result.clone())
            }
        }

        #[tokio::test]
        async fn compute_with_only_complete_block_ranges() {
            let block_range_roots = vec![
                (
                    BlockRange::from_block_number(BlockNumber(15)),
                    MKTreeNode::from_hex("AAAA").unwrap(),
                ),
                (
                    BlockRange::from_block_number(BlockNumber(30)),
                    MKTreeNode::from_hex("BBBB").unwrap(),
                ),
                (
                    BlockRange::from_block_number(BlockNumber(45)),
                    MKTreeNode::from_hex("CCCC").unwrap(),
                ),
            ];

            let retriever =
                DumbBlockRangeRootRetriever::new(block_range_roots.clone(), BTreeSet::new());

            let retrieved_mk_map = retriever
                .compute_merkle_map_from_block_range_roots(BlockNumber(59))
                .await
                .unwrap();

            let retrieved_mk_map_root = retrieved_mk_map.compute_root().unwrap();
            let expected_mk_map_root = MKMap::<
                BlockRange,
                MKMapNode<BlockRange, MKTreeStoreInMemory>,
                MKTreeStoreInMemory,
            >::compute_root_from_iter(
                block_range_roots.into_iter().map(|(k, v)| (k, v.into()))
            )
            .unwrap();
            assert_eq!(expected_mk_map_root, retrieved_mk_map_root);
        }

        #[tokio::test]
        async fn compute_with_a_partial_block_range_when_last_range_is_not_empty() {
            let stored_block_ranges_roots = vec![
                (
                    BlockRange::from_block_number(BlockNumber(15)),
                    MKTreeNode::from_hex("AAAA").unwrap(),
                ),
                (
                    BlockRange::from_block_number(BlockNumber(30)),
                    MKTreeNode::from_hex("BBBB").unwrap(),
                ),
                (
                    BlockRange::from_block_number(BlockNumber(45)),
                    MKTreeNode::from_hex("CCCC").unwrap(),
                ),
            ];
            let latest_partial_block_range_nodes = BTreeSet::from([
                CardanoBlockTransactionMkTreeNode::Block {
                    block_hash: "block_hash-62".to_string(),
                    block_number: BlockNumber(62),
                    slot_number: SlotNumber(162),
                },
                CardanoBlockTransactionMkTreeNode::Transaction {
                    transaction_hash: "tx_hash-1".to_string(),
                    block_number: BlockNumber(62),
                    slot_number: SlotNumber(162),
                    block_hash: "block_hash-62".to_string(),
                },
            ]);

            let retriever = DumbBlockRangeRootRetriever::new(
                stored_block_ranges_roots.clone(),
                latest_partial_block_range_nodes.clone(),
            );

            let retrieved_mk_map = retriever
                .compute_merkle_map_from_block_range_roots(BlockNumber(63))
                .await
                .unwrap();

            let retrieved_mk_map_root = retrieved_mk_map.compute_root().unwrap();
            let expected_mk_map_root = MKMap::<
                BlockRange,
                MKMapNode<BlockRange, MKTreeStoreInMemory>,
                MKTreeStoreInMemory,
            >::compute_root_from_iter(
                [
                    stored_block_ranges_roots,
                    vec![(
                        BlockRange::from_block_number(BlockNumber(60)),
                        MKTree::<MKTreeStoreInMemory>::compute_root_from_iter(
                            latest_partial_block_range_nodes,
                        )
                        .unwrap(),
                    )],
                ]
                .concat()
                .into_iter()
                .map(|(k, v)| (k, v.into())),
            )
            .unwrap();
            assert_eq!(expected_mk_map_root, retrieved_mk_map_root);
        }

        #[tokio::test]
        async fn compute_with_a_partial_block_range_when_last_range_is_empty() {
            let stored_block_ranges_roots = vec![
                (
                    BlockRange::from_block_number(BlockNumber(15)),
                    MKTreeNode::from_hex("AAAA").unwrap(),
                ),
                (
                    BlockRange::from_block_number(BlockNumber(30)),
                    MKTreeNode::from_hex("BBBB").unwrap(),
                ),
                (
                    BlockRange::from_block_number(BlockNumber(45)),
                    MKTreeNode::from_hex("CCCC").unwrap(),
                ),
            ];

            let retriever = DumbBlockRangeRootRetriever::new(
                stored_block_ranges_roots.clone(),
                BTreeSet::new(),
            );

            let retrieved_mk_map = retriever
                .compute_merkle_map_from_block_range_roots(BlockNumber(63))
                .await
                .unwrap();

            let retrieved_mk_map_root = retrieved_mk_map.compute_root().unwrap();
            let expected_mk_map_root = MKMap::<
                BlockRange,
                MKMapNode<BlockRange, MKTreeStoreInMemory>,
                MKTreeStoreInMemory,
            >::compute_root_from_iter(
                stored_block_ranges_roots.into_iter().map(|(k, v)| (k, v.into())),
            )
            .unwrap();
            assert_eq!(expected_mk_map_root, retrieved_mk_map_root);
        }
    }
}
