use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;
use slog::{debug, Logger};

use crate::{
    crypto_helper::{MKMap, MKMapNode, MKTreeNode, MKTreeStorer},
    entities::{BlockNumber, BlockRange, ProtocolMessage, ProtocolMessagePartKey},
    logging::LoggerExtensions,
    signable_builder::SignableBuilder,
    StdResult,
};

#[cfg(test)]
use mockall::automock;

/// Cardano transactions importer
#[cfg_attr(test, automock)]
#[async_trait]
pub trait TransactionsImporter: Send + Sync {
    /// Returns all transactions up to the given beacon
    async fn import(&self, up_to_beacon: BlockNumber) -> StdResult<()>;
}

/// Block Range Merkle roots retriever
#[cfg_attr(test, automock)]
#[async_trait]
pub trait BlockRangeRootRetriever<S: MKTreeStorer>: Send + Sync {
    /// Returns a Merkle map of the block ranges roots up to a given beacon
    async fn retrieve_block_range_roots<'a>(
        &'a self,
        up_to_beacon: BlockNumber,
    ) -> StdResult<Box<dyn Iterator<Item = (BlockRange, MKTreeNode)> + 'a>>;

    /// Returns a Merkle map of the block ranges roots up to a given beacon
    async fn compute_merkle_map_from_block_range_roots(
        &self,
        up_to_beacon: BlockNumber,
    ) -> StdResult<MKMap<BlockRange, MKMapNode<BlockRange, S>, S>> {
        let block_range_roots_iterator = self
            .retrieve_block_range_roots(up_to_beacon)
            .await?
            .map(|(block_range, root)| (block_range, root.into()));
        let mk_hash_map = MKMap::new_from_iter(block_range_roots_iterator)
        .with_context(|| "BlockRangeRootRetriever failed to compute the merkelized structure that proves ownership of the transaction")?;

        Ok(mk_hash_map)
    }
}

/// A [CardanoTransactionsSignableBuilder] builder
pub struct CardanoTransactionsSignableBuilder<S: MKTreeStorer> {
    transaction_importer: Arc<dyn TransactionsImporter>,
    block_range_root_retriever: Arc<dyn BlockRangeRootRetriever<S>>,
    logger: Logger,
}

impl<S: MKTreeStorer> CardanoTransactionsSignableBuilder<S> {
    /// Constructor
    pub fn new(
        transaction_importer: Arc<dyn TransactionsImporter>,
        block_range_root_retriever: Arc<dyn BlockRangeRootRetriever<S>>,
        logger: Logger,
    ) -> Self {
        Self {
            transaction_importer,
            block_range_root_retriever,
            logger: logger.new_with_component_name::<Self>(),
        }
    }
}

#[async_trait]
impl<S: MKTreeStorer> SignableBuilder<BlockNumber> for CardanoTransactionsSignableBuilder<S> {
    async fn compute_protocol_message(
        &self,
        beacon: BlockNumber,
        seed_protocol_message: ProtocolMessage,
    ) -> StdResult<ProtocolMessage> {
        debug!(
            self.logger,
            "Compute protocol message for CardanoTransactions at block_number: {beacon}"
        );

        self.transaction_importer.import(beacon).await?;

        let mk_root = self
            .block_range_root_retriever
            .compute_merkle_map_from_block_range_roots(beacon)
            .await?
            .compute_root()?;

        let mut protocol_message = seed_protocol_message;
        protocol_message.set_message_part(
            ProtocolMessagePartKey::CardanoTransactionsMerkleRoot,
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
        entities::CardanoTransaction,
        test_utils::{CardanoTransactionsBuilder, TestLogger},
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
        let seed_protocol_message = ProtocolMessage::new();
        let transactions = CardanoTransactionsBuilder::new().build_transactions(3);
        let mk_map = compute_mk_map_from_transactions(transactions.clone());
        let mut transaction_importer = MockTransactionsImporter::new();
        transaction_importer
            .expect_import()
            .return_once(move |_| Ok(()));
        let retrieved_transactions = transactions.clone();
        let mut block_range_root_retriever = MockBlockRangeRootRetriever::new();
        block_range_root_retriever
            .expect_compute_merkle_map_from_block_range_roots()
            .return_once(move |_| Ok(compute_mk_map_from_transactions(retrieved_transactions)));

        let cardano_transactions_signable_builder = CardanoTransactionsSignableBuilder::new(
            Arc::new(transaction_importer),
            Arc::new(block_range_root_retriever),
            TestLogger::stdout(),
        );

        // Action
        let signable = cardano_transactions_signable_builder
            .compute_protocol_message(block_number, seed_protocol_message)
            .await
            .unwrap();

        // Assert
        let mut signable_expected = ProtocolMessage::new();
        signable_expected.set_message_part(
            ProtocolMessagePartKey::CardanoTransactionsMerkleRoot,
            mk_map.compute_root().unwrap().to_hex(),
        );
        signable_expected.set_message_part(
            ProtocolMessagePartKey::LatestBlockNumber,
            format!("{}", block_number),
        );
        assert_eq!(signable_expected, signable);
    }

    #[tokio::test]
    async fn test_compute_signable_with_no_block_range_root_return_error() {
        let block_number = BlockNumber(50);
        let seed_protocol_message = ProtocolMessage::new();
        let mut transaction_importer = MockTransactionsImporter::new();
        transaction_importer.expect_import().return_once(|_| Ok(()));
        let mut block_range_root_retriever = MockBlockRangeRootRetriever::new();
        block_range_root_retriever
            .expect_compute_merkle_map_from_block_range_roots()
            .return_once(move |_| Ok(compute_mk_map_from_transactions(vec![])));
        let cardano_transactions_signable_builder = CardanoTransactionsSignableBuilder::new(
            Arc::new(transaction_importer),
            Arc::new(block_range_root_retriever),
            TestLogger::stdout(),
        );

        let result = cardano_transactions_signable_builder
            .compute_protocol_message(block_number, seed_protocol_message)
            .await;

        assert!(result.is_err());
    }
}
