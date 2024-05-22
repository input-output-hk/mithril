use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;
use slog::{debug, Logger};

use crate::{
    crypto_helper::{MKMap, MKMapNode, MKTreeNode},
    entities::{BlockRange, CardanoDbBeacon, ProtocolMessage, ProtocolMessagePartKey},
    signable_builder::SignableBuilder,
    StdResult,
};

use crate::entities::ImmutableFileNumber;
#[cfg(test)]
use mockall::automock;

/// Cardano transactions importer
#[cfg_attr(test, automock)]
#[async_trait]
pub trait TransactionsImporter: Send + Sync {
    /// Returns all transactions up to the given beacon
    async fn import(&self, up_to_beacon: ImmutableFileNumber) -> StdResult<()>;
}

/// Block Range Merkle roots retriever
#[cfg_attr(test, automock)]
#[async_trait]
pub trait BlockRangeRootRetriever: Send + Sync {
    /// Returns a Merkle map of the block ranges roots up to a given beacon
    async fn retrieve_block_range_roots(
        &self,
        up_to_beacon: ImmutableFileNumber,
    ) -> StdResult<Box<dyn Iterator<Item = (BlockRange, MKTreeNode)>>>;

    /// Returns a Merkle map of the block ranges roots up to a given beacon
    async fn compute_merkle_map_from_block_range_roots(
        &self,
        up_to_beacon: ImmutableFileNumber,
    ) -> StdResult<MKMap<BlockRange, MKMapNode<BlockRange>>> {
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
pub struct CardanoTransactionsSignableBuilder {
    transaction_importer: Arc<dyn TransactionsImporter>,
    block_range_root_retriever: Arc<dyn BlockRangeRootRetriever>,
    logger: Logger,
}

impl CardanoTransactionsSignableBuilder {
    /// Constructor
    pub fn new(
        transaction_importer: Arc<dyn TransactionsImporter>,
        block_range_root_retriever: Arc<dyn BlockRangeRootRetriever>,
        logger: Logger,
    ) -> Self {
        Self {
            transaction_importer,
            block_range_root_retriever,
            logger,
        }
    }
}

#[async_trait]
impl SignableBuilder<CardanoDbBeacon> for CardanoTransactionsSignableBuilder {
    async fn compute_protocol_message(
        &self,
        beacon: CardanoDbBeacon,
    ) -> StdResult<ProtocolMessage> {
        debug!(
            self.logger,
            "Compute protocol message for CardanoTransactions at beacon: {beacon}"
        );

        self.transaction_importer
            .import(beacon.immutable_file_number)
            .await?;

        let mk_root = self
            .block_range_root_retriever
            .compute_merkle_map_from_block_range_roots(beacon.immutable_file_number)
            .await?
            .compute_root()?;

        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::CardanoTransactionsMerkleRoot,
            mk_root.to_hex(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::LatestImmutableFileNumber,
            beacon.immutable_file_number.to_string(),
        );

        Ok(protocol_message)
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        entities::CardanoTransaction,
        test_utils::{CardanoTransactionsBuilder, TestLogger},
    };

    use super::*;

    fn compute_mk_map_from_transactions(
        transactions: Vec<CardanoTransaction>,
    ) -> MKMap<BlockRange, MKMapNode<BlockRange>> {
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
        let beacon = CardanoDbBeacon {
            immutable_file_number: 14,
            ..CardanoDbBeacon::default()
        };
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
            .compute_protocol_message(beacon.clone())
            .await
            .unwrap();

        // Assert
        let mut signable_expected = ProtocolMessage::new();
        signable_expected.set_message_part(
            ProtocolMessagePartKey::CardanoTransactionsMerkleRoot,
            mk_map.compute_root().unwrap().to_hex(),
        );
        signable_expected.set_message_part(
            ProtocolMessagePartKey::LatestImmutableFileNumber,
            "14".to_string(),
        );
        assert_eq!(signable_expected, signable);
    }

    #[tokio::test]
    async fn test_compute_signable_with_no_block_range_root_return_error() {
        let beacon = CardanoDbBeacon::default();
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
            .compute_protocol_message(beacon.clone())
            .await;

        assert!(result.is_err());
    }
}
