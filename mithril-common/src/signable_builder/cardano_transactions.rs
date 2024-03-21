use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::Context;
use async_trait::async_trait;
use slog::{debug, Logger};

use crate::{
    cardano_transaction_parser::TransactionParser,
    crypto_helper::{MKMap, MKMapNode, MKTree, MKTreeNode},
    entities::{
        Beacon, BlockRange, CardanoTransaction, ProtocolMessage, ProtocolMessagePartKey,
        TransactionHash,
    },
    signable_builder::SignableBuilder,
    StdResult,
};

#[cfg(test)]
use mockall::automock;

/// Cardano transactions store
#[cfg_attr(test, automock)]
#[async_trait]
pub trait TransactionStore: Send + Sync {
    /// Store list of transactions
    async fn store_transactions(&self, transactions: &[CardanoTransaction]) -> StdResult<()>;
}

/// A [CardanoTransactionsSignableBuilder] builder
pub struct CardanoTransactionsSignableBuilder {
    transaction_parser: Arc<dyn TransactionParser>,
    transaction_store: Arc<dyn TransactionStore>,
    logger: Logger,
    dirpath: PathBuf,
}

impl CardanoTransactionsSignableBuilder {
    /// Constructor
    pub fn new(
        transaction_parser: Arc<dyn TransactionParser>,
        transaction_store: Arc<dyn TransactionStore>,
        dirpath: &Path,
        logger: Logger,
    ) -> Self {
        Self {
            transaction_parser,
            transaction_store,
            logger,
            dirpath: dirpath.to_owned(),
        }
    }

    // Note: Code duplicated from aggregator Prover service as is.
    // This will be not be the case when we use the cached intermediate merkle roots.
    fn compute_merkle_map_from_transactions(
        &self,
        transactions: &[CardanoTransaction],
    ) -> StdResult<MKMap<BlockRange, MKMapNode<BlockRange>>> {
        let mut transactions_by_block_ranges: HashMap<BlockRange, Vec<TransactionHash>> =
            HashMap::new();
        for transaction in transactions {
            let block_range = BlockRange::from_block_number(transaction.block_number);
            transactions_by_block_ranges
                .entry(block_range)
                .or_default()
                .push(transaction.transaction_hash.to_owned());
        }
        let mk_hash_map = MKMap::new(
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
        )
        .with_context(|| "ProverService failed to compute the merkelized structure that proves ownership of the transaction")?;

        Ok(mk_hash_map)
    }

    fn compute_merkle_root(&self, transactions: &[CardanoTransaction]) -> StdResult<MKTreeNode> {
        let mk_map = self.compute_merkle_map_from_transactions(transactions)?;

        let mk_root = mk_map.compute_root().with_context(|| {
            "CardanoTransactionsSignableBuilder failed to compute MKHashMap root"
        })?;

        Ok(mk_root)
    }
}

#[async_trait]
impl SignableBuilder<Beacon> for CardanoTransactionsSignableBuilder {
    // TODO: return a protocol message computed from the transactions when it's ready to be implemented
    async fn compute_protocol_message(&self, beacon: Beacon) -> StdResult<ProtocolMessage> {
        debug!(
            self.logger,
            "Compute protocol message for CardanoTransactions at beacon: {beacon}"
        );

        let transactions = self
            .transaction_parser
            .parse(&self.dirpath, &beacon)
            .await?;
        debug!(
            self.logger,
            "Retrieved {} Cardano transactions at beacon: {beacon}",
            transactions.len()
        );

        let transaction_chunk_size = 100;
        for transactions_in_chunk in transactions.chunks(transaction_chunk_size) {
            self.transaction_store
                .store_transactions(transactions_in_chunk)
                .await?;
        }

        let mk_root = self.compute_merkle_root(&transactions)?;

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
    use crate::cardano_transaction_parser::DumbTransactionParser;
    use crate::signable_builder::MockTransactionStore;

    use super::*;
    use slog::Drain;

    fn create_logger() -> slog::Logger {
        let decorator = slog_term::PlainDecorator::new(slog_term::TestStdoutWriter);
        let drain = slog_term::CompactFormat::new(decorator).build().fuse();
        let drain = slog_async::Async::new(drain).build().fuse();
        slog::Logger::root(Arc::new(drain), slog::o!())
    }

    #[tokio::test]
    async fn test_compute_merkle_root() {
        let transaction_1 = CardanoTransaction::new("tx-hash-123", 1, 1);
        let transaction_2 = CardanoTransaction::new("tx-hash-456", 2, 1);
        let transaction_3 = CardanoTransaction::new("tx-hash-789", 3, 1);
        let transaction_4 = CardanoTransaction::new("tx-hash-abc", 4, 1);

        let transactions_set_reference = vec![
            transaction_1.clone(),
            transaction_2.clone(),
            transaction_3.clone(),
        ];
        let cardano_transaction_signable_builder = CardanoTransactionsSignableBuilder::new(
            Arc::new(DumbTransactionParser::new(
                transactions_set_reference.clone(),
            )),
            Arc::new(MockTransactionStore::new()),
            Path::new("/tmp"),
            create_logger(),
        );

        let merkle_root_reference = cardano_transaction_signable_builder
            .compute_merkle_root(&transactions_set_reference)
            .unwrap();
        {
            let transactions_set = vec![transaction_1.clone()];
            let mk_root = cardano_transaction_signable_builder
                .compute_merkle_root(&transactions_set)
                .unwrap();
            assert_ne!(merkle_root_reference, mk_root);
        }
        {
            let transactions_set = vec![transaction_1.clone(), transaction_2.clone()];
            let mk_root = cardano_transaction_signable_builder
                .compute_merkle_root(&transactions_set)
                .unwrap();
            assert_ne!(merkle_root_reference, mk_root);
        }
        {
            let transactions_set = vec![
                transaction_1.clone(),
                transaction_2.clone(),
                transaction_3.clone(),
                transaction_4.clone(),
            ];
            let mk_root = cardano_transaction_signable_builder
                .compute_merkle_root(&transactions_set)
                .unwrap();
            assert_ne!(merkle_root_reference, mk_root);
        }

        {
            // Transactions in a different order returns a different merkle root.
            let transactions_set = vec![
                transaction_1.clone(),
                transaction_3.clone(),
                transaction_2.clone(),
            ];
            let mk_root = cardano_transaction_signable_builder
                .compute_merkle_root(&transactions_set)
                .unwrap();
            assert_ne!(merkle_root_reference, mk_root);
        }
    }

    #[tokio::test]
    async fn test_compute_signable() {
        // Arrange
        let beacon = Beacon {
            immutable_file_number: 14,
            ..Beacon::default()
        };
        let transactions = vec![
            CardanoTransaction::new("tx-hash-123", 1, 11),
            CardanoTransaction::new("tx-hash-456", 2, 12),
            CardanoTransaction::new("tx-hash-789", 3, 13),
        ];
        let transaction_parser = Arc::new(DumbTransactionParser::new(transactions.clone()));
        let mut mock_transaction_store = MockTransactionStore::new();
        mock_transaction_store
            .expect_store_transactions()
            .returning(|_| Ok(()));
        let transaction_store = Arc::new(mock_transaction_store);
        let cardano_transactions_signable_builder = CardanoTransactionsSignableBuilder::new(
            transaction_parser,
            transaction_store,
            Path::new("/tmp"),
            create_logger(),
        );

        // Action
        let signable = cardano_transactions_signable_builder
            .compute_protocol_message(beacon.clone())
            .await
            .unwrap();

        // Assert
        let mk_root = cardano_transactions_signable_builder
            .compute_merkle_root(&transactions)
            .unwrap();
        let mut signable_expected = ProtocolMessage::new();
        signable_expected.set_message_part(
            ProtocolMessagePartKey::CardanoTransactionsMerkleRoot,
            mk_root.to_hex(),
        );
        signable_expected.set_message_part(
            ProtocolMessagePartKey::LatestImmutableFileNumber,
            "14".to_string(),
        );
        assert_eq!(signable_expected, signable);
    }

    #[tokio::test]
    async fn test_compute_signable_with_no_transaction_return_error() {
        let beacon = Beacon::default();
        let transactions = vec![];
        let transaction_parser = Arc::new(DumbTransactionParser::new(transactions.clone()));
        let mut mock_transaction_store = MockTransactionStore::new();
        mock_transaction_store
            .expect_store_transactions()
            .returning(|_| Ok(()));
        let transaction_store = Arc::new(mock_transaction_store);
        let cardano_transactions_signable_builder = CardanoTransactionsSignableBuilder::new(
            transaction_parser,
            transaction_store,
            Path::new("/tmp"),
            create_logger(),
        );

        let result = cardano_transactions_signable_builder
            .compute_protocol_message(beacon.clone())
            .await;

        assert!(result.is_err());
    }
}
