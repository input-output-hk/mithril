use std::{collections::HashMap, sync::Arc};

use anyhow::Context;
use async_trait::async_trait;
use slog::{debug, Logger};

use crate::{
    crypto_helper::{MKMap, MKMapNode, MKTree, MKTreeNode},
    entities::{
        BlockRange, CardanoDbBeacon, CardanoTransaction, ProtocolMessage, ProtocolMessagePartKey,
        TransactionHash,
    },
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
    async fn import(&self, up_to_beacon: ImmutableFileNumber)
        -> StdResult<Vec<CardanoTransaction>>;
}

/// A [CardanoTransactionsSignableBuilder] builder
pub struct CardanoTransactionsSignableBuilder {
    transaction_importer: Arc<dyn TransactionsImporter>,
    logger: Logger,
}

impl CardanoTransactionsSignableBuilder {
    /// Constructor
    pub fn new(transaction_importer: Arc<dyn TransactionsImporter>, logger: Logger) -> Self {
        Self {
            transaction_importer,
            logger,
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
impl SignableBuilder<CardanoDbBeacon> for CardanoTransactionsSignableBuilder {
    async fn compute_protocol_message(
        &self,
        beacon: CardanoDbBeacon,
    ) -> StdResult<ProtocolMessage> {
        debug!(
            self.logger,
            "Compute protocol message for CardanoTransactions at beacon: {beacon}"
        );

        let transactions = self
            .transaction_importer
            .import(beacon.immutable_file_number)
            .await?;
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
    use super::*;
    use slog::Drain;

    fn create_logger() -> slog::Logger {
        let decorator = slog_term::PlainDecorator::new(slog_term::TestStdoutWriter);
        let drain = slog_term::CompactFormat::new(decorator).build().fuse();
        let drain = slog_async::Async::new(drain).build().fuse();
        slog::Logger::root(Arc::new(drain), slog::o!())
    }

    #[tokio::test]
    async fn test_compute_merkle_root_in_same_block_range() {
        let transaction_1 = CardanoTransaction::new("tx-hash-123", 1, 10, "block_hash", 1);
        let transaction_2 = CardanoTransaction::new("tx-hash-456", 2, 20, "block_hash", 1);
        let transaction_3 = CardanoTransaction::new("tx-hash-789", 3, 30, "block_hash", 1);
        let transaction_4 = CardanoTransaction::new("tx-hash-abc", 4, 40, "block_hash", 1);

        for tx in [
            &transaction_1,
            &transaction_2,
            &transaction_3,
            &transaction_4,
        ] {
            assert!(
                tx.block_number < BlockRange::LENGTH,
                "all manipulated transactions should be in the same block range"
            );
        }

        let cardano_transaction_signable_builder = CardanoTransactionsSignableBuilder::new(
            Arc::new(MockTransactionsImporter::new()),
            create_logger(),
        );

        let merkle_root_reference = cardano_transaction_signable_builder
            .compute_merkle_root(&[
                transaction_1.clone(),
                transaction_2.clone(),
                transaction_3.clone(),
            ])
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
            // In a same block range Transactions in a different order return a different merkle root.
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
    async fn test_compute_merkle_root_order_of_block_range_does_not_matter() {
        let transaction_1 =
            CardanoTransaction::new("tx-hash-123", BlockRange::LENGTH - 1, 10, "block_hash", 1);
        let transaction_2 =
            CardanoTransaction::new("tx-hash-456", BlockRange::LENGTH + 1, 20, "block_hash", 1);

        let cardano_transaction_signable_builder = CardanoTransactionsSignableBuilder::new(
            Arc::new(MockTransactionsImporter::new()),
            create_logger(),
        );

        let merkle_root_reference = cardano_transaction_signable_builder
            .compute_merkle_root(&[transaction_1.clone(), transaction_2.clone()])
            .unwrap();

        let mk_root = cardano_transaction_signable_builder
            .compute_merkle_root(&[transaction_2.clone(), transaction_1.clone()])
            .unwrap();

        assert_eq!(merkle_root_reference, mk_root);
    }

    #[tokio::test]
    async fn test_compute_signable() {
        // Arrange
        let beacon = CardanoDbBeacon {
            immutable_file_number: 14,
            ..CardanoDbBeacon::default()
        };
        let transactions = vec![
            CardanoTransaction::new("tx-hash-123", 10, 1, "block_hash-", 11),
            CardanoTransaction::new("tx-hash-456", 20, 2, "block_hash", 12),
            CardanoTransaction::new("tx-hash-789", 30, 3, "block_hash", 13),
        ];
        let imported_transactions = transactions.clone();
        let mut transaction_importer = MockTransactionsImporter::new();
        transaction_importer
            .expect_import()
            .return_once(move |_| Ok(imported_transactions));
        let cardano_transactions_signable_builder = CardanoTransactionsSignableBuilder::new(
            Arc::new(transaction_importer),
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
        let beacon = CardanoDbBeacon::default();
        let mut transaction_importer = MockTransactionsImporter::new();
        transaction_importer
            .expect_import()
            .return_once(|_| Ok(vec![]));
        let cardano_transactions_signable_builder = CardanoTransactionsSignableBuilder::new(
            Arc::new(transaction_importer),
            create_logger(),
        );

        let result = cardano_transactions_signable_builder
            .compute_protocol_message(beacon.clone())
            .await;

        assert!(result.is_err());
    }
}
