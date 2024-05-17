use std::ops::Range;
use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;
use sqlite::Value;

use mithril_common::crypto_helper::MKTreeNode;
use mithril_common::entities::{
    BlockHash, BlockNumber, BlockRange, CardanoTransaction, ImmutableFileNumber, SlotNumber,
    TransactionHash,
};
use mithril_common::signable_builder::BlockRangeRootRetriever;
use mithril_common::StdResult;

use crate::database::provider::{
    DeleteCardanoTransactionProvider, GetBlockRangeRootProvider, GetCardanoTransactionProvider,
    GetIntervalWithoutBlockRangeRootProvider, InsertBlockRangeRootProvider,
    InsertCardanoTransactionProvider,
};
use crate::database::record::{BlockRangeRootRecord, CardanoTransactionRecord};
use crate::sqlite::{
    ConnectionExtensions, GetAllProvider, Provider, SqliteConnection, WhereCondition,
};

/// ## Cardano transaction repository
///
/// This is a business oriented layer to perform actions on the database through
/// providers.
pub struct CardanoTransactionRepository {
    connection: Arc<SqliteConnection>,
}

impl CardanoTransactionRepository {
    /// Instantiate service
    pub fn new(connection: Arc<SqliteConnection>) -> Self {
        Self { connection }
    }

    /// Return all the [CardanoTransactionRecord]s in the database.
    pub async fn get_all_transactions(&self) -> StdResult<Vec<CardanoTransactionRecord>> {
        let provider = GetCardanoTransactionProvider::new(&self.connection);
        let filters = WhereCondition::default();
        let transactions = provider.find(filters)?;

        Ok(transactions.collect())
    }

    /// Return all the [CardanoTransactionRecord]s in the database where block number is in the
    /// given range.
    pub async fn get_transactions_in_range_blocks(
        &self,
        range: Range<BlockNumber>,
    ) -> StdResult<Vec<CardanoTransactionRecord>> {
        let provider = GetCardanoTransactionProvider::new(&self.connection);
        let filters = provider.get_transaction_between_blocks_condition(range);
        let transactions = provider.find(filters)?;

        Ok(transactions.collect())
    }

    /// Return all the [CardanoTransactionRecord]s in the database up to the given beacon.
    ///
    /// Note: until we rely on block number based beacons, this function needs to compute the highest block number for the given immutable file number.
    pub async fn get_transactions_up_to(
        &self,
        beacon: ImmutableFileNumber,
    ) -> StdResult<Vec<CardanoTransactionRecord>> {
        // Get the highest block number for the given immutable number.
        // This is a temporary fix that will be removed when the retrieval is based on block number instead of immutable number.
        let block_number = self
            .get_highest_block_number_for_immutable_number(beacon)
            .await?
            .unwrap_or(0);
        let provider = GetCardanoTransactionProvider::new(&self.connection);
        let filters = provider.get_transaction_between_blocks_condition(0..block_number + 1);
        let transactions = provider.find(filters)?;

        Ok(transactions.collect())
    }

    /// Return the [CardanoTransactionRecord] for the given transaction hash.
    pub async fn get_transaction<T: Into<TransactionHash>>(
        &self,
        transaction_hash: T,
    ) -> StdResult<Option<CardanoTransactionRecord>> {
        let provider = GetCardanoTransactionProvider::new(&self.connection);
        let filters = provider.get_transaction_hash_condition(&transaction_hash.into());
        let mut transactions = provider.find(filters)?;

        Ok(transactions.next())
    }

    /// Create a new [CardanoTransactionRecord] in the database.
    pub async fn create_transaction<T: Into<TransactionHash>, U: Into<BlockHash>>(
        &self,
        transaction_hash: T,
        block_number: BlockNumber,
        slot_number: SlotNumber,
        block_hash: U,
        immutable_file_number: ImmutableFileNumber,
    ) -> StdResult<Option<CardanoTransactionRecord>> {
        let provider = InsertCardanoTransactionProvider::new(&self.connection);
        let filters = provider.get_insert_condition(&CardanoTransactionRecord {
            transaction_hash: transaction_hash.into(),
            block_number,
            slot_number,
            block_hash: block_hash.into(),
            immutable_file_number,
        })?;
        let mut cursor = provider.find(filters)?;

        Ok(cursor.next())
    }

    /// Create new [CardanoTransactionRecord]s in the database.
    pub async fn create_transactions<T: Into<CardanoTransactionRecord>>(
        &self,
        transactions: Vec<T>,
    ) -> StdResult<Vec<CardanoTransactionRecord>> {
        let records: Vec<CardanoTransactionRecord> =
            transactions.into_iter().map(|tx| tx.into()).collect();

        let provider = InsertCardanoTransactionProvider::new(&self.connection);
        let filters = provider.get_insert_many_condition(records)?;
        let cursor = provider.find(filters)?;

        Ok(cursor.collect())
    }

    /// Create new [BlockRangeRootRecord]s in the database.
    pub async fn create_block_range_roots<T: Into<BlockRangeRootRecord>>(
        &self,
        block_ranges: Vec<T>,
    ) -> StdResult<Vec<BlockRangeRootRecord>> {
        let records: Vec<BlockRangeRootRecord> =
            block_ranges.into_iter().map(|tx| tx.into()).collect();

        let provider = InsertBlockRangeRootProvider::new(&self.connection);
        let filters = provider.get_insert_many_condition(records)?;
        let cursor = provider.find(filters)?;

        Ok(cursor.collect())
    }

    // TODO: remove this function when the Cardano transaction signature is based on block number instead of immutable number
    /// Get the highest [BlockNumber] of the cardano transactions stored in the database.
    pub async fn get_highest_block_number_for_immutable_number(
        &self,
        immutable_file_number: ImmutableFileNumber,
    ) -> StdResult<Option<BlockNumber>> {
        let highest: Option<i64> = self.connection.query_single_cell(
            "select max(block_number) as highest from cardano_tx where immutable_file_number <= ?;",
            &[Value::Integer(immutable_file_number as i64)],
        )?;
        highest
            .map(u64::try_from)
            .transpose()
            .with_context(||
                format!("Integer field max(block_number) (value={highest:?}) is incompatible with u64 representation.")
            )
    }

    /// Get the highest start [BlockNumber] of the block range roots stored in the database.
    pub async fn get_highest_start_block_number_for_block_range_roots(
        &self,
    ) -> StdResult<Option<BlockNumber>> {
        let highest: Option<i64> = self
            .connection
            .query_single_cell("select max(start) as highest from block_range_root;", &[])?;
        highest
            .map(u64::try_from)
            .transpose()
            .with_context(||
                format!("Integer field max(start) (value={highest:?}) is incompatible with u64 representation.")
            )
    }

    /// Retrieve all the Block Range Roots in database up to the given end block number excluded.
    pub async fn retrieve_block_range_roots_up_to(
        &self,
        end_block_number: BlockNumber,
    ) -> StdResult<Box<dyn Iterator<Item = (BlockRange, MKTreeNode)>>> {
        let provider = GetBlockRangeRootProvider::new(&self.connection);
        let filters = provider.get_up_to_block_number_condition(end_block_number);
        let block_range_roots = provider.find(filters)?;
        let iterator = block_range_roots
            .into_iter()
            .map(|record| -> (BlockRange, MKTreeNode) { record.into() })
            .collect::<Vec<_>>() // TODO: remove this collect when we should ba able return the iterator directly
            .into_iter();

        Ok(Box::new(iterator))
    }

    /// Retrieve all the [CardanoTransaction] in database.
    pub async fn get_all(&self) -> StdResult<Vec<CardanoTransaction>> {
        let provider = GetCardanoTransactionProvider::new(&self.connection);
        let records = provider.get_all()?;

        Ok(records.map(|record| record.into()).collect())
    }

    /// Retrieve all the [BlockRangeRootRecord] in database.
    pub fn get_all_block_range_root(&self) -> StdResult<Vec<BlockRangeRootRecord>> {
        let provider = GetBlockRangeRootProvider::new(&self.connection);
        let records = provider.get_all()?;

        Ok(records.collect())
    }

    /// Get the highest [ImmutableFileNumber] of the cardano transactions stored in the database.
    pub async fn get_transaction_highest_immutable_file_number(
        &self,
    ) -> StdResult<Option<ImmutableFileNumber>> {
        let highest: Option<i64> = self.connection.query_single_cell(
            "select max(immutable_file_number) as highest from cardano_tx;",
            &[],
        )?;
        highest
            .map(u64::try_from)
            .transpose()
            .with_context(||
                format!("Integer field max(immutable_file_number) (value={highest:?}) is incompatible with u64 representation.")
            )
    }

    /// Store the given transactions in the database.
    ///
    /// The storage is done in chunks to avoid exceeding sqlite binding limitations.
    pub async fn store_transactions<T: Into<CardanoTransactionRecord> + Clone>(
        &self,
        transactions: Vec<T>,
    ) -> StdResult<()> {
        const DB_TRANSACTION_SIZE: usize = 100000;
        for transactions_in_db_transaction_chunk in transactions.chunks(DB_TRANSACTION_SIZE) {
            self.connection.execute("BEGIN TRANSACTION;")?;

            // Chunk transactions to avoid an error when we exceed sqlite binding limitations
            for transactions_in_chunk in transactions_in_db_transaction_chunk.chunks(100) {
                self.create_transactions(transactions_in_chunk.to_vec())
                    .await
                    .with_context(|| "CardanoTransactionRepository can not store transactions")?;
            }

            self.connection.execute("END TRANSACTION;")?;
        }
        Ok(())
    }

    /// Get the block interval without block range root if any.
    pub async fn get_block_interval_without_block_range_root(
        &self,
    ) -> StdResult<Option<Range<BlockNumber>>> {
        let provider = GetIntervalWithoutBlockRangeRootProvider::new(&self.connection);
        let row = provider
            .find(provider.get_interval_without_block_range_condition())?
            .next();

        match row {
            // Should be impossible - the request as written in the provider always returns a single row
            None => panic!("IntervalWithoutBlockRangeProvider should always return a single row"),
            Some(interval) => interval.to_range(),
        }
    }

    /// Get the [CardanoTransactionRecord] for the given transaction hashes.
    pub async fn get_transaction_by_hashes<T: Into<TransactionHash>>(
        &self,
        hashes: Vec<T>,
    ) -> StdResult<Vec<CardanoTransactionRecord>> {
        let provider = GetCardanoTransactionProvider::new(&self.connection);
        let filters =
            provider.get_transaction_hashes_condition(hashes.into_iter().map(Into::into).collect());
        let transactions = provider.find(filters)?;

        Ok(transactions.collect())
    }

    /// Get the [CardanoTransactionRecord] for the given block ranges.
    pub async fn get_transaction_by_block_ranges(
        &self,
        block_ranges: Vec<BlockRange>,
    ) -> StdResult<Vec<CardanoTransactionRecord>> {
        let provider = GetCardanoTransactionProvider::new(&self.connection);
        let filters = provider.get_transaction_block_ranges_condition(block_ranges);
        let transactions = provider.find(filters)?;

        Ok(transactions.collect())
    }

    /// Prune the transactions older than the given number of blocks (based on the block range root
    /// stored).
    pub async fn prune_transaction(&self, number_of_blocks_to_keep: BlockNumber) -> StdResult<()> {
        if let Some(highest_block_range_start) = self
            .get_highest_start_block_number_for_block_range_roots()
            .await?
        {
            let provider = DeleteCardanoTransactionProvider::new(&self.connection);
            let threshold = highest_block_range_start.saturating_sub(number_of_blocks_to_keep);
            provider.prune(threshold)?.next();
        }

        Ok(())
    }
}

#[async_trait]
impl BlockRangeRootRetriever for CardanoTransactionRepository {
    async fn retrieve_block_range_roots(
        &self,
        up_to_beacon: ImmutableFileNumber,
    ) -> StdResult<Box<dyn Iterator<Item = (BlockRange, MKTreeNode)>>> {
        // Get the highest block number for the given immutable number.
        // This is a temporary fix that will be removed when the retrieval is based on block number instead of immutable number.
        let block_number = self
            .get_highest_block_number_for_immutable_number(up_to_beacon)
            .await?
            .unwrap_or(0);

        self.retrieve_block_range_roots_up_to(block_number).await
    }
}

#[cfg(test)]
mod tests {
    use crate::database::provider::GetBlockRangeRootProvider;
    use crate::database::test_helper::cardano_tx_db_connection;
    use crate::sqlite::GetAllProvider;

    use super::*;

    #[tokio::test]
    async fn repository_create_and_get_transaction() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);
        repository
            .create_transactions(vec![
                CardanoTransaction::new("tx_hash-123", 10, 50, "block_hash-123", 99),
                CardanoTransaction::new("tx_hash-456", 11, 51, "block_hash-456", 100),
            ])
            .await
            .unwrap();

        {
            let transaction_result = repository.get_transaction("tx_hash-123").await.unwrap();
            assert_eq!(
                Some(CardanoTransactionRecord {
                    transaction_hash: "tx_hash-123".to_string(),
                    block_number: 10,
                    slot_number: 50,
                    block_hash: "block_hash-123".to_string(),
                    immutable_file_number: 99
                }),
                transaction_result
            );
        }
        {
            let transaction_result = repository.get_transaction("not-exist").await.unwrap();
            assert_eq!(None, transaction_result);
        }
    }

    #[tokio::test]
    async fn repository_get_transaction_by_hashes() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);
        repository
            .create_transactions(vec![
                CardanoTransactionRecord::new("tx_hash-123", 10, 50, "block_hash-123", 99),
                CardanoTransactionRecord::new("tx_hash-456", 11, 51, "block_hash-456", 100),
                CardanoTransactionRecord::new("tx_hash-789", 12, 52, "block_hash-789", 101),
            ])
            .await
            .unwrap();

        {
            let transactions = repository
                .get_transaction_by_hashes(vec!["tx_hash-123", "tx_hash-789"])
                .await
                .unwrap();

            assert_eq!(
                vec![
                    CardanoTransactionRecord::new("tx_hash-123", 10, 50, "block_hash-123", 99),
                    CardanoTransactionRecord::new("tx_hash-789", 12, 52, "block_hash-789", 101),
                ],
                transactions
            );
        }
        {
            let transactions = repository
                .get_transaction_by_hashes(vec!["not-exist".to_string()])
                .await
                .unwrap();

            assert_eq!(Vec::<CardanoTransactionRecord>::new(), transactions);
        }
    }

    #[tokio::test]
    async fn repository_create_ignore_further_transactions_when_exists() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);
        repository
            .create_transaction("tx-hash-123", 10, 50, "block_hash-123", 99)
            .await
            .unwrap();
        repository
            .create_transaction("tx-hash-123", 11, 51, "block_hash-123-bis", 100)
            .await
            .unwrap();
        let transaction_result = repository.get_transaction("tx-hash-123").await.unwrap();

        assert_eq!(
            Some(CardanoTransactionRecord {
                transaction_hash: "tx-hash-123".to_string(),
                block_number: 10,
                slot_number: 50,
                block_hash: "block_hash-123".to_string(),
                immutable_file_number: 99
            }),
            transaction_result
        );
    }

    #[tokio::test]
    async fn repository_store_transactions_and_get_stored_transactions() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);

        let cardano_transactions = vec![
            CardanoTransaction::new("tx-hash-123", 10, 50, "block-hash-123", 99),
            CardanoTransaction::new("tx-hash-456", 11, 51, "block-hash-456", 100),
        ];
        repository
            .create_transactions(cardano_transactions)
            .await
            .unwrap();

        let transaction_result = repository.get_transaction("tx-hash-123").await.unwrap();

        assert_eq!(
            Some(CardanoTransactionRecord {
                transaction_hash: "tx-hash-123".to_string(),
                block_number: 10,
                slot_number: 50,
                block_hash: "block-hash-123".to_string(),
                immutable_file_number: 99
            }),
            transaction_result
        );

        let transaction_result = repository.get_transaction("tx-hash-456").await.unwrap();

        assert_eq!(
            Some(CardanoTransactionRecord {
                transaction_hash: "tx-hash-456".to_string(),
                block_number: 11,
                slot_number: 51,
                block_hash: "block-hash-456".to_string(),
                immutable_file_number: 100,
            }),
            transaction_result
        );
    }

    #[tokio::test]
    async fn repository_get_up_to_beacon_transactions() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);

        // Build transactions with block numbers from 20 to 40 and immutable file numbers from 12 to 14
        let cardano_transactions: Vec<CardanoTransactionRecord> = (20..=40)
            .map(|i| CardanoTransactionRecord {
                transaction_hash: format!("tx-hash-{i}"),
                block_number: i,
                slot_number: i * 100,
                block_hash: format!("block-hash-{i}"),
                immutable_file_number: i / 10 + 10,
            })
            .collect();

        repository
            .create_transactions(cardano_transactions.clone())
            .await
            .unwrap();

        let transaction_result = repository.get_transactions_up_to(12).await.unwrap();
        let transaction_up_to_immutable_file_number_12 = cardano_transactions[0..10].to_vec();
        assert_eq!(
            transaction_up_to_immutable_file_number_12,
            transaction_result
        );

        let transaction_result = repository.get_transactions_up_to(300).await.unwrap();
        let transaction_all = cardano_transactions[..].to_vec();
        assert_eq!(transaction_all, transaction_result);

        let transaction_result = repository.get_transactions_up_to(9).await.unwrap();
        assert_eq!(Vec::<CardanoTransactionRecord>::new(), transaction_result);
    }

    #[tokio::test]
    async fn repository_get_all_stored_transactions() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);

        let cardano_transactions = vec![
            CardanoTransaction::new("tx-hash-123".to_string(), 10, 50, "block-hash-123", 99),
            CardanoTransaction::new("tx-hash-456".to_string(), 11, 51, "block-hash-456", 100),
        ];
        repository
            .create_transactions(cardano_transactions.clone())
            .await
            .unwrap();

        let transactions_result = repository.get_all_transactions().await.unwrap();
        let transactions_expected: Vec<CardanoTransactionRecord> = cardano_transactions
            .iter()
            .map(|tx| tx.clone().into())
            .collect();

        assert_eq!(transactions_expected, transactions_result);
    }

    #[tokio::test]
    async fn repository_store_transactions_with_existing_hash_doesnt_erase_existing_data() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);

        repository
            .create_transaction("tx-hash-000", 1, 5, "block-hash", 9)
            .await
            .unwrap();

        let cardano_transactions = vec![CardanoTransaction::new(
            "tx-hash-123",
            10,
            50,
            "block-hash-123",
            99,
        )];
        repository
            .create_transactions(cardano_transactions)
            .await
            .unwrap();

        let transaction_result = repository.get_transaction("tx-hash-000").await.unwrap();

        assert_eq!(
            Some(CardanoTransactionRecord {
                transaction_hash: "tx-hash-000".to_string(),
                block_number: 1,
                slot_number: 5,
                block_hash: "block-hash".to_string(),
                immutable_file_number: 9
            }),
            transaction_result
        );
    }

    #[tokio::test]
    async fn repository_get_transaction_highest_immutable_file_number_without_transactions_in_db() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);

        let highest_beacon = repository
            .get_transaction_highest_immutable_file_number()
            .await
            .unwrap();
        assert_eq!(None, highest_beacon);
    }

    #[tokio::test]
    async fn repository_get_transaction_highest_immutable_file_number_with_transactions_in_db() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);

        let cardano_transactions = vec![
            CardanoTransaction::new("tx-hash-123".to_string(), 10, 50, "block-hash-123", 50),
            CardanoTransaction::new("tx-hash-456".to_string(), 11, 51, "block-hash-456", 100),
        ];
        repository
            .create_transactions(cardano_transactions)
            .await
            .unwrap();

        let highest_beacon = repository
            .get_transaction_highest_immutable_file_number()
            .await
            .unwrap();
        assert_eq!(Some(100), highest_beacon);
    }

    #[tokio::test]
    async fn repository_get_transactions_in_range_blocks() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);

        let transactions = vec![
            CardanoTransactionRecord::new("tx-hash-1", 10, 50, "block-hash-1", 99),
            CardanoTransactionRecord::new("tx-hash-2", 11, 51, "block-hash-2", 100),
            CardanoTransactionRecord::new("tx-hash-3", 12, 52, "block-hash-3", 101),
        ];
        repository
            .create_transactions(transactions.clone())
            .await
            .unwrap();

        {
            let transaction_result = repository
                .get_transactions_in_range_blocks(0..10)
                .await
                .unwrap();
            assert_eq!(Vec::<CardanoTransactionRecord>::new(), transaction_result);
        }
        {
            let transaction_result = repository
                .get_transactions_in_range_blocks(13..21)
                .await
                .unwrap();
            assert_eq!(Vec::<CardanoTransactionRecord>::new(), transaction_result);
        }
        {
            let transaction_result = repository
                .get_transactions_in_range_blocks(9..12)
                .await
                .unwrap();
            assert_eq!(transactions[0..=1].to_vec(), transaction_result);
        }
        {
            let transaction_result = repository
                .get_transactions_in_range_blocks(10..13)
                .await
                .unwrap();
            assert_eq!(transactions.clone(), transaction_result);
        }
        {
            let transaction_result = repository
                .get_transactions_in_range_blocks(11..14)
                .await
                .unwrap();
            assert_eq!(transactions[1..=2].to_vec(), transaction_result);
        }
    }

    #[tokio::test]
    async fn repository_get_block_interval_without_block_range_root() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);

        // The last block range give the lower bound
        let last_block_range = BlockRange::from_block_number(0);
        repository
            .create_block_range_roots(vec![(
                last_block_range.clone(),
                MKTreeNode::from_hex("AAAA").unwrap(),
            )])
            .await
            .unwrap();

        // The last transaction block number give the upper bound
        let last_transaction_block_number = BlockRange::LENGTH * 4;
        repository
            .create_transaction("tx-1", last_transaction_block_number, 50, "block-1", 99)
            .await
            .unwrap();

        let interval = repository
            .get_block_interval_without_block_range_root()
            .await
            .unwrap();

        assert_eq!(
            Some(last_block_range.end..(last_transaction_block_number + 1)),
            interval
        );
    }

    #[tokio::test]
    async fn repository_get_transactions_by_block_ranges() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);

        let transactions = vec![
            CardanoTransactionRecord::new("tx-hash-1", 10, 50, "block-hash-1", 99),
            CardanoTransactionRecord::new("tx-hash-2", 11, 51, "block-hash-2", 100),
            CardanoTransactionRecord::new("tx-hash-3", 20, 52, "block-hash-3", 101),
            CardanoTransactionRecord::new("tx-hash-4", 31, 53, "block-hash-4", 102),
            CardanoTransactionRecord::new("tx-hash-5", 35, 54, "block-hash-5", 103),
            CardanoTransactionRecord::new("tx-hash-6", 46, 55, "block-hash-6", 104),
        ];
        repository
            .create_transactions(transactions.clone())
            .await
            .unwrap();

        {
            let transaction_result = repository
                .get_transaction_by_block_ranges(vec![BlockRange::from_block_number(100)])
                .await
                .unwrap();
            assert_eq!(Vec::<CardanoTransactionRecord>::new(), transaction_result);
        }
        {
            let transaction_result = repository
                .get_transaction_by_block_ranges(vec![BlockRange::from_block_number(0)])
                .await
                .unwrap();
            assert_eq!(transactions[0..=1].to_vec(), transaction_result);
        }
        {
            let transaction_result = repository
                .get_transaction_by_block_ranges(vec![
                    BlockRange::from_block_number(0),
                    BlockRange::from_block_number(15),
                ])
                .await
                .unwrap();
            assert_eq!(transactions[0..=2].to_vec(), transaction_result);
        }
        {
            let transaction_result = repository
                .get_transaction_by_block_ranges(vec![
                    BlockRange::from_block_number(0),
                    BlockRange::from_block_number(30),
                ])
                .await
                .unwrap();
            assert_eq!(
                [transactions[0..=1].to_vec(), transactions[3..=4].to_vec()].concat(),
                transaction_result
            );
        }
    }

    #[tokio::test]
    async fn repository_store_block_range() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let provider = GetBlockRangeRootProvider::new(&connection);
        let repository = CardanoTransactionRepository::new(connection.clone());

        repository
            .create_block_range_roots(vec![
                (
                    BlockRange::from_block_number(0),
                    MKTreeNode::from_hex("AAAA").unwrap(),
                ),
                (
                    BlockRange::from_block_number(BlockRange::LENGTH),
                    MKTreeNode::from_hex("BBBB").unwrap(),
                ),
            ])
            .await
            .unwrap();

        let records: Vec<BlockRangeRootRecord> = provider.get_all().unwrap().collect();
        assert_eq!(
            vec![
                BlockRangeRootRecord {
                    range: BlockRange::from_block_number(0),
                    merkle_root: MKTreeNode::from_hex("AAAA").unwrap(),
                },
                BlockRangeRootRecord {
                    range: BlockRange::from_block_number(BlockRange::LENGTH),
                    merkle_root: MKTreeNode::from_hex("BBBB").unwrap(),
                }
            ],
            records
        );
    }

    #[tokio::test]
    async fn repository_store_block_range_with_existing_hash_doesnt_erase_existing_data() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let provider = GetBlockRangeRootProvider::new(&connection);
        let repository = CardanoTransactionRepository::new(connection.clone());
        let range = BlockRange::from_block_number(0);

        repository
            .create_block_range_roots(vec![(range.clone(), MKTreeNode::from_hex("AAAA").unwrap())])
            .await
            .unwrap();
        repository
            .create_block_range_roots(vec![(range.clone(), MKTreeNode::from_hex("BBBB").unwrap())])
            .await
            .unwrap();

        let record: Vec<BlockRangeRootRecord> = provider.get_all().unwrap().collect();
        assert_eq!(
            vec![BlockRangeRootRecord {
                range,
                merkle_root: MKTreeNode::from_hex("AAAA").unwrap()
            }],
            record
        );
    }

    #[tokio::test]
    async fn repository_retrieve_block_range_roots_up_to() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);
        let block_range_roots = vec![
            (
                BlockRange::from_block_number(15),
                MKTreeNode::from_hex("AAAA").unwrap(),
            ),
            (
                BlockRange::from_block_number(30),
                MKTreeNode::from_hex("BBBB").unwrap(),
            ),
            (
                BlockRange::from_block_number(45),
                MKTreeNode::from_hex("CCCC").unwrap(),
            ),
        ];
        repository
            .create_block_range_roots(block_range_roots.clone())
            .await
            .unwrap();

        // Retrieve with a block far higher than the highest block range - should return all
        {
            let retrieved_block_ranges = repository
                .retrieve_block_range_roots_up_to(1000)
                .await
                .unwrap();
            assert_eq!(
                block_range_roots,
                retrieved_block_ranges.collect::<Vec<_>>()
            );
        }
        // Retrieve with a block bellow than the smallest block range - should return none
        {
            let retrieved_block_ranges = repository
                .retrieve_block_range_roots_up_to(2)
                .await
                .unwrap();
            assert_eq!(
                Vec::<(BlockRange, MKTreeNode)>::new(),
                retrieved_block_ranges.collect::<Vec<_>>()
            );
        }
        // The given block is matched to the end (excluded) - should return the first of the three
        {
            let retrieved_block_ranges = repository
                .retrieve_block_range_roots_up_to(45)
                .await
                .unwrap();
            assert_eq!(
                vec![block_range_roots[0].clone()],
                retrieved_block_ranges.collect::<Vec<_>>()
            );
        }
        // Right after the end of the second block range - should return first two of the three
        {
            let retrieved_block_ranges = repository
                .retrieve_block_range_roots_up_to(46)
                .await
                .unwrap();
            assert_eq!(
                block_range_roots[0..=1].to_vec(),
                retrieved_block_ranges.collect::<Vec<_>>()
            );
        }
    }

    #[tokio::test]
    async fn repository_prune_transactions() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);

        // Build transactions with block numbers from 20 to 50
        let cardano_transactions: Vec<CardanoTransactionRecord> = (20..=50)
            .map(|i| CardanoTransactionRecord {
                transaction_hash: format!("tx-hash-{i}"),
                block_number: i,
                slot_number: i * 100,
                block_hash: format!("block-hash-{i}"),
                immutable_file_number: 1,
            })
            .collect();

        repository
            .create_transactions(cardano_transactions.clone())
            .await
            .unwrap();
        repository
            .create_block_range_roots(vec![(
                BlockRange::from_block_number(45),
                MKTreeNode::from_hex("BBBB").unwrap(),
            )])
            .await
            .unwrap();

        let transaction_result = repository.get_all().await.unwrap();
        assert_eq!(cardano_transactions.len(), transaction_result.len());

        // Pruning with a number of block to keep greater than the highest block range start should
        // do nothing.
        repository.prune_transaction(10_000_000).await.unwrap();
        let transaction_result = repository.get_all_transactions().await.unwrap();
        assert_eq!(cardano_transactions, transaction_result);

        // Since the highest block range start is 45, pruning with 20 should remove transactions
        // with a block number strictly below 25.
        repository.prune_transaction(20).await.unwrap();
        let transaction_result = repository
            .get_transactions_in_range_blocks(0..25)
            .await
            .unwrap();
        assert_eq!(Vec::<CardanoTransactionRecord>::new(), transaction_result);
    }

    #[tokio::test]
    async fn get_highest_start_block_number_for_block_range_roots() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);

        let highest = repository
            .get_highest_start_block_number_for_block_range_roots()
            .await
            .unwrap();
        assert_eq!(None, highest);

        let block_range_roots = vec![
            (
                BlockRange::from_block_number(15),
                MKTreeNode::from_hex("AAAA").unwrap(),
            ),
            (
                BlockRange::from_block_number(30),
                MKTreeNode::from_hex("BBBB").unwrap(),
            ),
        ];
        repository
            .create_block_range_roots(block_range_roots.clone())
            .await
            .unwrap();

        let highest = repository
            .get_highest_start_block_number_for_block_range_roots()
            .await
            .unwrap();
        assert_eq!(Some(30), highest);
    }
}
