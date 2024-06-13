use std::ops::Range;
use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;

use mithril_common::cardano_block_scanner::ImmutableLowerBoundFinder;
use mithril_common::crypto_helper::MKTreeNode;
use mithril_common::entities::{
    BlockHash, BlockNumber, BlockRange, CardanoTransaction, ChainPoint, ImmutableFileNumber,
    SlotNumber, TransactionHash,
};
use mithril_common::signable_builder::BlockRangeRootRetriever;
use mithril_common::StdResult;

use crate::database::query::{
    DeleteBlockRangeRootQuery, DeleteCardanoTransactionQuery, GetBlockRangeRootQuery,
    GetCardanoTransactionQuery, GetIntervalWithoutBlockRangeRootQuery, InsertBlockRangeRootQuery,
    InsertCardanoTransactionQuery,
};
use crate::database::record::{BlockRangeRootRecord, CardanoTransactionRecord};
use crate::sqlite::{ConnectionExtensions, SqliteConnection};

/// ## Cardano transaction repository
///
/// This is a business oriented layer to perform actions on the database through
/// queries.
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
        self.connection
            .fetch_collect(GetCardanoTransactionQuery::all())
    }

    /// Return all the [CardanoTransactionRecord]s in the database where block number is in the
    /// given range.
    pub async fn get_transactions_in_range_blocks(
        &self,
        range: Range<BlockNumber>,
    ) -> StdResult<Vec<CardanoTransactionRecord>> {
        self.connection
            .fetch_collect(GetCardanoTransactionQuery::between_blocks(range))
    }

    /// Return the [CardanoTransactionRecord] for the given transaction hash.
    pub async fn get_transaction<T: Into<TransactionHash>>(
        &self,
        transaction_hash: T,
    ) -> StdResult<Option<CardanoTransactionRecord>> {
        self.connection
            .fetch_first(GetCardanoTransactionQuery::by_transaction_hash(
                &transaction_hash.into(),
            ))
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
        let query = InsertCardanoTransactionQuery::insert_one(&CardanoTransactionRecord {
            transaction_hash: transaction_hash.into(),
            block_number,
            slot_number,
            block_hash: block_hash.into(),
            immutable_file_number,
        })?;

        self.connection.fetch_first(query)
    }

    /// Create new [CardanoTransactionRecord]s in the database.
    pub async fn create_transactions<T: Into<CardanoTransactionRecord>>(
        &self,
        transactions: Vec<T>,
    ) -> StdResult<Vec<CardanoTransactionRecord>> {
        let records: Vec<CardanoTransactionRecord> =
            transactions.into_iter().map(|tx| tx.into()).collect();

        self.connection
            .fetch_collect(InsertCardanoTransactionQuery::insert_many(records)?)
    }

    /// Create new [BlockRangeRootRecord]s in the database.
    pub async fn create_block_range_roots<T: Into<BlockRangeRootRecord>>(
        &self,
        block_ranges: Vec<T>,
    ) -> StdResult<Vec<BlockRangeRootRecord>> {
        let records: Vec<BlockRangeRootRecord> =
            block_ranges.into_iter().map(|tx| tx.into()).collect();

        self.connection
            .fetch_collect(InsertBlockRangeRootQuery::insert_many(records)?)
    }

    /// Get the highest [ChainPoint] of the cardano transactions stored in the database.
    pub async fn get_transaction_highest_chain_point(&self) -> StdResult<Option<ChainPoint>> {
        let first_transaction_with_highest_block_number = self
            .connection
            .fetch_first(GetCardanoTransactionQuery::with_highest_block_number())?;

        Ok(first_transaction_with_highest_block_number.map(|record| {
            ChainPoint::new(record.slot_number, record.block_number, record.block_hash)
        }))
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
    ) -> StdResult<Box<dyn Iterator<Item = (BlockRange, MKTreeNode)> + '_>> {
        let block_range_roots = self
            .connection
            .fetch(GetBlockRangeRootQuery::up_to_block_number(end_block_number))?
            .map(|record| -> (BlockRange, MKTreeNode) { record.into() });

        Ok(Box::new(block_range_roots))
    }

    /// Retrieve all the [CardanoTransaction] in database.
    pub async fn get_all(&self) -> StdResult<Vec<CardanoTransaction>> {
        let records = self.connection.fetch(GetCardanoTransactionQuery::all())?;

        Ok(records.map(|record| record.into()).collect())
    }

    /// Retrieve all the [BlockRangeRootRecord] in database.
    pub fn get_all_block_range_root(&self) -> StdResult<Vec<BlockRangeRootRecord>> {
        self.connection.fetch_collect(GetBlockRangeRootQuery::all())
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
            let transaction = self.connection.begin_transaction()?;

            // Chunk transactions to avoid an error when we exceed sqlite binding limitations
            for transactions_in_chunk in transactions_in_db_transaction_chunk.chunks(100) {
                self.create_transactions(transactions_in_chunk.to_vec())
                    .await
                    .with_context(|| "CardanoTransactionRepository can not store transactions")?;
            }

            transaction.commit()?;
        }
        Ok(())
    }

    /// Get the block interval without block range root if any.
    pub async fn get_block_interval_without_block_range_root(
        &self,
    ) -> StdResult<Option<Range<BlockNumber>>> {
        let interval = self
            .connection
            .fetch_first(GetIntervalWithoutBlockRangeRootQuery::new())?
            // Should be impossible - the request as written in the query always returns a single row
            .unwrap_or_else(|| {
                panic!("GetIntervalWithoutBlockRangeRootQuery should always return a single row")
            });

        interval.to_range()
    }

    /// Get the [CardanoTransactionRecord] for the given transaction hashes, up to a block number
    pub async fn get_transaction_by_hashes<T: Into<TransactionHash>>(
        &self,
        hashes: Vec<T>,
        up_to: BlockNumber,
    ) -> StdResult<Vec<CardanoTransactionRecord>> {
        let query = GetCardanoTransactionQuery::by_transaction_hashes(
            hashes.into_iter().map(Into::into).collect(),
            up_to,
        );
        self.connection.fetch_collect(query)
    }

    /// Get the [CardanoTransactionRecord] for the given block ranges.
    pub async fn get_transaction_by_block_ranges(
        &self,
        block_ranges: Vec<BlockRange>,
    ) -> StdResult<Vec<CardanoTransactionRecord>> {
        let mut transactions = vec![];
        for block_range in block_ranges {
            let block_range_transactions: Vec<CardanoTransactionRecord> = self
                .connection
                .fetch_collect(GetCardanoTransactionQuery::by_block_ranges(vec![
                    block_range,
                ]))?;
            transactions.extend(block_range_transactions);
        }

        Ok(transactions)
    }

    /// Prune the transactions older than the given number of blocks (based on the block range root
    /// stored).
    pub async fn prune_transaction(&self, number_of_blocks_to_keep: BlockNumber) -> StdResult<()> {
        if let Some(highest_block_range_start) = self
            .get_highest_start_block_number_for_block_range_roots()
            .await?
        {
            let threshold = highest_block_range_start.saturating_sub(number_of_blocks_to_keep);
            let query = DeleteCardanoTransactionQuery::below_block_number_threshold(threshold)?;
            self.connection.fetch_first(query)?;
        }

        Ok(())
    }

    /// Remove transactions and block range roots that are in a rolled-back fork
    ///
    /// * Remove transactions with block number strictly greater than the given block number
    /// * Remove block range roots that have lower bound range strictly above the given block number
    pub async fn remove_rolled_back_transactions_and_block_range(
        &self,
        block_number: BlockNumber,
    ) -> StdResult<()> {
        let transaction = self.connection.begin_transaction()?;
        let query = DeleteCardanoTransactionQuery::above_block_number_threshold(block_number)?;
        self.connection.fetch_first(query)?;

        let query =
            DeleteBlockRangeRootQuery::contains_or_above_block_number_threshold(block_number)?;
        self.connection.fetch_first(query)?;
        transaction.commit()?;

        Ok(())
    }
}

#[async_trait]
impl BlockRangeRootRetriever for CardanoTransactionRepository {
    async fn retrieve_block_range_roots(
        &self,
        up_to_beacon: BlockNumber,
    ) -> StdResult<Box<dyn Iterator<Item = (BlockRange, MKTreeNode)>>> {
        let iterator = self
            .retrieve_block_range_roots_up_to(up_to_beacon)
            .await?
            .collect::<Vec<_>>() // TODO: remove this collect to return the iterator directly
            .into_iter();
        Ok(Box::new(iterator))
    }
}

#[async_trait]
impl ImmutableLowerBoundFinder for CardanoTransactionRepository {
    async fn find_lower_bound(&self) -> StdResult<Option<ImmutableFileNumber>> {
        self.get_transaction_highest_immutable_file_number().await
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::CardanoTransactionsBuilder;

    use crate::database::query::GetBlockRangeRootQuery;
    use crate::database::test_helper::cardano_tx_db_connection;

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
                CardanoTransactionRecord::new("tx_hash-123", 10, 50, "block_hash-123", 1234),
                CardanoTransactionRecord::new("tx_hash-456", 11, 51, "block_hash-456", 1234),
                CardanoTransactionRecord::new("tx_hash-789", 12, 52, "block_hash-789", 1234),
                CardanoTransactionRecord::new("tx_hash-000", 101, 100, "block_hash-000", 1234),
            ])
            .await
            .unwrap();

        {
            let transactions = repository
                .get_transaction_by_hashes(vec!["tx_hash-123", "tx_hash-789"], 100)
                .await
                .unwrap();

            assert_eq!(
                vec![
                    CardanoTransactionRecord::new("tx_hash-123", 10, 50, "block_hash-123", 1234),
                    CardanoTransactionRecord::new("tx_hash-789", 12, 52, "block_hash-789", 1234),
                ],
                transactions
            );
        }
        {
            let transactions = repository
                .get_transaction_by_hashes(vec!["tx_hash-123", "tx_hash-789", "tx_hash-000"], 100)
                .await
                .unwrap();

            assert_eq!(
                vec![
                    CardanoTransactionRecord::new("tx_hash-123", 10, 50, "block_hash-123", 1234),
                    CardanoTransactionRecord::new("tx_hash-789", 12, 52, "block_hash-789", 1234),
                ],
                transactions
            );
        }
        {
            let transactions = repository
                .get_transaction_by_hashes(vec!["tx_hash-123", "tx_hash-789", "tx_hash-000"], 101)
                .await
                .unwrap();

            assert_eq!(
                vec![
                    CardanoTransactionRecord::new("tx_hash-123", 10, 50, "block_hash-123", 1234),
                    CardanoTransactionRecord::new("tx_hash-789", 12, 52, "block_hash-789", 1234),
                    CardanoTransactionRecord::new("tx_hash-000", 101, 100, "block_hash-000", 1234),
                ],
                transactions
            );
        }
        {
            let transactions = repository
                .get_transaction_by_hashes(vec!["not-exist".to_string()], 100)
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
    async fn repository_store_transactions_doesnt_erase_existing_data() {
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
    async fn repository_get_transaction_highest_chain_point_without_transactions_in_db() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);

        let highest_beacon = repository
            .get_transaction_highest_chain_point()
            .await
            .unwrap();
        assert_eq!(None, highest_beacon);
    }

    #[tokio::test]
    async fn repository_get_transaction_highest_chain_point_with_transactions_in_db() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);

        let cardano_transactions = vec![
            CardanoTransaction::new("tx-hash-123", 10, 50, "block-hash-10", 50),
            CardanoTransaction::new("tx-hash-456", 25, 51, "block-hash-25", 100),
        ];
        repository
            .create_transactions(cardano_transactions)
            .await
            .unwrap();

        let highest_beacon = repository
            .get_transaction_highest_chain_point()
            .await
            .unwrap();
        assert_eq!(
            Some(ChainPoint {
                slot_number: 51,
                block_number: 25,
                block_hash: "block-hash-25".to_string()
            }),
            highest_beacon
        );
    }

    #[tokio::test]
    async fn repository_get_transaction_highest_chain_point_with_transactions_with_same_block_number_in_db(
    ) {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);

        let cardano_transactions = vec![
            CardanoTransaction::new("tx-hash-123", 10, 50, "block-hash-10", 50),
            CardanoTransaction::new("tx-hash-456", 25, 51, "block-hash-25", 100),
            CardanoTransaction::new("tx-hash-789", 25, 51, "block-hash-25", 100),
        ];
        repository
            .create_transactions(cardano_transactions)
            .await
            .unwrap();

        let highest_beacon = repository
            .get_transaction_highest_chain_point()
            .await
            .unwrap();
        assert_eq!(
            Some(ChainPoint {
                slot_number: 51,
                block_number: 25,
                block_hash: "block-hash-25".to_string()
            }),
            highest_beacon
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

        let records: Vec<BlockRangeRootRecord> = connection
            .fetch_collect(GetBlockRangeRootQuery::all())
            .unwrap();
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

        let record: Vec<BlockRangeRootRecord> = connection
            .fetch_collect(GetBlockRangeRootQuery::all())
            .unwrap();
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

        let cardano_transactions: Vec<CardanoTransactionRecord> = CardanoTransactionsBuilder::new()
            .blocks_per_block_range(15)
            .build_transactions(53)
            .into_iter()
            .map(CardanoTransactionRecord::from)
            .collect();

        repository
            .create_transactions(cardano_transactions.clone())
            .await
            .unwrap();
        // Use by 'prune_transaction' to get the block_range of the highest block number
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

        let transaction_result = repository
            .get_transactions_in_range_blocks(25..1000)
            .await
            .unwrap();
        assert_eq!(28, transaction_result.len());
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

    #[tokio::test]
    async fn find_block_scanner_lower_bound() {
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

        let highest_beacon = repository.find_lower_bound().await.unwrap();
        assert_eq!(Some(100), highest_beacon);
    }

    #[tokio::test]
    async fn remove_transactions_and_block_range_greater_than_given_block_number() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);

        let cardano_transactions = vec![
            CardanoTransaction::new("tx-hash-123", BlockRange::LENGTH, 50, "block-hash-123", 50),
            CardanoTransaction::new(
                "tx-hash-123",
                BlockRange::LENGTH * 3 - 1,
                50,
                "block-hash-123",
                50,
            ),
            CardanoTransaction::new(
                "tx-hash-456",
                BlockRange::LENGTH * 3,
                51,
                "block-hash-456",
                100,
            ),
        ];
        repository
            .create_transactions(cardano_transactions)
            .await
            .unwrap();
        repository
            .create_block_range_roots(vec![
                (
                    BlockRange::from_block_number(BlockRange::LENGTH),
                    MKTreeNode::from_hex("AAAA").unwrap(),
                ),
                (
                    BlockRange::from_block_number(BlockRange::LENGTH * 2),
                    MKTreeNode::from_hex("AAAA").unwrap(),
                ),
                (
                    BlockRange::from_block_number(BlockRange::LENGTH * 3),
                    MKTreeNode::from_hex("AAAA").unwrap(),
                ),
            ])
            .await
            .unwrap();

        repository
            .remove_rolled_back_transactions_and_block_range(BlockRange::LENGTH * 3)
            .await
            .unwrap();
        assert_eq!(2, repository.get_all_transactions().await.unwrap().len());
        assert_eq!(2, repository.get_all_block_range_root().unwrap().len());
    }
}
