use std::ops::Range;
use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;

use mithril_common::crypto_helper::{MKTreeNode, MKTreeStore};
use mithril_common::entities::{
    BlockHash, BlockNumber, BlockRange, CardanoTransaction, ChainPoint, SlotNumber, TransactionHash,
};
use mithril_common::signable_builder::BlockRangeRootRetriever;
use mithril_common::StdResult;

use crate::database::query::{
    DeleteBlockRangeRootQuery, DeleteCardanoTransactionQuery, GetBlockRangeRootQuery,
    GetCardanoTransactionQuery, InsertBlockRangeRootQuery, InsertCardanoTransactionQuery,
};
use crate::database::record::{BlockRangeRootRecord, CardanoTransactionRecord};
use crate::sqlite::{ConnectionExtensions, SqliteConnection, SqliteConnectionPool};

/// ## Cardano transaction repository
///
/// This is a business oriented layer to perform actions on the database through
/// queries.
pub struct CardanoTransactionRepository {
    connection_pool: Arc<SqliteConnectionPool>,
}

impl CardanoTransactionRepository {
    /// Instantiate service
    pub fn new(connection_pool: Arc<SqliteConnectionPool>) -> Self {
        Self { connection_pool }
    }

    /// Return all the [CardanoTransactionRecord]s in the database.
    pub async fn get_all_transactions(&self) -> StdResult<Vec<CardanoTransactionRecord>> {
        self.connection_pool
            .connection()?
            .fetch_collect(GetCardanoTransactionQuery::all())
    }

    /// Return all the [CardanoTransactionRecord]s in the database where block number is in the
    /// given range.
    pub async fn get_transactions_in_range_blocks(
        &self,
        range: Range<BlockNumber>,
    ) -> StdResult<Vec<CardanoTransactionRecord>> {
        self.connection_pool
            .connection()?
            .fetch_collect(GetCardanoTransactionQuery::between_blocks(range))
    }

    /// Return the [CardanoTransactionRecord] for the given transaction hash.
    pub async fn get_transaction<T: Into<TransactionHash>>(
        &self,
        transaction_hash: T,
    ) -> StdResult<Option<CardanoTransactionRecord>> {
        self.connection_pool.connection()?.fetch_first(
            GetCardanoTransactionQuery::by_transaction_hash(&transaction_hash.into()),
        )
    }

    /// Create a new [CardanoTransactionRecord] in the database.
    pub async fn create_transaction<T: Into<TransactionHash>, U: Into<BlockHash>>(
        &self,
        transaction_hash: T,
        block_number: BlockNumber,
        slot_number: SlotNumber,
        block_hash: U,
    ) -> StdResult<Option<CardanoTransactionRecord>> {
        let query = InsertCardanoTransactionQuery::insert_one(&CardanoTransactionRecord {
            transaction_hash: transaction_hash.into(),
            block_number,
            slot_number,
            block_hash: block_hash.into(),
        })?;

        self.connection_pool.connection()?.fetch_first(query)
    }

    /// Create new [CardanoTransactionRecord]s in the database.
    pub async fn create_transactions<T: Into<CardanoTransactionRecord>>(
        &self,
        transactions: Vec<T>,
    ) -> StdResult<Vec<CardanoTransactionRecord>> {
        let connection = self.connection_pool.connection()?;

        self.create_transactions_with_connection(transactions, &connection)
            .await
    }

    /// Create new [CardanoTransactionRecord]s in the database.
    async fn create_transactions_with_connection<T: Into<CardanoTransactionRecord>>(
        &self,
        transactions: Vec<T>,
        connection: &SqliteConnection,
    ) -> StdResult<Vec<CardanoTransactionRecord>> {
        let records: Vec<CardanoTransactionRecord> =
            transactions.into_iter().map(|tx| tx.into()).collect();

        connection.fetch_collect(InsertCardanoTransactionQuery::insert_many(records)?)
    }

    /// Create new [BlockRangeRootRecord]s in the database.
    pub async fn create_block_range_roots<T: Into<BlockRangeRootRecord>>(
        &self,
        block_ranges: Vec<T>,
    ) -> StdResult<Vec<BlockRangeRootRecord>> {
        let records: Vec<BlockRangeRootRecord> =
            block_ranges.into_iter().map(|tx| tx.into()).collect();
        let connection = self.connection_pool.connection()?;

        connection.fetch_collect(InsertBlockRangeRootQuery::insert_many(records)?)
    }

    /// Get the highest [ChainPoint] of the cardano transactions stored in the database.
    pub async fn get_transaction_highest_chain_point(&self) -> StdResult<Option<ChainPoint>> {
        let first_transaction_with_highest_block_number = self
            .connection_pool
            .connection()?
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
            .connection_pool
            .connection()?
            .query_single_cell("select max(start) as highest from block_range_root;", &[])?;
        highest
            .map(u64::try_from)
            .transpose()
            .map(|num| num.map(BlockNumber))
            .with_context(||
                format!("Integer field max(start) (value={highest:?}) is incompatible with u64 representation.")
            )
    }

    /// Retrieve all the Block Range Roots in database up to the block range that contains the given
    /// block number.
    pub async fn retrieve_block_range_roots_up_to(
        &self,
        block_number: BlockNumber,
    ) -> StdResult<Box<dyn Iterator<Item = (BlockRange, MKTreeNode)> + '_>> {
        let block_range_roots = self
            .connection_pool
            .connection()?
            .fetch(GetBlockRangeRootQuery::contains_or_below_block_number(
                block_number,
            ))?
            .map(|record| -> (BlockRange, MKTreeNode) { record.into() })
            .collect::<Vec<_>>(); // TODO: remove this collect to return the iterator directly

        Ok(Box::new(block_range_roots.into_iter()))
    }

    /// Retrieve the block range root with the highest bounds in the database.
    pub async fn retrieve_highest_block_range_root(
        &self,
    ) -> StdResult<Option<BlockRangeRootRecord>> {
        self.connection_pool
            .connection()?
            .fetch_first(GetBlockRangeRootQuery::highest())
    }

    /// Retrieve all the [CardanoTransaction] in database.
    pub async fn get_all(&self) -> StdResult<Vec<CardanoTransaction>> {
        let records = self
            .connection_pool
            .connection()?
            .fetch(GetCardanoTransactionQuery::all())?
            .map(|record| record.into())
            .collect();

        Ok(records)
    }

    /// Retrieve all the [BlockRangeRootRecord] in database.
    pub fn get_all_block_range_root(&self) -> StdResult<Vec<BlockRangeRootRecord>> {
        self.connection_pool
            .connection()?
            .fetch_collect(GetBlockRangeRootQuery::all())
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
            let connection = self.connection_pool.connection()?;
            let transaction = connection.begin_transaction()?;

            // Chunk transactions to avoid an error when we exceed sqlite binding limitations
            for transactions_in_chunk in transactions_in_db_transaction_chunk.chunks(100) {
                self.create_transactions_with_connection(
                    transactions_in_chunk.to_vec(),
                    &connection,
                )
                .await
                .with_context(|| "CardanoTransactionRepository can not store transactions")?;
            }

            transaction.commit()?;
        }
        Ok(())
    }

    /// Get the block number for a given slot number
    pub async fn get_block_number_by_slot_number(
        &self,
        slot_number: SlotNumber,
    ) -> StdResult<Option<BlockNumber>> {
        let query = GetCardanoTransactionQuery::by_slot_number(slot_number);
        let record = self.connection_pool.connection()?.fetch_first(query)?;

        Ok(record.map(|r| r.block_number))
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
        self.connection_pool.connection()?.fetch_collect(query)
    }

    /// Get the [CardanoTransactionRecord] for the given block ranges.
    pub async fn get_transaction_by_block_ranges(
        &self,
        block_ranges: Vec<BlockRange>,
    ) -> StdResult<Vec<CardanoTransactionRecord>> {
        let mut transactions = vec![];
        for block_range in block_ranges {
            let block_range_transactions: Vec<CardanoTransactionRecord> =
                self.connection_pool.connection()?.fetch_collect(
                    GetCardanoTransactionQuery::by_block_ranges(vec![block_range]),
                )?;
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
            let threshold = highest_block_range_start - number_of_blocks_to_keep;
            let query = DeleteCardanoTransactionQuery::below_block_number_threshold(threshold)?;

            let connection = self.connection_pool.connection()?;
            connection.fetch_first(query)?;
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
        let connection = self.connection_pool.connection()?;
        let transaction = connection.begin_transaction()?;
        let query = DeleteCardanoTransactionQuery::above_block_number_threshold(block_number)?;
        connection.fetch_first(query)?;

        let query =
            DeleteBlockRangeRootQuery::contains_or_above_block_number_threshold(block_number)?;
        connection.fetch_first(query)?;
        transaction.commit()?;

        Ok(())
    }
}

#[async_trait]
impl<S: MKTreeStore> BlockRangeRootRetriever<S> for CardanoTransactionRepository {
    async fn retrieve_block_range_roots<'a>(
        &'a self,
        up_to_beacon: BlockNumber,
    ) -> StdResult<Box<dyn Iterator<Item = (BlockRange, MKTreeNode)> + 'a>> {
        self.retrieve_block_range_roots_up_to(up_to_beacon).await
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
        let repository = CardanoTransactionRepository::new(Arc::new(
            SqliteConnectionPool::build(1, cardano_tx_db_connection).unwrap(),
        ));

        repository
            .create_transactions(vec![
                CardanoTransaction::new(
                    "tx_hash-123",
                    BlockNumber(10),
                    SlotNumber(50),
                    "block_hash-123",
                ),
                CardanoTransaction::new(
                    "tx_hash-456",
                    BlockNumber(11),
                    SlotNumber(51),
                    "block_hash-456",
                ),
            ])
            .await
            .unwrap();

        {
            let transaction_result = repository.get_transaction("tx_hash-123").await.unwrap();
            assert_eq!(
                Some(CardanoTransactionRecord {
                    transaction_hash: "tx_hash-123".to_string(),
                    block_number: BlockNumber(10),
                    slot_number: SlotNumber(50),
                    block_hash: "block_hash-123".to_string(),
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
        let repository = CardanoTransactionRepository::new(Arc::new(
            SqliteConnectionPool::build(1, cardano_tx_db_connection).unwrap(),
        ));

        repository
            .create_transactions(vec![
                CardanoTransactionRecord::new(
                    "tx_hash-123",
                    BlockNumber(10),
                    SlotNumber(50),
                    "block_hash-123",
                ),
                CardanoTransactionRecord::new(
                    "tx_hash-456",
                    BlockNumber(11),
                    SlotNumber(51),
                    "block_hash-456",
                ),
                CardanoTransactionRecord::new(
                    "tx_hash-789",
                    BlockNumber(12),
                    SlotNumber(52),
                    "block_hash-789",
                ),
                CardanoTransactionRecord::new(
                    "tx_hash-000",
                    BlockNumber(101),
                    SlotNumber(100),
                    "block_hash-000",
                ),
            ])
            .await
            .unwrap();

        {
            let transactions = repository
                .get_transaction_by_hashes(vec!["tx_hash-123", "tx_hash-789"], BlockNumber(100))
                .await
                .unwrap();

            assert_eq!(
                vec![
                    CardanoTransactionRecord::new(
                        "tx_hash-123",
                        BlockNumber(10),
                        SlotNumber(50),
                        "block_hash-123"
                    ),
                    CardanoTransactionRecord::new(
                        "tx_hash-789",
                        BlockNumber(12),
                        SlotNumber(52),
                        "block_hash-789"
                    ),
                ],
                transactions
            );
        }
        {
            let transactions = repository
                .get_transaction_by_hashes(
                    vec!["tx_hash-123", "tx_hash-789", "tx_hash-000"],
                    BlockNumber(100),
                )
                .await
                .unwrap();

            assert_eq!(
                vec![
                    CardanoTransactionRecord::new(
                        "tx_hash-123",
                        BlockNumber(10),
                        SlotNumber(50),
                        "block_hash-123"
                    ),
                    CardanoTransactionRecord::new(
                        "tx_hash-789",
                        BlockNumber(12),
                        SlotNumber(52),
                        "block_hash-789"
                    ),
                ],
                transactions
            );
        }
        {
            let transactions = repository
                .get_transaction_by_hashes(
                    vec!["tx_hash-123", "tx_hash-789", "tx_hash-000"],
                    BlockNumber(101),
                )
                .await
                .unwrap();

            assert_eq!(
                vec![
                    CardanoTransactionRecord::new(
                        "tx_hash-123",
                        BlockNumber(10),
                        SlotNumber(50),
                        "block_hash-123"
                    ),
                    CardanoTransactionRecord::new(
                        "tx_hash-789",
                        BlockNumber(12),
                        SlotNumber(52),
                        "block_hash-789"
                    ),
                    CardanoTransactionRecord::new(
                        "tx_hash-000",
                        BlockNumber(101),
                        SlotNumber(100),
                        "block_hash-000"
                    ),
                ],
                transactions
            );
        }
        {
            let transactions = repository
                .get_transaction_by_hashes(vec!["not-exist".to_string()], BlockNumber(100))
                .await
                .unwrap();

            assert_eq!(Vec::<CardanoTransactionRecord>::new(), transactions);
        }
    }

    #[tokio::test]
    async fn repository_create_ignore_further_transactions_when_exists() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = CardanoTransactionRepository::new(Arc::new(
            SqliteConnectionPool::build_from_connection(connection),
        ));

        repository
            .create_transaction(
                "tx-hash-123",
                BlockNumber(10),
                SlotNumber(50),
                "block_hash-123",
            )
            .await
            .unwrap();
        repository
            .create_transaction(
                "tx-hash-123",
                BlockNumber(11),
                SlotNumber(51),
                "block_hash-123-bis",
            )
            .await
            .unwrap();
        let transaction_result = repository.get_transaction("tx-hash-123").await.unwrap();

        assert_eq!(
            Some(CardanoTransactionRecord {
                transaction_hash: "tx-hash-123".to_string(),
                block_number: BlockNumber(10),
                slot_number: SlotNumber(50),
                block_hash: "block_hash-123".to_string(),
            }),
            transaction_result
        );
    }

    #[tokio::test]
    async fn repository_store_transactions_and_get_stored_transactions() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = CardanoTransactionRepository::new(Arc::new(
            SqliteConnectionPool::build_from_connection(connection),
        ));

        let cardano_transactions = vec![
            CardanoTransaction::new(
                "tx-hash-123",
                BlockNumber(10),
                SlotNumber(50),
                "block-hash-123",
            ),
            CardanoTransaction::new(
                "tx-hash-456",
                BlockNumber(11),
                SlotNumber(51),
                "block-hash-456",
            ),
        ];
        repository
            .create_transactions(cardano_transactions)
            .await
            .unwrap();

        let transaction_result = repository.get_transaction("tx-hash-123").await.unwrap();

        assert_eq!(
            Some(CardanoTransactionRecord {
                transaction_hash: "tx-hash-123".to_string(),
                block_number: BlockNumber(10),
                slot_number: SlotNumber(50),
                block_hash: "block-hash-123".to_string(),
            }),
            transaction_result
        );

        let transaction_result = repository.get_transaction("tx-hash-456").await.unwrap();

        assert_eq!(
            Some(CardanoTransactionRecord {
                transaction_hash: "tx-hash-456".to_string(),
                block_number: BlockNumber(11),
                slot_number: SlotNumber(51),
                block_hash: "block-hash-456".to_string(),
            }),
            transaction_result
        );
    }

    #[tokio::test]
    async fn repository_get_all_stored_transactions() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = CardanoTransactionRepository::new(Arc::new(
            SqliteConnectionPool::build_from_connection(connection),
        ));

        let cardano_transactions = vec![
            CardanoTransaction::new(
                "tx-hash-123",
                BlockNumber(10),
                SlotNumber(50),
                "block-hash-123",
            ),
            CardanoTransaction::new(
                "tx-hash-456",
                BlockNumber(11),
                SlotNumber(51),
                "block-hash-456",
            ),
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
        let connection = cardano_tx_db_connection().unwrap();
        let repository = CardanoTransactionRepository::new(Arc::new(
            SqliteConnectionPool::build_from_connection(connection),
        ));

        repository
            .create_transaction("tx-hash-000", BlockNumber(1), SlotNumber(5), "block-hash")
            .await
            .unwrap();

        let cardano_transactions = vec![CardanoTransaction::new(
            "tx-hash-123",
            BlockNumber(10),
            SlotNumber(50),
            "block-hash-123",
        )];
        repository
            .create_transactions(cardano_transactions)
            .await
            .unwrap();

        let transaction_result = repository.get_transaction("tx-hash-000").await.unwrap();

        assert_eq!(
            Some(CardanoTransactionRecord {
                transaction_hash: "tx-hash-000".to_string(),
                block_number: BlockNumber(1),
                slot_number: SlotNumber(5),
                block_hash: "block-hash".to_string(),
            }),
            transaction_result
        );
    }

    #[tokio::test]
    async fn repository_get_transaction_highest_chain_point_without_transactions_in_db() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = CardanoTransactionRepository::new(Arc::new(
            SqliteConnectionPool::build_from_connection(connection),
        ));

        let highest_beacon = repository
            .get_transaction_highest_chain_point()
            .await
            .unwrap();
        assert_eq!(None, highest_beacon);
    }

    #[tokio::test]
    async fn repository_get_transaction_highest_chain_point_with_transactions_in_db() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = CardanoTransactionRepository::new(Arc::new(
            SqliteConnectionPool::build_from_connection(connection),
        ));

        let cardano_transactions = vec![
            CardanoTransaction::new(
                "tx-hash-123",
                BlockNumber(10),
                SlotNumber(50),
                "block-hash-10",
            ),
            CardanoTransaction::new(
                "tx-hash-456",
                BlockNumber(25),
                SlotNumber(51),
                "block-hash-25",
            ),
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
                slot_number: SlotNumber(51),
                block_number: BlockNumber(25),
                block_hash: "block-hash-25".to_string()
            }),
            highest_beacon
        );
    }

    #[tokio::test]
    async fn repository_get_transaction_highest_chain_point_with_transactions_with_same_block_number_in_db(
    ) {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = CardanoTransactionRepository::new(Arc::new(
            SqliteConnectionPool::build_from_connection(connection),
        ));

        let cardano_transactions = vec![
            CardanoTransaction::new(
                "tx-hash-123",
                BlockNumber(10),
                SlotNumber(50),
                "block-hash-10",
            ),
            CardanoTransaction::new(
                "tx-hash-456",
                BlockNumber(25),
                SlotNumber(51),
                "block-hash-25",
            ),
            CardanoTransaction::new(
                "tx-hash-789",
                BlockNumber(25),
                SlotNumber(51),
                "block-hash-25",
            ),
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
                slot_number: SlotNumber(51),
                block_number: BlockNumber(25),
                block_hash: "block-hash-25".to_string()
            }),
            highest_beacon
        );
    }

    #[tokio::test]
    async fn repository_get_transactions_in_range_blocks() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = CardanoTransactionRepository::new(Arc::new(
            SqliteConnectionPool::build_from_connection(connection),
        ));

        let transactions = vec![
            CardanoTransactionRecord::new(
                "tx-hash-1",
                BlockNumber(10),
                SlotNumber(50),
                "block-hash-1",
            ),
            CardanoTransactionRecord::new(
                "tx-hash-2",
                BlockNumber(11),
                SlotNumber(51),
                "block-hash-2",
            ),
            CardanoTransactionRecord::new(
                "tx-hash-3",
                BlockNumber(12),
                SlotNumber(52),
                "block-hash-3",
            ),
        ];
        repository
            .create_transactions(transactions.clone())
            .await
            .unwrap();

        {
            let transaction_result = repository
                .get_transactions_in_range_blocks(BlockNumber(0)..BlockNumber(10))
                .await
                .unwrap();
            assert_eq!(Vec::<CardanoTransactionRecord>::new(), transaction_result);
        }
        {
            let transaction_result = repository
                .get_transactions_in_range_blocks(BlockNumber(13)..BlockNumber(21))
                .await
                .unwrap();
            assert_eq!(Vec::<CardanoTransactionRecord>::new(), transaction_result);
        }
        {
            let transaction_result = repository
                .get_transactions_in_range_blocks(BlockNumber(9)..BlockNumber(12))
                .await
                .unwrap();
            assert_eq!(transactions[0..=1].to_vec(), transaction_result);
        }
        {
            let transaction_result = repository
                .get_transactions_in_range_blocks(BlockNumber(10)..BlockNumber(13))
                .await
                .unwrap();
            assert_eq!(transactions.clone(), transaction_result);
        }
        {
            let transaction_result = repository
                .get_transactions_in_range_blocks(BlockNumber(11)..BlockNumber(14))
                .await
                .unwrap();
            assert_eq!(transactions[1..=2].to_vec(), transaction_result);
        }
    }

    #[tokio::test]
    async fn repository_get_transactions_by_block_ranges() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = CardanoTransactionRepository::new(Arc::new(
            SqliteConnectionPool::build_from_connection(connection),
        ));

        let transactions = vec![
            CardanoTransactionRecord::new(
                "tx-hash-1",
                BlockNumber(10),
                SlotNumber(50),
                "block-hash-1",
            ),
            CardanoTransactionRecord::new(
                "tx-hash-2",
                BlockNumber(11),
                SlotNumber(51),
                "block-hash-2",
            ),
            CardanoTransactionRecord::new(
                "tx-hash-3",
                BlockNumber(20),
                SlotNumber(52),
                "block-hash-3",
            ),
            CardanoTransactionRecord::new(
                "tx-hash-4",
                BlockNumber(31),
                SlotNumber(53),
                "block-hash-4",
            ),
            CardanoTransactionRecord::new(
                "tx-hash-5",
                BlockNumber(35),
                SlotNumber(54),
                "block-hash-5",
            ),
            CardanoTransactionRecord::new(
                "tx-hash-6",
                BlockNumber(46),
                SlotNumber(55),
                "block-hash-6",
            ),
        ];
        repository
            .create_transactions(transactions.clone())
            .await
            .unwrap();

        {
            let transaction_result = repository
                .get_transaction_by_block_ranges(vec![BlockRange::from_block_number(BlockNumber(
                    100,
                ))])
                .await
                .unwrap();
            assert_eq!(Vec::<CardanoTransactionRecord>::new(), transaction_result);
        }
        {
            let transaction_result = repository
                .get_transaction_by_block_ranges(vec![BlockRange::from_block_number(BlockNumber(
                    0,
                ))])
                .await
                .unwrap();
            assert_eq!(transactions[0..=1].to_vec(), transaction_result);
        }
        {
            let transaction_result = repository
                .get_transaction_by_block_ranges(vec![
                    BlockRange::from_block_number(BlockNumber(0)),
                    BlockRange::from_block_number(BlockNumber(15)),
                ])
                .await
                .unwrap();
            assert_eq!(transactions[0..=2].to_vec(), transaction_result);
        }
        {
            let transaction_result = repository
                .get_transaction_by_block_ranges(vec![
                    BlockRange::from_block_number(BlockNumber(0)),
                    BlockRange::from_block_number(BlockNumber(30)),
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
    async fn repository_get_block_number_by_slot_number() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = CardanoTransactionRepository::new(Arc::new(
            SqliteConnectionPool::build_from_connection(connection),
        ));

        let transactions = vec![
            CardanoTransactionRecord::new("tx-1", BlockNumber(100), SlotNumber(500), "block-1"),
            CardanoTransactionRecord::new("tx-2", BlockNumber(100), SlotNumber(500), "block-1"),
            CardanoTransactionRecord::new("tx-3", BlockNumber(101), SlotNumber(501), "block-1"),
        ];
        repository
            .create_transactions(transactions.clone())
            .await
            .unwrap();

        let transaction_block_number_retrieved = repository
            .get_block_number_by_slot_number(SlotNumber(500))
            .await
            .unwrap();

        assert_eq!(transaction_block_number_retrieved, Some(BlockNumber(100)));
    }

    #[tokio::test]
    async fn repository_store_block_range() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = CardanoTransactionRepository::new(Arc::new(
            SqliteConnectionPool::build_from_connection(connection),
        ));

        repository
            .create_block_range_roots(vec![
                (
                    BlockRange::from_block_number(BlockNumber(0)),
                    MKTreeNode::from_hex("AAAA").unwrap(),
                ),
                (
                    BlockRange::from_block_number(BlockRange::LENGTH),
                    MKTreeNode::from_hex("BBBB").unwrap(),
                ),
            ])
            .await
            .unwrap();

        let records: Vec<BlockRangeRootRecord> = repository
            .connection_pool
            .connection()
            .unwrap()
            .fetch_collect(GetBlockRangeRootQuery::all())
            .unwrap();
        assert_eq!(
            vec![
                BlockRangeRootRecord {
                    range: BlockRange::from_block_number(BlockNumber(0)),
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
        let connection = cardano_tx_db_connection().unwrap();
        let repository = CardanoTransactionRepository::new(Arc::new(
            SqliteConnectionPool::build_from_connection(connection),
        ));
        let range = BlockRange::from_block_number(BlockNumber(0));

        repository
            .create_block_range_roots(vec![(range.clone(), MKTreeNode::from_hex("AAAA").unwrap())])
            .await
            .unwrap();
        repository
            .create_block_range_roots(vec![(range.clone(), MKTreeNode::from_hex("BBBB").unwrap())])
            .await
            .unwrap();

        let record: Vec<BlockRangeRootRecord> = repository
            .connection_pool
            .connection()
            .unwrap()
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
        let connection = cardano_tx_db_connection().unwrap();
        let repository = CardanoTransactionRepository::new(Arc::new(
            SqliteConnectionPool::build_from_connection(connection),
        ));
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
        repository
            .create_block_range_roots(block_range_roots.clone())
            .await
            .unwrap();

        let retrieved_block_ranges = repository
            .retrieve_block_range_roots_up_to(BlockNumber(45))
            .await
            .unwrap();
        assert_eq!(
            block_range_roots[0..2].to_vec(),
            retrieved_block_ranges.collect::<Vec<_>>()
        );
    }

    #[tokio::test]
    async fn repository_retrieve_highest_block_range_roots() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = CardanoTransactionRepository::new(Arc::new(
            SqliteConnectionPool::build_from_connection(connection),
        ));
        let block_range_roots = vec![
            BlockRangeRootRecord {
                range: BlockRange::from_block_number(BlockNumber(15)),
                merkle_root: MKTreeNode::from_hex("AAAA").unwrap(),
            },
            BlockRangeRootRecord {
                range: BlockRange::from_block_number(BlockNumber(30)),
                merkle_root: MKTreeNode::from_hex("BBBB").unwrap(),
            },
            BlockRangeRootRecord {
                range: BlockRange::from_block_number(BlockNumber(45)),
                merkle_root: MKTreeNode::from_hex("CCCC").unwrap(),
            },
        ];
        repository
            .create_block_range_roots(block_range_roots.clone())
            .await
            .unwrap();

        let retrieved_block_range = repository
            .retrieve_highest_block_range_root()
            .await
            .unwrap();
        assert_eq!(block_range_roots.last().cloned(), retrieved_block_range);
    }

    #[tokio::test]
    async fn repository_prune_transactions() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = CardanoTransactionRepository::new(Arc::new(
            SqliteConnectionPool::build_from_connection(connection),
        ));

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
                BlockRange::from_block_number(BlockNumber(45)),
                MKTreeNode::from_hex("BBBB").unwrap(),
            )])
            .await
            .unwrap();

        let transaction_result = repository.get_all().await.unwrap();
        assert_eq!(cardano_transactions.len(), transaction_result.len());

        // Pruning with a number of block to keep greater than the highest block range start should
        // do nothing.
        repository
            .prune_transaction(BlockNumber(10_000_000))
            .await
            .unwrap();
        let transaction_result = repository.get_all_transactions().await.unwrap();
        assert_eq!(cardano_transactions, transaction_result);

        // Since the highest block range start is 45, pruning with 20 should remove transactions
        // with a block number strictly below 25.
        repository.prune_transaction(BlockNumber(20)).await.unwrap();
        let transaction_result = repository
            .get_transactions_in_range_blocks(BlockNumber(0)..BlockNumber(25))
            .await
            .unwrap();
        assert_eq!(Vec::<CardanoTransactionRecord>::new(), transaction_result);

        let transaction_result = repository
            .get_transactions_in_range_blocks(BlockNumber(25)..BlockNumber(1000))
            .await
            .unwrap();
        assert_eq!(28, transaction_result.len());
    }

    #[tokio::test]
    async fn get_highest_start_block_number_for_block_range_roots() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = CardanoTransactionRepository::new(Arc::new(
            SqliteConnectionPool::build_from_connection(connection),
        ));

        let highest = repository
            .get_highest_start_block_number_for_block_range_roots()
            .await
            .unwrap();
        assert_eq!(None, highest);

        let block_range_roots = vec![
            (
                BlockRange::from_block_number(BlockNumber(15)),
                MKTreeNode::from_hex("AAAA").unwrap(),
            ),
            (
                BlockRange::from_block_number(BlockNumber(30)),
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
        assert_eq!(Some(BlockNumber(30)), highest);
    }

    #[tokio::test]
    async fn remove_transactions_and_block_range_greater_than_given_block_number() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = CardanoTransactionRepository::new(Arc::new(
            SqliteConnectionPool::build_from_connection(connection),
        ));

        let cardano_transactions = vec![
            CardanoTransaction::new(
                "tx-hash-123",
                BlockRange::LENGTH,
                SlotNumber(50),
                "block-hash-123",
            ),
            CardanoTransaction::new(
                "tx-hash-123",
                BlockRange::LENGTH * 3 - 1,
                SlotNumber(50),
                "block-hash-123",
            ),
            CardanoTransaction::new(
                "tx-hash-456",
                BlockRange::LENGTH * 3,
                SlotNumber(51),
                "block-hash-456",
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
