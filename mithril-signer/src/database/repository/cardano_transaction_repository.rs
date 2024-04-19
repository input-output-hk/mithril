use std::ops::RangeInclusive;
use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;
use slog_scope::warn;

use mithril_common::crypto_helper::MKTreeNode;
use mithril_common::entities::{
    BlockHash, BlockNumber, BlockRange, CardanoTransaction, ImmutableFileNumber, SlotNumber,
    TransactionHash,
};
use mithril_common::StdResult;
use mithril_persistence::sqlite::{Provider, SqliteConnection, WhereCondition};

use crate::database::provider::{
    GetCardanoTransactionProvider, GetIntervalWithoutBlockRangeRootProvider,
    InsertBlockRangeRootProvider, InsertCardanoTransactionProvider,
};
use crate::database::record::{BlockRangeRootRecord, CardanoTransactionRecord};
use crate::TransactionStore;

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

    /// Return all the [CardanoTransactionRecord]s in the database using chronological order.
    pub async fn get_all_transactions(&self) -> StdResult<Vec<CardanoTransactionRecord>> {
        let provider = GetCardanoTransactionProvider::new(&self.connection);
        let filters = WhereCondition::default();
        let transactions = provider.find(filters)?;

        Ok(transactions.collect())
    }

    /// Return all the [CardanoTransactionRecord]s in the database using chronological order.
    pub async fn get_transactions_between_blocks(
        &self,
        range: RangeInclusive<BlockNumber>,
    ) -> StdResult<Vec<CardanoTransactionRecord>> {
        let provider = GetCardanoTransactionProvider::new(&self.connection);
        let filters = provider.get_transaction_between_blocks_condition(range);
        let transactions = provider.find(filters)?;

        Ok(transactions.collect())
    }

    /// Return all the [CardanoTransactionRecord]s in the database up to the given beacon using
    /// chronological order.
    pub async fn get_transactions_up_to(
        &self,
        beacon: ImmutableFileNumber,
    ) -> StdResult<Vec<CardanoTransactionRecord>> {
        let provider = GetCardanoTransactionProvider::new(&self.connection);
        let filters = provider.get_transaction_up_to_beacon_condition(beacon);
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
    pub async fn create_block_ranges<T: Into<BlockRangeRootRecord>>(
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
}

#[cfg(test)]
pub mod test_extensions {
    use mithril_persistence::sqlite::GetAllProvider;

    use crate::database::provider::GetBlockRangeRootProvider;

    use super::*;

    impl CardanoTransactionRepository {
        pub fn get_all_block_range_root(&self) -> StdResult<Vec<BlockRangeRootRecord>> {
            let provider = GetBlockRangeRootProvider::new(&self.connection);
            let records = provider.get_all()?;

            Ok(records.collect())
        }
    }
}

#[async_trait]
impl TransactionStore for CardanoTransactionRepository {
    async fn get_highest_beacon(&self) -> StdResult<Option<ImmutableFileNumber>> {
        let sql = "select max(immutable_file_number) as highest from cardano_tx;";
        match self
            .connection
            .prepare(sql)
            .with_context(|| {
                format!(
                    "Prepare query error: SQL=`{}`",
                    &sql.replace('\n', " ").trim()
                )
            })?
            .iter()
            .next()
        {
            None => Ok(None),
            Some(row) => {
                let highest = row?.read::<Option<i64>, _>(0);
                highest
                    .map(u64::try_from)
                    .transpose()
                    .with_context(||
                        format!("Integer field max(immutable_file_number) (value={highest:?}) is incompatible with u64 representation.")
                    )
            }
        }
    }

    async fn get_block_interval_without_block_range_root(
        &self,
    ) -> StdResult<Option<RangeInclusive<BlockNumber>>> {
        let provider = GetIntervalWithoutBlockRangeRootProvider::new(&self.connection);
        let row = provider
            .find(provider.get_interval_without_block_range_condition())?
            .next();

        match row {
            // Should be impossible - the request as written in the provider always returns a single row
            None => panic!("IntervalWithoutBlockRangeProvider should always return a single row"),
            Some(interval) => match (interval.start, interval.end) {
                (_, None) => Ok(None),
                (None, Some(end)) => Ok(Some(0..=end)),
                (Some(start), Some(end)) if end < start => {
                    // To discuss : should we prune all block ranges from the DB to force recompute ?
                    warn!(
                        "Last computed block range is higher than the last transaction block number. \
                        This should not happen. Did you forgot to prune the `block_range` table after pruning the\
                        `cardano_tx` table ?";
                        "start" => start, "end" => end
                    );
                    Ok(None)
                }
                (Some(start), Some(end)) => Ok(Some(start..=end)),
            },
        }
    }

    async fn get_transactions_between(
        &self,
        range: RangeInclusive<BlockNumber>,
    ) -> StdResult<Vec<CardanoTransaction>> {
        self.get_transactions_between_blocks(range).await.map(|v| {
            v.into_iter()
                .map(|record| record.into())
                .collect::<Vec<CardanoTransaction>>()
        })
    }

    async fn get_up_to(&self, beacon: ImmutableFileNumber) -> StdResult<Vec<CardanoTransaction>> {
        self.get_transactions_up_to(beacon).await.map(|v| {
            v.into_iter()
                .map(|record| record.into())
                .collect::<Vec<CardanoTransaction>>()
        })
    }

    async fn store_transactions(&self, transactions: Vec<CardanoTransaction>) -> StdResult<()> {
        // Chunk transactions to avoid an error when we exceed sqlite binding limitations
        for transactions_in_chunk in transactions.chunks(100) {
            self.create_transactions(transactions_in_chunk.to_vec())
                .await
                .with_context(|| "CardanoTransactionRepository can not store transactions")?;
        }
        Ok(())
    }

    async fn store_block_ranges(
        &self,
        block_ranges: Vec<(BlockRange, MKTreeNode)>,
    ) -> StdResult<()> {
        self.create_block_ranges(block_ranges).await?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use mithril_persistence::sqlite::GetAllProvider;

    use crate::database::provider::GetBlockRangeRootProvider;
    use crate::database::record::BlockRangeRootRecord;
    use crate::database::test_utils::cardano_tx_db_connection;

    use super::*;

    #[tokio::test]
    async fn repository_create_and_get_transaction() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);
        repository
            .create_transaction("tx-hash-123", 10, 50, "block_hash-123", 99)
            .await
            .unwrap();
        repository
            .create_transaction("tx-hash-456", 11, 51, "block_hash-456", 100)
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

        let cardano_transactions: Vec<CardanoTransactionRecord> = (20..=40)
            .map(|i| CardanoTransactionRecord {
                transaction_hash: format!("tx-hash-{i}"),
                block_number: i % 10,
                slot_number: i * 100,
                block_hash: format!("block-hash-{i}"),
                immutable_file_number: i,
            })
            .collect();
        repository
            .create_transactions(cardano_transactions.clone())
            .await
            .unwrap();

        let transaction_result = repository.get_transactions_up_to(34).await.unwrap();
        assert_eq!(cardano_transactions[0..=14].to_vec(), transaction_result);

        let transaction_result = repository.get_transactions_up_to(300).await.unwrap();
        assert_eq!(cardano_transactions.clone(), transaction_result);

        let transaction_result = repository.get_transactions_up_to(19).await.unwrap();
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
    async fn repository_get_highest_beacon_without_transactions_in_db() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);

        let highest_beacon = repository.get_highest_beacon().await.unwrap();
        assert_eq!(None, highest_beacon);
    }

    #[tokio::test]
    async fn repository_get_highest_beacon_with_transactions_in_db() {
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

        let highest_beacon = repository.get_highest_beacon().await.unwrap();
        assert_eq!(Some(100), highest_beacon);
    }

    #[tokio::test]
    async fn repository_get_transactions_between_blocks() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);

        let transactions = vec![
            CardanoTransaction::new("tx-hash-1", 10, 50, "block-hash-1", 99),
            CardanoTransaction::new("tx-hash-2", 11, 51, "block-hash-2", 100),
            CardanoTransaction::new("tx-hash-3", 12, 52, "block-hash-3", 101),
        ];
        repository
            .create_transactions(transactions.clone())
            .await
            .unwrap();

        {
            let transaction_result = repository.get_transactions_between(0..=9).await.unwrap();
            assert_eq!(Vec::<CardanoTransaction>::new(), transaction_result);
        }
        {
            let transaction_result = repository.get_transactions_between(13..=20).await.unwrap();
            assert_eq!(Vec::<CardanoTransaction>::new(), transaction_result);
        }
        {
            let transaction_result = repository.get_transactions_between(9..=11).await.unwrap();
            assert_eq!(transactions[0..=1].to_vec(), transaction_result);
        }
        {
            let transaction_result = repository.get_transactions_between(10..=12).await.unwrap();
            assert_eq!(transactions.clone(), transaction_result);
        }
        {
            let transaction_result = repository.get_transactions_between(11..=13).await.unwrap();
            assert_eq!(transactions[1..=2].to_vec(), transaction_result);
        }
    }

    #[tokio::test]
    async fn repository_get_block_interval_without_block_range_root() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);

        {
            let interval = repository
                .get_block_interval_without_block_range_root()
                .await
                .unwrap();

            assert_eq!(None, interval);
        }

        // The last transaction block number give the upper bound
        let last_transaction_block_number = BlockRange::LENGTH * 4;
        repository
            .create_transaction("tx-1", last_transaction_block_number, 50, "block-1", 99)
            .await
            .unwrap();

        {
            let interval = repository
                .get_block_interval_without_block_range_root()
                .await
                .unwrap();

            assert_eq!(Some(0..=last_transaction_block_number), interval);
        }
        {
            // The last block range give the lower bound
            let last_block_range = BlockRange::from_block_number(0);
            repository
                .store_block_ranges(vec![(
                    last_block_range.clone(),
                    MKTreeNode::from_hex("AAAA").unwrap(),
                )])
                .await
                .unwrap();

            let interval = repository
                .get_block_interval_without_block_range_root()
                .await
                .unwrap();

            assert_eq!(
                Some(last_block_range.end..=last_transaction_block_number),
                interval
            );
        }
    }

    #[tokio::test]
    async fn repository_get_block_interval_without_block_range_root_when_last_block_range_higher_than_stored_transactions(
    ) {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let repository = CardanoTransactionRepository::new(connection);

        let last_block_range = BlockRange::from_block_number(BlockRange::LENGTH * 10);
        repository
            .store_block_ranges(vec![(
                last_block_range.clone(),
                MKTreeNode::from_hex("AAAA").unwrap(),
            )])
            .await
            .unwrap();

        // Only block range in db
        {
            let interval = repository
                .get_block_interval_without_block_range_root()
                .await
                .unwrap();

            assert_eq!(None, interval);
        }
        // Highest transaction block number is below the last computed block range, this may happen
        // if the latest transactions were pruned from DB (whatever the reason for this pruned was).
        {
            let last_transaction_block_number = last_block_range.end - 10;
            repository
                .create_transaction("tx-1", last_transaction_block_number, 50, "block-1", 99)
                .await
                .unwrap();

            let interval = repository
                .get_block_interval_without_block_range_root()
                .await
                .unwrap();

            assert_eq!(None, interval);
        }
    }

    #[tokio::test]
    async fn repository_store_block_range() {
        let connection = Arc::new(cardano_tx_db_connection().unwrap());
        let provider = GetBlockRangeRootProvider::new(&connection);
        let repository = CardanoTransactionRepository::new(connection.clone());

        repository
            .store_block_ranges(vec![
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
            .store_block_ranges(vec![(range.clone(), MKTreeNode::from_hex("AAAA").unwrap())])
            .await
            .unwrap();
        repository
            .store_block_ranges(vec![(range.clone(), MKTreeNode::from_hex("BBBB").unwrap())])
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
}
