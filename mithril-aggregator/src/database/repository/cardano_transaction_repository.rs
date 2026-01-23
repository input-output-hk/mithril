use std::ops::{Deref, Range};
use std::sync::Arc;

use mithril_cardano_node_chain::chain_importer::ChainDataStore;
use mithril_common::StdResult;
use mithril_common::crypto_helper::{MKTreeNode, MKTreeStorer};
use mithril_common::entities::{
    BlockNumber, BlockRange, CardanoTransaction, ChainPoint, SlotNumber, TransactionHash,
};
use mithril_common::signable_builder::BlockRangeRootRetriever;
use mithril_persistence::database::repository::CardanoTransactionRepository;
use mithril_persistence::sqlite::SqliteConnectionPool;

use crate::services::TransactionsRetriever;

/// Wrapper around [CardanoTransactionRepository] to allow traits implementations
pub struct AggregatorCardanoChainDataRepository {
    inner: CardanoTransactionRepository,
}

impl AggregatorCardanoChainDataRepository {
    /// Instantiate a new `AggregatorCardanoChainDataRepository`
    pub fn new(connection_pool: Arc<SqliteConnectionPool>) -> Self {
        Self {
            inner: CardanoTransactionRepository::new(connection_pool),
        }
    }
}

impl Deref for AggregatorCardanoChainDataRepository {
    type Target = CardanoTransactionRepository;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[async_trait::async_trait]
impl ChainDataStore for AggregatorCardanoChainDataRepository {
    async fn get_highest_beacon(&self) -> StdResult<Option<ChainPoint>> {
        self.inner.get_transaction_highest_chain_point().await
    }

    async fn get_highest_block_range(&self) -> StdResult<Option<BlockRange>> {
        let record = self.inner.retrieve_highest_legacy_block_range_root().await?;
        Ok(record.map(|record| record.range))
    }

    async fn store_transactions(&self, transactions: Vec<CardanoTransaction>) -> StdResult<()> {
        self.inner.store_transactions(transactions).await
    }

    async fn get_transactions_in_range(
        &self,
        range: Range<BlockNumber>,
    ) -> StdResult<Vec<CardanoTransaction>> {
        self.get_transactions_in_range_blocks(range).await.map(|v| {
            v.into_iter()
                .map(|record| record.into())
                .collect::<Vec<CardanoTransaction>>()
        })
    }

    async fn store_block_range_roots(
        &self,
        block_ranges: Vec<(BlockRange, MKTreeNode)>,
    ) -> StdResult<()> {
        if !block_ranges.is_empty() {
            self.inner.create_legacy_block_range_roots(block_ranges).await?;
        }
        Ok(())
    }

    async fn remove_rolled_back_transactions_and_block_range(
        &self,
        slot_number: SlotNumber,
    ) -> StdResult<()> {
        self.inner
            .remove_rolled_back_transactions_and_block_range_by_slot_number(slot_number)
            .await
    }
}

#[async_trait::async_trait]
impl<S: MKTreeStorer> BlockRangeRootRetriever<S> for AggregatorCardanoChainDataRepository {
    async fn retrieve_block_range_roots<'a>(
        &'a self,
        up_to_beacon: BlockNumber,
    ) -> StdResult<Box<dyn Iterator<Item = (BlockRange, MKTreeNode)> + 'a>> {
        self.inner.retrieve_legacy_block_range_roots_up_to(up_to_beacon).await
    }
}

#[async_trait::async_trait]
impl TransactionsRetriever for AggregatorCardanoChainDataRepository {
    async fn get_by_hashes(
        &self,
        hashes: Vec<TransactionHash>,
        up_to: BlockNumber,
    ) -> StdResult<Vec<CardanoTransaction>> {
        self.inner.get_transaction_by_hashes(hashes, up_to).await.map(|v| {
            v.into_iter()
                .map(|record| record.into())
                .collect::<Vec<CardanoTransaction>>()
        })
    }

    async fn get_by_block_ranges(
        &self,
        block_ranges: Vec<BlockRange>,
    ) -> StdResult<Vec<CardanoTransaction>> {
        self.inner
            .get_transaction_by_block_ranges(block_ranges)
            .await
            .map(|v| {
                v.into_iter()
                    .map(|record| record.into())
                    .collect::<Vec<CardanoTransaction>>()
            })
    }
}

#[async_trait::async_trait]
impl TransactionsRetriever for CardanoTransactionRepository {
    async fn get_by_hashes(
        &self,
        hashes: Vec<TransactionHash>,
        up_to: BlockNumber,
    ) -> StdResult<Vec<CardanoTransaction>> {
        self.get_transaction_by_hashes(hashes, up_to).await.map(|v| {
            v.into_iter()
                .map(|record| record.into())
                .collect::<Vec<CardanoTransaction>>()
        })
    }

    async fn get_by_block_ranges(
        &self,
        block_ranges: Vec<BlockRange>,
    ) -> StdResult<Vec<CardanoTransaction>> {
        self.get_transaction_by_block_ranges(block_ranges).await.map(|v| {
            v.into_iter()
                .map(|record| record.into())
                .collect::<Vec<CardanoTransaction>>()
        })
    }
}

#[cfg(test)]
mod tests {
    use mithril_cardano_node_chain::chain_importer::{CardanoChainDataImporter, ChainDataImporter};
    use mithril_cardano_node_chain::entities::ScannedBlock;
    use mithril_cardano_node_chain::test::double::DumbBlockScanner;

    use crate::database::test_helper::cardano_tx_db_connection;
    use crate::test::TestLogger;

    use super::*;

    fn into_transactions(blocks: &[ScannedBlock]) -> Vec<CardanoTransaction> {
        blocks.iter().flat_map(|b| b.clone().into_transactions()).collect()
    }

    #[tokio::test]
    async fn importing_twice_starting_with_nothing_in_a_real_db_should_yield_transactions_in_same_order()
     {
        let blocks = vec![
            ScannedBlock::new(
                "block_hash-1",
                BlockNumber(10),
                SlotNumber(15),
                vec!["tx_hash-1", "tx_hash-2"],
            ),
            ScannedBlock::new(
                "block_hash-2",
                BlockNumber(20),
                SlotNumber(25),
                vec!["tx_hash-3", "tx_hash-4"],
            ),
        ];
        let up_to_block_number = BlockNumber(1000);
        let transactions = into_transactions(&blocks);

        let (importer, repository) = {
            let connection = cardano_tx_db_connection().unwrap();
            let connection_pool = Arc::new(SqliteConnectionPool::build_from_connection(connection));
            let repository = Arc::new(AggregatorCardanoChainDataRepository::new(connection_pool));
            let importer = CardanoChainDataImporter::new(
                Arc::new(DumbBlockScanner::new().forwards(vec![blocks.clone()])),
                repository.clone(),
                TestLogger::stdout(),
            );
            (importer, repository)
        };

        importer
            .import(up_to_block_number)
            .await
            .expect("Transactions Importer should succeed");
        let cold_imported_transactions = repository.get_all().await.unwrap();

        importer
            .import(up_to_block_number)
            .await
            .expect("Transactions Importer should succeed");
        let warm_imported_transactions = repository.get_all().await.unwrap();

        assert_eq!(transactions, cold_imported_transactions);
        assert_eq!(cold_imported_transactions, warm_imported_transactions);
    }
}
