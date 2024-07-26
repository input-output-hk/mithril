use std::ops::Range;

use async_trait::async_trait;

use mithril_common::crypto_helper::MKTreeNode;
use mithril_common::entities::{
    BlockNumber, BlockRange, CardanoTransaction, ChainPoint, SlotNumber, TransactionHash,
};
use mithril_common::StdResult;
use mithril_persistence::database::repository::CardanoTransactionRepository;

use crate::services::{TransactionStore, TransactionsRetriever};

#[async_trait]
impl TransactionStore for CardanoTransactionRepository {
    async fn get_highest_beacon(&self) -> StdResult<Option<ChainPoint>> {
        self.get_transaction_highest_chain_point().await
    }

    async fn get_highest_block_range(&self) -> StdResult<Option<BlockRange>> {
        let record = self.retrieve_highest_block_range_root().await?;
        Ok(record.map(|record| record.range))
    }

    async fn store_transactions(&self, transactions: Vec<CardanoTransaction>) -> StdResult<()> {
        self.store_transactions(transactions).await
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
            self.create_block_range_roots(block_ranges).await?;
        }
        Ok(())
    }

    async fn remove_rolled_back_transactions_and_block_range(
        &self,
        slot_number: SlotNumber,
    ) -> StdResult<()> {
        let block_number = self
            .get_block_number_by_slot_number(slot_number)
            .await?
            .ok_or_else(|| {
                anyhow::anyhow!("No block number found for slot number {}", slot_number)
            })?;
        self.remove_rolled_back_transactions_and_block_range(block_number)
            .await
    }
}

#[async_trait]
impl TransactionsRetriever for CardanoTransactionRepository {
    async fn get_by_hashes(
        &self,
        hashes: Vec<TransactionHash>,
        up_to: BlockNumber,
    ) -> StdResult<Vec<CardanoTransaction>> {
        self.get_transaction_by_hashes(hashes, up_to)
            .await
            .map(|v| {
                v.into_iter()
                    .map(|record| record.into())
                    .collect::<Vec<CardanoTransaction>>()
            })
    }

    async fn get_by_block_ranges(
        &self,
        block_ranges: Vec<BlockRange>,
    ) -> StdResult<Vec<CardanoTransaction>> {
        self.get_transaction_by_block_ranges(block_ranges)
            .await
            .map(|v| {
                v.into_iter()
                    .map(|record| record.into())
                    .collect::<Vec<CardanoTransaction>>()
            })
    }
}
