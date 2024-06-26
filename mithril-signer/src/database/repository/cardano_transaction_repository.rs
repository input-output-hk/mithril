use std::ops::Range;

use async_trait::async_trait;

use mithril_common::crypto_helper::MKTreeNode;
use mithril_common::entities::{
    BlockNumber, BlockRange, CardanoTransaction, ChainPoint, SlotNumber,
};
use mithril_common::StdResult;
use mithril_persistence::database::repository::CardanoTransactionRepository;

use crate::{HighestTransactionBlockNumberGetter, TransactionPruner, TransactionStore};

#[async_trait]
impl TransactionStore for CardanoTransactionRepository {
    async fn get_highest_beacon(&self) -> StdResult<Option<ChainPoint>> {
        self.get_transaction_highest_chain_point().await
    }

    async fn store_transactions(&self, transactions: Vec<CardanoTransaction>) -> StdResult<()> {
        self.store_transactions(transactions).await
    }

    async fn get_block_interval_without_block_range_root(
        &self,
    ) -> StdResult<Option<Range<BlockNumber>>> {
        self.get_block_interval_without_block_range_root().await
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
impl TransactionPruner for CardanoTransactionRepository {
    async fn prune(&self, number_of_blocks_to_keep: BlockNumber) -> StdResult<()> {
        self.prune_transaction(number_of_blocks_to_keep).await
    }
}

#[async_trait]
impl HighestTransactionBlockNumberGetter for CardanoTransactionRepository {
    async fn get(&self) -> StdResult<Option<BlockNumber>> {
        let highest_chain_point = self.get_transaction_highest_chain_point().await?;
        Ok(highest_chain_point.map(|c| c.block_number))
    }
}
