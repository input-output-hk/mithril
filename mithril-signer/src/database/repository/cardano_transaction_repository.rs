use std::ops::Range;

use async_trait::async_trait;

use mithril_common::StdResult;
use mithril_common::crypto_helper::MKTreeNode;
use mithril_common::entities::{
    BlockNumber, BlockRange, CardanoTransaction, ChainPoint, SlotNumber,
};
use mithril_persistence::database::repository::CardanoTransactionRepository;

use crate::services::{HighestTransactionBlockNumberGetter, TransactionPruner, TransactionStore};

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
        self.remove_rolled_back_transactions_and_block_range_by_slot_number(slot_number)
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
