use std::ops::Range;

use async_trait::async_trait;

use mithril_common::StdResult;
use mithril_common::crypto_helper::MKTreeNode;
use mithril_common::entities::{
    BlockNumber, BlockRange, CardanoTransaction, ChainPoint, SlotNumber,
};
use mithril_common::signable_builder::TransactionsImporter;

/// Cardano chain data importer
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait ChainDataImporter: Send + Sync {
    /// Import data stored on the chain up to the given beacon into the system
    async fn import(&self, up_to_beacon: BlockNumber) -> StdResult<()>;
}

#[async_trait]
impl TransactionsImporter for dyn ChainDataImporter {
    async fn import(&self, up_to_beacon: BlockNumber) -> StdResult<()> {
        self.import(up_to_beacon).await
    }
}

/// Macro to generates the boilerplate code to bridge Signable builder importers (currently `TransactionsImporter`)
/// for types that implement ChainDataImporter.
#[macro_export]
macro_rules! impl_signable_builder_importers_for_chain_data_importer {
    ($type:ty) => {
        #[async_trait::async_trait]
        impl mithril_common::signable_builder::TransactionsImporter for $type {
            async fn import(
                &self,
                up_to_beacon: mithril_common::entities::BlockNumber,
            ) -> mithril_common::StdResult<()> {
                $crate::chain_importer::ChainDataImporter::import(self, up_to_beacon).await
            }
        }
    };
}

/// Cardano chain data store
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait ChainDataStore: Send + Sync {
    /// Get the highest known transaction beacon
    async fn get_highest_beacon(&self) -> StdResult<Option<ChainPoint>>;

    /// Get the highest stored block range root bounds
    async fn get_highest_block_range(&self) -> StdResult<Option<BlockRange>>;

    /// Store list of transactions
    async fn store_transactions(&self, transactions: Vec<CardanoTransaction>) -> StdResult<()>;

    /// Get transactions in an interval of blocks
    async fn get_transactions_in_range(
        &self,
        range: Range<BlockNumber>,
    ) -> StdResult<Vec<CardanoTransaction>>;

    /// Store list of block ranges with their corresponding merkle root
    async fn store_block_range_roots(
        &self,
        block_ranges: Vec<(BlockRange, MKTreeNode)>,
    ) -> StdResult<()>;

    /// Remove transactions and block range roots that are in a rolled-back fork
    ///
    /// * Remove transactions with slot number strictly greater than the given slot number
    /// * Remove block range roots that have lower bound range strictly above the given slot number
    async fn remove_rolled_back_transactions_and_block_range(
        &self,
        slot_number: SlotNumber,
    ) -> StdResult<()>;
}
