use std::ops::Range;

use tokio::sync::Mutex;

use mithril_common::StdResult;
use mithril_common::crypto_helper::MKTreeNode;
use mithril_common::entities::{
    BlockNumber, BlockRange, CardanoTransaction, ChainPoint, SlotNumber,
};

use crate::chain_importer::ChainDataStore;

/// In memory Block range root representation, for testing purposes.
#[derive(Debug, PartialEq, Clone)]
pub struct InMemoryBlockRangeRoot {
    /// Range of block numbers covered
    pub range: BlockRange,
    /// Merkle root of the block range
    pub merkle_root: MKTreeNode,
}

impl From<(BlockRange, MKTreeNode)> for InMemoryBlockRangeRoot {
    fn from(value: (BlockRange, MKTreeNode)) -> Self {
        Self {
            range: value.0,
            merkle_root: value.1,
        }
    }
}

impl From<InMemoryBlockRangeRoot> for (BlockRange, MKTreeNode) {
    fn from(value: InMemoryBlockRangeRoot) -> Self {
        (value.range, value.merkle_root)
    }
}

/// In memory implementation of [ChainDataStore], for testing purposes.
pub struct InMemoryChainDataStore {
    transactions: Mutex<Vec<CardanoTransaction>>,
    block_range_roots: Mutex<Vec<InMemoryBlockRangeRoot>>,
}

impl Default for InMemoryChainDataStore {
    fn default() -> Self {
        Self::builder().build()
    }
}

/// Builder for [InMemoryChainDataStore].
pub struct InMemoryChainDataStoreBuilder {
    transactions: Vec<CardanoTransaction>,
    block_range_roots: Vec<InMemoryBlockRangeRoot>,
}

impl InMemoryChainDataStoreBuilder {
    /// Set the initial transactions for the store.
    pub fn with_transactions(mut self, transactions: &[CardanoTransaction]) -> Self {
        self.transactions = transactions.to_vec();
        self
    }

    /// Set the initial block range roots for the store.
    pub fn with_block_range_roots<T: Into<InMemoryBlockRangeRoot> + Clone>(
        mut self,
        block_range_roots: &[T],
    ) -> Self {
        self.block_range_roots = block_range_roots.iter().map(|brr| brr.clone().into()).collect();
        self
    }

    /// Creates a new [InMemoryChainDataStore] with the current builder's data.
    pub fn build(self) -> InMemoryChainDataStore {
        InMemoryChainDataStore {
            transactions: Mutex::new(self.transactions),
            block_range_roots: Mutex::new(self.block_range_roots),
        }
    }
}

impl InMemoryChainDataStore {
    /// Creates a new in-memory store builder with no initial data.
    pub fn builder() -> InMemoryChainDataStoreBuilder {
        InMemoryChainDataStoreBuilder {
            transactions: vec![],
            block_range_roots: vec![],
        }
    }

    /// Returns all transactions in the store.
    pub async fn get_all_transactions(&self) -> Vec<CardanoTransaction> {
        self.transactions.lock().await.clone()
    }

    /// Returns all block range roots in the store.
    pub async fn get_all_block_range_root(&self) -> Vec<InMemoryBlockRangeRoot> {
        self.block_range_roots.lock().await.clone()
    }

    /// Returns all block ranges in the store.
    pub async fn get_all_block_range(&self) -> Vec<BlockRange> {
        self.block_range_roots
            .lock()
            .await
            .iter()
            .map(|r| r.range.clone())
            .collect()
    }
}

#[async_trait::async_trait]
impl ChainDataStore for InMemoryChainDataStore {
    async fn get_highest_beacon(&self) -> StdResult<Option<ChainPoint>> {
        let txs = self.transactions.lock().await;
        Ok(txs
            .iter()
            .max_by_key(|tx| tx.block_number)
            .map(|tx| ChainPoint::new(tx.slot_number, tx.block_number, tx.block_hash.clone())))
    }

    async fn get_highest_block_range(&self) -> StdResult<Option<BlockRange>> {
        let roots = self.block_range_roots.lock().await;
        Ok(roots.iter().map(|record| record.range.clone()).max_by_key(|r| r.end))
    }

    async fn store_transactions(&self, transactions: Vec<CardanoTransaction>) -> StdResult<()> {
        self.transactions.lock().await.extend(transactions);
        Ok(())
    }

    async fn get_transactions_in_range(
        &self,
        range: Range<BlockNumber>,
    ) -> StdResult<Vec<CardanoTransaction>> {
        let txs = self.transactions.lock().await;
        Ok(txs
            .iter()
            .filter(|tx| range.contains(&tx.block_number))
            .cloned()
            .collect())
    }

    async fn store_block_range_roots(
        &self,
        block_ranges: Vec<(BlockRange, MKTreeNode)>,
    ) -> StdResult<()> {
        self.block_range_roots
            .lock()
            .await
            .extend(block_ranges.into_iter().map(Into::into));
        Ok(())
    }

    async fn remove_rolled_back_transactions_and_block_range(
        &self,
        slot_number: SlotNumber,
    ) -> StdResult<()> {
        self.transactions
            .lock()
            .await
            .retain(|tx| tx.slot_number <= slot_number);

        if let Some(highest_remaining_block_number) =
            self.transactions.lock().await.last().map(|tx| tx.block_number)
        {
            self.block_range_roots
                .lock()
                .await
                .retain(|record| record.range.start < highest_remaining_block_number);
        } else {
            self.block_range_roots.lock().await.clear();
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn default_store_is_empty() {
        let store = InMemoryChainDataStore::default();

        assert!(store.get_all_transactions().await.is_empty());
        assert!(store.get_all_block_range_root().await.is_empty());
        assert!(store.get_all_block_range().await.is_empty());
    }

    #[tokio::test]
    async fn store_and_get_transactions() {
        let store = InMemoryChainDataStore::default();

        let transactions = vec![
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
        ];

        store.store_transactions(transactions.clone()).await.unwrap();

        let stored_transactions = store.get_all_transactions().await;
        assert_eq!(transactions, stored_transactions);
    }

    #[tokio::test]
    async fn store_transactions_appends_to_existing() {
        let existing_batch = vec![CardanoTransaction::new(
            "tx_hash-123",
            BlockNumber(10),
            SlotNumber(50),
            "block_hash-123",
        )];

        let store = InMemoryChainDataStore::builder()
            .with_transactions(&existing_batch)
            .build();

        let second_batch = vec![CardanoTransaction::new(
            "tx_hash-456",
            BlockNumber(11),
            SlotNumber(51),
            "block_hash-456",
        )];

        store.store_transactions(second_batch.clone()).await.unwrap();

        let stored_transactions = store.get_all_transactions().await;
        assert_eq!(2, stored_transactions.len());
        assert_eq!(existing_batch[0], stored_transactions[0]);
        assert_eq!(second_batch[0], stored_transactions[1]);
    }

    #[tokio::test]
    async fn get_highest_beacon_returns_none_when_empty() {
        let store = InMemoryChainDataStore::default();

        let highest_beacon = store.get_highest_beacon().await.unwrap();

        assert_eq!(None, highest_beacon);
    }

    #[tokio::test]
    async fn get_highest_beacon_returns_transaction_with_highest_block_number() {
        let store = InMemoryChainDataStore::builder()
            .with_transactions(&[
                CardanoTransaction::new(
                    "tx_hash-123",
                    BlockNumber(10),
                    SlotNumber(50),
                    "block_hash-10",
                ),
                CardanoTransaction::new(
                    "tx_hash-456",
                    BlockNumber(25),
                    SlotNumber(51),
                    "block_hash-25",
                ),
                CardanoTransaction::new(
                    "tx_hash-789",
                    BlockNumber(15),
                    SlotNumber(52),
                    "block_hash-15",
                ),
            ])
            .build();

        let highest_beacon = store.get_highest_beacon().await.unwrap();

        assert_eq!(
            Some(ChainPoint::new(
                SlotNumber(51),
                BlockNumber(25),
                "block_hash-25"
            )),
            highest_beacon
        );
    }

    #[tokio::test]
    async fn get_highest_beacon_with_multiple_transactions_same_block_number() {
        let store = InMemoryChainDataStore::builder()
            .with_transactions(&[
                CardanoTransaction::new(
                    "tx_hash-123",
                    BlockNumber(10),
                    SlotNumber(50),
                    "block_hash-10",
                ),
                CardanoTransaction::new(
                    "tx_hash-456",
                    BlockNumber(25),
                    SlotNumber(51),
                    "block_hash-25",
                ),
                CardanoTransaction::new(
                    "tx_hash-789",
                    BlockNumber(25),
                    SlotNumber(51),
                    "block_hash-25",
                ),
            ])
            .build();

        let highest_beacon = store.get_highest_beacon().await.unwrap();

        assert_eq!(
            Some(ChainPoint::new(
                SlotNumber(51),
                BlockNumber(25),
                "block_hash-25"
            )),
            highest_beacon
        );
    }

    #[tokio::test]
    async fn store_and_get_block_range_roots() {
        let store = InMemoryChainDataStore::default();

        let block_ranges = vec![
            (
                BlockRange::from_block_number(BlockNumber(0)),
                MKTreeNode::from_hex("AAAA").unwrap(),
            ),
            (
                BlockRange::from_block_number(BlockRange::LENGTH),
                MKTreeNode::from_hex("BBBB").unwrap(),
            ),
        ];

        store.store_block_range_roots(block_ranges.clone()).await.unwrap();

        let stored_roots = store.get_all_block_range_root().await;
        assert_eq!(
            vec![
                InMemoryBlockRangeRoot {
                    range: BlockRange::from_block_number(BlockNumber(0)),
                    merkle_root: MKTreeNode::from_hex("AAAA").unwrap(),
                },
                InMemoryBlockRangeRoot {
                    range: BlockRange::from_block_number(BlockRange::LENGTH),
                    merkle_root: MKTreeNode::from_hex("BBBB").unwrap(),
                },
            ],
            stored_roots
        );
    }

    #[tokio::test]
    async fn store_block_range_roots_appends_to_existing() {
        let store = InMemoryChainDataStore::builder()
            .with_block_range_roots(&[(
                BlockRange::from_block_number(BlockNumber(0)),
                MKTreeNode::from_hex("AAAA").unwrap(),
            )])
            .build();

        store
            .store_block_range_roots(vec![(
                BlockRange::from_block_number(BlockRange::LENGTH),
                MKTreeNode::from_hex("BBBB").unwrap(),
            )])
            .await
            .unwrap();

        let stored_roots = store.get_all_block_range_root().await;
        assert_eq!(
            vec![
                InMemoryBlockRangeRoot {
                    range: BlockRange::from_block_number(BlockNumber(0)),
                    merkle_root: MKTreeNode::from_hex("AAAA").unwrap()
                },
                InMemoryBlockRangeRoot {
                    range: BlockRange::from_block_number(BlockRange::LENGTH),
                    merkle_root: MKTreeNode::from_hex("BBBB").unwrap()
                },
            ],
            stored_roots
        );

        let ranges = store.get_all_block_range().await;
        assert_eq!(
            vec![
                BlockRange::from_block_number(BlockNumber(0)),
                BlockRange::from_block_number(BlockRange::LENGTH),
            ],
            ranges
        );
    }

    #[tokio::test]
    async fn get_highest_block_range_returns_none_when_empty() {
        let store = InMemoryChainDataStore::default();

        let highest_range = store.get_highest_block_range().await.unwrap();

        assert_eq!(None, highest_range);
    }

    #[tokio::test]
    async fn get_highest_block_range_returns_range_with_highest_end() {
        let store = InMemoryChainDataStore::builder()
            .with_block_range_roots(&[
                (
                    BlockRange::from_block_number(BlockNumber(0)),
                    MKTreeNode::from_hex("AAAA").unwrap(),
                ),
                (
                    BlockRange::from_block_number(BlockRange::LENGTH * 2),
                    MKTreeNode::from_hex("CCCC").unwrap(),
                ),
                (
                    BlockRange::from_block_number(BlockRange::LENGTH),
                    MKTreeNode::from_hex("BBBB").unwrap(),
                ),
            ])
            .build();

        let highest_range = store.get_highest_block_range().await.unwrap();

        assert_eq!(
            Some(BlockRange::from_block_number(BlockRange::LENGTH * 2)),
            highest_range
        );
    }

    #[tokio::test]
    async fn get_transactions_in_range_returns_empty_when_no_transactions() {
        let store = InMemoryChainDataStore::default();

        let transactions = store
            .get_transactions_in_range(BlockNumber(0)..BlockNumber(100))
            .await
            .unwrap();

        assert!(transactions.is_empty());
    }

    #[tokio::test]
    async fn get_transactions_in_range_filters_correctly() {
        let transactions = vec![
            CardanoTransaction::new("tx-hash-1", BlockNumber(10), SlotNumber(50), "block-hash-1"),
            CardanoTransaction::new("tx-hash-2", BlockNumber(11), SlotNumber(51), "block-hash-2"),
            CardanoTransaction::new("tx-hash-3", BlockNumber(12), SlotNumber(52), "block-hash-3"),
        ];
        let store = InMemoryChainDataStore::builder()
            .with_transactions(&transactions)
            .build();

        // Range excludes all transactions
        {
            let result = store
                .get_transactions_in_range(BlockNumber(0)..BlockNumber(10))
                .await
                .unwrap();
            assert!(result.is_empty());
        }

        // Range after all transactions
        {
            let result = store
                .get_transactions_in_range(BlockNumber(13)..BlockNumber(21))
                .await
                .unwrap();
            assert!(result.is_empty());
        }

        // Range includes the first two transactions (10, 11)
        {
            let result = store
                .get_transactions_in_range(BlockNumber(9)..BlockNumber(12))
                .await
                .unwrap();
            assert_eq!(transactions[0..=1].to_vec(), result);
        }

        // Range includes all transactions
        {
            let result = store
                .get_transactions_in_range(BlockNumber(10)..BlockNumber(13))
                .await
                .unwrap();
            assert_eq!(transactions, result);
        }

        // Range includes the last two transactions (11, 12)
        {
            let result = store
                .get_transactions_in_range(BlockNumber(11)..BlockNumber(14))
                .await
                .unwrap();
            assert_eq!(transactions[1..=2].to_vec(), result);
        }
    }

    #[tokio::test]
    async fn remove_rolled_back_transactions_and_block_range_removes_transactions_above_slot_number()
     {
        let transactions = vec![
            CardanoTransaction::new("tx-hash-1", BlockNumber(10), SlotNumber(50), "block-hash-1"),
            CardanoTransaction::new("tx-hash-2", BlockNumber(11), SlotNumber(51), "block-hash-2"),
            CardanoTransaction::new("tx-hash-3", BlockNumber(12), SlotNumber(52), "block-hash-3"),
        ];
        let store = InMemoryChainDataStore::builder()
            .with_transactions(&transactions)
            .build();

        store
            .remove_rolled_back_transactions_and_block_range(SlotNumber(51))
            .await
            .unwrap();

        let remaining = store.get_all_transactions().await;
        assert_eq!(transactions[0..=1].to_vec(), remaining);
    }

    #[tokio::test]
    async fn remove_rolled_back_transactions_and_block_range_removes_block_ranges_above_highest_remaining_block()
     {
        let transactions = vec![
            CardanoTransaction::new("tx-hash-1", BlockNumber(10), SlotNumber(50), "block-hash-1"),
            CardanoTransaction::new(
                "tx-hash-2",
                BlockRange::LENGTH * 2,
                SlotNumber(100),
                "block-hash-2",
            ),
            CardanoTransaction::new(
                "tx-hash-3",
                BlockRange::LENGTH * 4,
                SlotNumber(200),
                "block-hash-3",
            ),
        ];
        let block_ranges_roots = vec![
            (
                BlockRange::from_block_number(BlockNumber(0)),
                MKTreeNode::from_hex("AAAA").unwrap(),
            ),
            (
                BlockRange::from_block_number(BlockRange::LENGTH),
                MKTreeNode::from_hex("BBBB").unwrap(),
            ),
            (
                BlockRange::from_block_number(BlockRange::LENGTH * 2),
                MKTreeNode::from_hex("CCCC").unwrap(),
            ),
            (
                BlockRange::from_block_number(BlockRange::LENGTH * 3),
                MKTreeNode::from_hex("DDDD").unwrap(),
            ),
        ];
        let store = InMemoryChainDataStore::builder()
            .with_transactions(&transactions)
            .with_block_range_roots(&block_ranges_roots)
            .build();

        // Rollback to slot 100 (keeps transactions with slot <= 100)
        store
            .remove_rolled_back_transactions_and_block_range(SlotNumber(100))
            .await
            .unwrap();

        let remaining_transactions = store.get_all_transactions().await;
        assert_eq!(2, remaining_transactions.len());

        // Block ranges with start < highest remaining block number (BlockRange::LENGTH * 2) should remain
        let remaining_ranges = store.get_all_block_range().await;
        assert_eq!(
            vec![
                BlockRange::from_block_number(BlockNumber(0)),
                BlockRange::from_block_number(BlockRange::LENGTH),
            ],
            remaining_ranges
        );
    }

    #[tokio::test]
    async fn remove_rolled_back_transactions_and_block_range_with_no_remaining_transactions() {
        let store = InMemoryChainDataStore::builder()
            .with_transactions(&[CardanoTransaction::new(
                "tx-hash-1",
                BlockNumber(10),
                SlotNumber(50),
                "block-hash-1",
            )])
            .with_block_range_roots(&[(
                BlockRange::from_block_number(BlockNumber(0)),
                MKTreeNode::from_hex("AAAA").unwrap(),
            )])
            .build();

        // Rollback to slot before all transactions
        store
            .remove_rolled_back_transactions_and_block_range(SlotNumber(40))
            .await
            .unwrap();

        assert!(store.get_all_transactions().await.is_empty());
        assert!(store.get_all_block_range_root().await.is_empty());
    }

    #[tokio::test]
    async fn remove_rolled_back_transactions_and_block_range_keeps_transactions_with_equal_slot_number()
     {
        let transactions = vec![
            CardanoTransaction::new("tx-hash-1", BlockNumber(10), SlotNumber(50), "block-hash-1"),
            CardanoTransaction::new("tx-hash-2", BlockNumber(11), SlotNumber(50), "block-hash-1"),
            CardanoTransaction::new("tx-hash-3", BlockNumber(12), SlotNumber(51), "block-hash-2"),
        ];
        let store = InMemoryChainDataStore::builder()
            .with_transactions(&transactions)
            .build();

        store
            .remove_rolled_back_transactions_and_block_range(SlotNumber(50))
            .await
            .unwrap();

        let remaining = store.get_all_transactions().await;
        assert_eq!(transactions[0..=1].to_vec(), remaining);
    }
}
