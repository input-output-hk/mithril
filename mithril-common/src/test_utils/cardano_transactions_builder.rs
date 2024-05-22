use crate::entities::{BlockRange, CardanoTransaction};

/// Builder to easily build transactions with consistent values.
///
/// Note: the names generated for the transaction hashes and block hashes are not meaningful.
///
/// # Example 'build_transactions'
///
/// ```
///     use mithril_common::entities::CardanoTransaction;
///     use mithril_common::test_utils::CardanoTransactionsBuilder;
///
///     let txs = CardanoTransactionsBuilder::new()
///         .max_transactions_per_block(3)
///         .blocks_per_block_range(2)
///         .build_transactions(8);
///
///     assert_eq!(8, txs.len());
///     assert_eq!(
///         vec![
///             CardanoTransaction::new("tx-hash-0-100", 0, 100, "block-hash-0", 1),
///             CardanoTransaction::new("tx-hash-0-101", 0, 101, "block-hash-0", 2),
///             CardanoTransaction::new("tx-hash-0-102", 0, 102, "block-hash-0", 3),
///             CardanoTransaction::new("tx-hash-1-103", 1, 103, "block-hash-1", 4),
///             CardanoTransaction::new("tx-hash-1-104", 1, 104, "block-hash-1", 5),
///             CardanoTransaction::new("tx-hash-1-105", 1, 105, "block-hash-1", 6),
///             CardanoTransaction::new("tx-hash-15-106", 15, 106, "block-hash-15", 7),
///             CardanoTransaction::new("tx-hash-15-107", 15, 107, "block-hash-15", 8)
///         ],
///         txs
///     );
/// ```
///
/// # Example 'build_block_ranges'
///
/// ```
///     use mithril_common::entities::CardanoTransaction;
///     use mithril_common::test_utils::CardanoTransactionsBuilder;
///
///     let txs = CardanoTransactionsBuilder::new()
///         .max_transactions_per_block(3)
///         .blocks_per_block_range(2)
///         .build_block_ranges(2);
///
///     assert_eq!(3 * 2 * 2, txs.len());
///     assert_eq!(
///         vec![
///             CardanoTransaction::new("tx-hash-0-100", 0, 100, "block-hash-0", 1),
///             CardanoTransaction::new("tx-hash-0-101", 0, 101, "block-hash-0", 2),
///             CardanoTransaction::new("tx-hash-0-102", 0, 102, "block-hash-0", 3),
///             CardanoTransaction::new("tx-hash-1-103", 1, 103, "block-hash-1", 4),
///             CardanoTransaction::new("tx-hash-1-104", 1, 104, "block-hash-1", 5),
///             CardanoTransaction::new("tx-hash-1-105", 1, 105, "block-hash-1", 6),
///             CardanoTransaction::new("tx-hash-15-106", 15, 106, "block-hash-15", 7),
///             CardanoTransaction::new("tx-hash-15-107", 15, 107, "block-hash-15", 8),
///             CardanoTransaction::new("tx-hash-15-108", 15, 108, "block-hash-15", 9),
///             CardanoTransaction::new("tx-hash-16-109", 16, 109, "block-hash-16", 10),
///             CardanoTransaction::new("tx-hash-16-110", 16, 110, "block-hash-16", 11),
///             CardanoTransaction::new("tx-hash-16-111", 16, 111, "block-hash-16", 12),
///         ],
///         txs
///     );
/// ```
pub struct CardanoTransactionsBuilder {
    max_transactions_per_block: usize,
    max_blocks_per_block_range: usize,
    max_transactions_per_immutable_file: usize,
    first_immutable_file: u64,
}

impl Default for CardanoTransactionsBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl CardanoTransactionsBuilder {
    /// [CardanoTransactionsBuilder] constructor.
    pub fn new() -> Self {
        Self {
            max_transactions_per_block: 1,
            max_blocks_per_block_range: 1,
            max_transactions_per_immutable_file: 1,
            first_immutable_file: 1,
        }
    }

    /// Define how many transactions we generate in each block.
    pub fn max_transactions_per_block(mut self, transactions_per_block: usize) -> Self {
        self.max_transactions_per_block = transactions_per_block;
        self
    }

    /// Define how many transactions we generate for one immutable_file.
    pub fn max_transactions_per_immutable_file(
        mut self,
        transactions_per_immutable_file: usize,
    ) -> Self {
        self.max_transactions_per_immutable_file = transactions_per_immutable_file;
        self
    }

    /// Define the first immutable file number.
    pub fn first_immutable_file(mut self, first_immutable_file: u64) -> Self {
        self.first_immutable_file = first_immutable_file;
        self
    }

    /// Define how many blocks we generate in each block_range.
    /// If we set too many blocks for a block_range, this function panic.
    pub fn blocks_per_block_range(mut self, blocks_per_block_range: usize) -> Self {
        if blocks_per_block_range > BlockRange::LENGTH as usize {
            panic!(
                "blocks_per_block_range should be less than {}",
                BlockRange::LENGTH
            );
        }
        self.max_blocks_per_block_range = blocks_per_block_range;
        self
    }

    /// Build the number of transactions requested.
    pub fn build_transactions(self, transactions_count: usize) -> Vec<CardanoTransaction> {
        let mut transactions = Vec::new();
        let first_transaction_number = 100;
        for tx_index in 0..transactions_count {
            let block_number = self.block_number_from_transaction_index(tx_index);
            let immutable_file_number = tx_index as u64
                / self.max_transactions_per_immutable_file as u64
                + self.first_immutable_file;
            transactions.push(self.create_transaction(
                tx_index as u64 + first_transaction_number,
                block_number,
                immutable_file_number,
            ))
        }

        transactions
    }

    /// Build a list of transactions to get the number of block range requested.
    pub fn build_block_ranges(self, block_ranges_count: usize) -> Vec<CardanoTransaction> {
        let nb_txs =
            block_ranges_count * self.max_blocks_per_block_range * self.max_transactions_per_block;

        self.build_transactions(nb_txs)
    }

    fn block_number_from_transaction_index(&self, tx_index: usize) -> u64 {
        let max_transactions_per_block_range =
            self.max_transactions_per_block * self.max_blocks_per_block_range;
        let index_block_range = tx_index / max_transactions_per_block_range;
        let block_index_global = tx_index as u64 / self.max_transactions_per_block as u64;
        let block_index_in_block_range =
            block_index_global % self.max_blocks_per_block_range as u64;

        index_block_range as u64 * BlockRange::LENGTH + block_index_in_block_range
    }

    /// Create a transaction with a given index and block number.
    fn create_transaction(
        &self,
        transaction_id: u64,
        block_number: u64,
        immutable_file_number: u64,
    ) -> CardanoTransaction {
        CardanoTransaction::new(
            format!("tx-hash-{}-{}", block_number, transaction_id),
            block_number,
            transaction_id,
            format!("block-hash-{block_number}"),
            immutable_file_number,
        )
    }
}

#[cfg(test)]
mod test {
    use std::collections::{HashMap, HashSet};

    use super::*;

    fn count_distinct_values<T, R>(list: &[T], extract_value: &dyn Fn(&T) -> R) -> usize
    where
        R: Eq + std::hash::Hash,
    {
        list.iter().map(extract_value).collect::<HashSet<R>>().len()
    }

    fn group_by<'a, T, R>(list: &'a [T], extract_value: &dyn Fn(&T) -> R) -> HashMap<R, Vec<&'a T>>
    where
        R: Eq + std::hash::Hash,
    {
        let mut grouped_by_block = HashMap::new();
        for t in list {
            grouped_by_block
                .entry(extract_value(t))
                .or_insert(Vec::new())
                .push(t);
        }
        grouped_by_block
    }

    fn extract_by<T, R>(list: &[T], extract_value: &dyn Fn(&T) -> R) -> Vec<R> {
        list.iter().map(extract_value).collect()
    }

    #[test]
    fn return_given_number_of_transactions_with_distinct_values() {
        let txs = CardanoTransactionsBuilder::new().build_transactions(3);

        assert_eq!(txs.len(), 3);

        assert_eq!(
            3,
            count_distinct_values(&txs, &|t| t.transaction_hash.clone())
        );
        assert_eq!(3, count_distinct_values(&txs, &|t| t.block_number));
        assert_eq!(3, count_distinct_values(&txs, &|t| t.slot_number));
        assert_eq!(3, count_distinct_values(&txs, &|t| t.block_hash.clone()));
        assert_eq!(3, count_distinct_values(&txs, &|t| t.immutable_file_number));
    }

    #[test]
    fn return_all_transactions_in_same_block_when_ask_less_transactions_than_transactions_per_block(
    ) {
        let txs = CardanoTransactionsBuilder::new()
            .max_transactions_per_block(10)
            .build_transactions(3);

        assert_eq!(txs.len(), 3);

        assert_eq!(
            3,
            count_distinct_values(&txs, &|t| t.transaction_hash.clone())
        );
        assert_eq!(1, count_distinct_values(&txs, &|t| t.block_number));
        assert_eq!(1, count_distinct_values(&txs, &|t| t.block_hash.clone()));
    }

    #[test]
    fn return_no_more_transactions_in_a_same_block_than_number_per_block_requested() {
        let txs = CardanoTransactionsBuilder::new()
            .max_transactions_per_block(3)
            .build_transactions(12);

        assert_eq!(txs.len(), 12);

        assert_eq!(
            12,
            count_distinct_values(&txs, &|t| t.transaction_hash.clone())
        );
        assert_eq!(4, count_distinct_values(&txs, &|t| t.block_number));
        assert_eq!(4, count_distinct_values(&txs, &|t| t.block_hash.clone()));
    }

    #[test]
    fn only_the_last_block_is_not_full_when_we_can_not_fill_all_blocks() {
        let txs = CardanoTransactionsBuilder::new()
            .max_transactions_per_block(5)
            .build_transactions(12);

        assert_eq!(txs.len(), 12);

        assert_eq!(
            12,
            count_distinct_values(&txs, &|t| t.transaction_hash.clone())
        );
        assert_eq!(3, count_distinct_values(&txs, &|t| t.block_number));

        let grouped_by_block = group_by(&txs, &|t| t.block_number);
        let mut txs_per_block: Vec<_> = grouped_by_block.values().map(|v| v.len()).collect();
        txs_per_block.sort();
        assert_eq!(vec![2, 5, 5], txs_per_block);
    }

    #[test]
    fn generate_one_block_range_return_one_transaction_by_default() {
        let txs = CardanoTransactionsBuilder::new().build_block_ranges(1);
        assert_eq!(txs.len(), 1);
    }

    #[test]
    fn build_block_ranges_return_the_number_of_block_ranges_requested() {
        let block_ranges = 3;
        let txs = CardanoTransactionsBuilder::new().build_block_ranges(block_ranges);

        assert_eq!(txs.len(), 3);

        assert_eq!(
            3,
            count_distinct_values(&txs, &|t| BlockRange::start(t.block_number))
        );
    }

    #[test]
    fn build_block_ranges_return_many_transactions_per_block_when_requested() {
        let txs = CardanoTransactionsBuilder::new()
            .max_transactions_per_block(5)
            .build_block_ranges(3);

        assert_eq!(txs.len(), 3 * 5);

        assert_eq!(
            3 * 5,
            count_distinct_values(&txs, &|t| t.transaction_hash.clone())
        );

        assert_eq!(
            3,
            count_distinct_values(&txs, &|t| BlockRange::start(t.block_number))
        );
        assert_eq!(3, count_distinct_values(&txs, &|t| t.block_number));
        assert_eq!(3, count_distinct_values(&txs, &|t| t.block_hash.clone()));
    }

    #[test]
    fn build_block_ranges_with_many_blocks_per_block_ranges() {
        let txs = CardanoTransactionsBuilder::new()
            .max_transactions_per_block(5)
            .blocks_per_block_range(2)
            .build_block_ranges(3);

        assert_eq!(txs.len(), 3 * 2 * 5);

        assert_eq!(
            3 * 2 * 5,
            count_distinct_values(&txs, &|t| t.transaction_hash.clone())
        );

        assert_eq!(
            3,
            count_distinct_values(&txs, &|t| BlockRange::start(t.block_number))
        );
        assert_eq!(3 * 2, count_distinct_values(&txs, &|t| t.block_number));
        assert_eq!(
            3 * 2,
            count_distinct_values(&txs, &|t| t.block_hash.clone())
        );
    }

    #[test]
    fn build_transactions_with_many_blocks_per_block_ranges() {
        let txs = CardanoTransactionsBuilder::new()
            .max_transactions_per_block(5)
            .blocks_per_block_range(2)
            .build_transactions(18);

        // block range 1 - block 0  - 1, 2, 3, 4, 5
        // block range 1 - block 1  - 6, 7, 8, 7, 10
        // block range 2 - block 15 - 11, 12, 13, 14, 15
        // block range 2 - block 16 - 16, 17, 18

        assert_eq!(txs.len(), 18);

        assert_eq!(
            18,
            count_distinct_values(&txs, &|t| t.transaction_hash.clone())
        );

        assert_eq!(
            2,
            count_distinct_values(&txs, &|t| BlockRange::start(t.block_number))
        );
        assert_eq!(4, count_distinct_values(&txs, &|t| t.block_number));
        assert_eq!(4, count_distinct_values(&txs, &|t| t.block_hash.clone()));
    }

    #[test]
    #[should_panic]
    fn should_panic_when_too_many_blocks_per_block_range() {
        CardanoTransactionsBuilder::new().blocks_per_block_range(BlockRange::LENGTH as usize + 1);
    }

    #[test]
    fn build_transactions_with_many_transactions_per_immutable_file() {
        let transactions = CardanoTransactionsBuilder::new()
            .max_transactions_per_immutable_file(5)
            .build_transactions(18);

        assert_eq!(transactions.len(), 18);

        assert_eq!(
            4,
            count_distinct_values(&transactions, &|t| t.immutable_file_number)
        );
    }

    #[test]
    fn build_transactions_with_same_number_of_transactions_per_immutable_file() {
        let transactions = CardanoTransactionsBuilder::new()
            .max_transactions_per_immutable_file(5)
            .build_transactions(20);

        assert_eq!(transactions.len(), 20);

        let grouped_by_immutable_file = group_by(&transactions, &|t| t.immutable_file_number);
        assert_eq!(4, grouped_by_immutable_file.len());

        for transaction_for_immutable_file in grouped_by_immutable_file.values() {
            assert_eq!(5, transaction_for_immutable_file.len());
        }
    }

    #[test]
    fn build_transactions_with_immutable_file_starting_at_a_specific_number() {
        let transactions = CardanoTransactionsBuilder::new()
            .first_immutable_file(5)
            .build_transactions(3);

        assert_eq!(
            vec![5, 6, 7],
            extract_by(&transactions, &|t| t.immutable_file_number)
        );
    }
}
