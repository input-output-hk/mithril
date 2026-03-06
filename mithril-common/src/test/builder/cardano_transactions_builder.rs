use crate::entities::{
    BlockNumber, BlockRange, CardanoBlockWithTransactions, CardanoTransaction, SlotNumber,
};

/// Builder to easily build transactions with consistent values.
///
/// Note: the names generated for the transaction hashes and block hashes are not meaningful.
///
/// # Example 'build_transactions'
///
/// ```
///     use mithril_common::entities::{BlockNumber, CardanoTransaction, SlotNumber};
///     use mithril_common::test::builder::CardanoTransactionsBuilder;
///
///     let txs = CardanoTransactionsBuilder::new()
///         .max_transactions_per_block(3)
///         .blocks_per_block_range(2)
///         .build_transactions(8);
///
///     assert_eq!(8, txs.len());
///     assert_eq!(
///         vec![
///             CardanoTransaction::new("tx-hash-0-100", BlockNumber(0), SlotNumber(100), "block-hash-0"),
///             CardanoTransaction::new("tx-hash-0-101", BlockNumber(0), SlotNumber(100), "block-hash-0"),
///             CardanoTransaction::new("tx-hash-0-102", BlockNumber(0), SlotNumber(100), "block-hash-0"),
///             CardanoTransaction::new("tx-hash-1-103", BlockNumber(1), SlotNumber(101), "block-hash-1"),
///             CardanoTransaction::new("tx-hash-1-104", BlockNumber(1), SlotNumber(101), "block-hash-1"),
///             CardanoTransaction::new("tx-hash-1-105", BlockNumber(1), SlotNumber(101), "block-hash-1"),
///             CardanoTransaction::new("tx-hash-15-106", BlockNumber(15), SlotNumber(115), "block-hash-15"),
///             CardanoTransaction::new("tx-hash-15-107", BlockNumber(15), SlotNumber(115), "block-hash-15")
///         ],
///         txs
///     );
/// ```
///
/// # Example 'build_transactions_for_block_ranges'
///
/// ```
///     use mithril_common::entities::{BlockNumber, CardanoTransaction, SlotNumber};
///     use mithril_common::test::builder::CardanoTransactionsBuilder;
///
///     let txs = CardanoTransactionsBuilder::new()
///         .max_transactions_per_block(3)
///         .blocks_per_block_range(2)
///         .build_transactions_for_block_ranges(2);
///
///     assert_eq!(3 * 2 * 2, txs.len());
///     assert_eq!(
///         vec![
///             CardanoTransaction::new("tx-hash-0-100", BlockNumber(0), SlotNumber(100), "block-hash-0"),
///             CardanoTransaction::new("tx-hash-0-101", BlockNumber(0), SlotNumber(100), "block-hash-0"),
///             CardanoTransaction::new("tx-hash-0-102", BlockNumber(0), SlotNumber(100), "block-hash-0"),
///             CardanoTransaction::new("tx-hash-1-103", BlockNumber(1), SlotNumber(101), "block-hash-1"),
///             CardanoTransaction::new("tx-hash-1-104", BlockNumber(1), SlotNumber(101), "block-hash-1"),
///             CardanoTransaction::new("tx-hash-1-105", BlockNumber(1), SlotNumber(101), "block-hash-1"),
///             CardanoTransaction::new("tx-hash-15-106", BlockNumber(15), SlotNumber(115), "block-hash-15"),
///             CardanoTransaction::new("tx-hash-15-107", BlockNumber(15), SlotNumber(115), "block-hash-15"),
///             CardanoTransaction::new("tx-hash-15-108", BlockNumber(15), SlotNumber(115), "block-hash-15"),
///             CardanoTransaction::new("tx-hash-16-109", BlockNumber(16), SlotNumber(116), "block-hash-16"),
///             CardanoTransaction::new("tx-hash-16-110", BlockNumber(16), SlotNumber(116), "block-hash-16"),
///             CardanoTransaction::new("tx-hash-16-111", BlockNumber(16), SlotNumber(116), "block-hash-16"),
///         ],
///         txs
///     );
/// ```
pub struct CardanoTransactionsBuilder {
    max_transactions_per_block: usize,
    max_blocks_per_block_range: usize,
}

impl Default for CardanoTransactionsBuilder {
    fn default() -> Self {
        Self::new()
    }
}

enum FillUntil {
    BlockCountReached,
    TransactionCountReached,
}

impl CardanoTransactionsBuilder {
    const FIRST_TRANSACTION_NUMBER: usize = 100;
    /// Offset between a generated block block_number and its slot_number.
    const SLOT_NUMBER_OFFSET: u64 = 100;

    /// [CardanoTransactionsBuilder] constructor.
    pub fn new() -> Self {
        Self {
            max_transactions_per_block: 1,
            max_blocks_per_block_range: 1,
        }
    }

    /// Define how many transactions we generate in each block.
    pub fn max_transactions_per_block(mut self, transactions_per_block: usize) -> Self {
        self.max_transactions_per_block = transactions_per_block;
        self
    }

    /// Define how many blocks we generate in each block_range.
    /// If we set too many blocks for a block_range, this function panic.
    pub fn blocks_per_block_range(mut self, blocks_per_block_range: usize) -> Self {
        if blocks_per_block_range > *BlockRange::LENGTH as usize {
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
        let block_count = transactions_count / self.max_transactions_per_block;
        self.build_blocks_with_transactions(
            block_count,
            transactions_count,
            FillUntil::TransactionCountReached,
        )
        .into_iter()
        .flat_map(|b| b.into_transactions())
        .collect()
    }

    /// Build a list of transactions to get the number of block range requested.
    pub fn build_transactions_for_block_ranges(
        self,
        block_ranges_count: usize,
    ) -> Vec<CardanoTransaction> {
        let block_count = block_ranges_count * self.max_blocks_per_block_range;
        let nb_txs =
            block_ranges_count * self.max_blocks_per_block_range * self.max_transactions_per_block;

        self.build_blocks_with_transactions(block_count, nb_txs, FillUntil::TransactionCountReached)
            .into_iter()
            .flat_map(|b| b.into_transactions())
            .collect()
    }

    fn build_blocks_with_transactions(
        self,
        block_count: usize,
        transactions_count: usize,
        fill_behavior: FillUntil,
    ) -> Vec<CardanoBlockWithTransactions> {
        let mut blocks = Vec::with_capacity(block_count);
        let mut transactions_numbers: Vec<_> = (0..transactions_count)
            .map(|i| i + Self::FIRST_TRANSACTION_NUMBER)
            .collect();
        let mut block_number_offset = 0;
        let mut current_block_range_index = 0;

        while match fill_behavior {
            FillUntil::BlockCountReached => blocks.len() < block_count,
            FillUntil::TransactionCountReached => !transactions_numbers.is_empty(),
        } {
            let block_number = BlockNumber(current_block_range_index) + block_number_offset;
            let transactions_hashes: Vec<_> = transactions_numbers
                .drain(..self.max_transactions_per_block.min(transactions_numbers.len()))
                .map(|i| format!("tx-hash-{block_number}-{i}"))
                .collect();

            let block = CardanoBlockWithTransactions {
                block_hash: format!("block-hash-{block_number}"),
                block_number,
                slot_number: SlotNumber(*block_number + Self::SLOT_NUMBER_OFFSET),
                transactions_hashes,
            };
            blocks.push(block);

            current_block_range_index += 1;
            if (current_block_range_index as usize) == self.max_blocks_per_block_range {
                current_block_range_index = 0;
                block_number_offset += BlockRange::LENGTH;
            }
        }

        blocks
    }
}

#[cfg(test)]
mod test {
    use std::collections::{HashMap, HashSet};

    use super::*;

    fn count_distinct_values<T, R>(list: &[T], extract_value: &dyn Fn(T) -> R) -> usize
    where
        T: Clone,
        R: Eq + std::hash::Hash,
    {
        list.iter().cloned().map(extract_value).collect::<HashSet<R>>().len()
    }

    fn group_by<'a, T, R>(list: &'a [T], extract_value: &dyn Fn(T) -> R) -> HashMap<R, Vec<&'a T>>
    where
        T: Clone,
        R: Eq + std::hash::Hash,
    {
        let mut grouped_by_block = HashMap::new();
        for t in list {
            grouped_by_block
                .entry(extract_value(t.clone()))
                .or_insert(Vec::new())
                .push(t);
        }
        grouped_by_block
    }

    #[test]
    #[should_panic]
    fn should_panic_when_too_many_blocks_per_block_range() {
        CardanoTransactionsBuilder::new().blocks_per_block_range(*BlockRange::LENGTH as usize + 1);
    }

    mod build_transactions {
        use super::*;

        #[test]
        fn return_given_number_of_transactions_with_distinct_values() {
            let txs = CardanoTransactionsBuilder::new().build_transactions(3);

            assert_eq!(txs.len(), 3);

            assert_eq!(3, count_distinct_values(&txs, &|t| t.transaction_hash));
            assert_eq!(3, count_distinct_values(&txs, &|t| t.block_number));
            assert_eq!(3, count_distinct_values(&txs, &|t| t.slot_number));
            assert_eq!(3, count_distinct_values(&txs, &|t| t.block_hash));
        }

        #[test]
        fn return_all_transactions_in_same_block_when_ask_less_transactions_than_transactions_per_block()
         {
            let txs = CardanoTransactionsBuilder::new()
                .max_transactions_per_block(10)
                .build_transactions(3);

            assert_eq!(txs.len(), 3);

            assert_eq!(3, count_distinct_values(&txs, &|t| t.transaction_hash));
            assert_eq!(1, count_distinct_values(&txs, &|t| t.block_number));
            assert_eq!(1, count_distinct_values(&txs, &|t| t.block_hash));
        }

        #[test]
        fn return_no_more_transactions_in_a_same_block_than_number_per_block_requested() {
            let txs = CardanoTransactionsBuilder::new()
                .max_transactions_per_block(3)
                .build_transactions(12);

            assert_eq!(txs.len(), 12);

            assert_eq!(12, count_distinct_values(&txs, &|t| t.transaction_hash));
            assert_eq!(4, count_distinct_values(&txs, &|t| t.block_number));
            assert_eq!(4, count_distinct_values(&txs, &|t| t.block_hash));
        }

        #[test]
        fn only_the_last_block_is_not_full_when_we_can_not_fill_all_blocks() {
            let txs = CardanoTransactionsBuilder::new()
                .max_transactions_per_block(5)
                .build_transactions(12);

            assert_eq!(txs.len(), 12);

            assert_eq!(12, count_distinct_values(&txs, &|t| t.transaction_hash));
            assert_eq!(3, count_distinct_values(&txs, &|t| t.block_number));

            let grouped_by_block = group_by(&txs, &|t| t.block_number);
            let mut txs_per_block: Vec<_> = grouped_by_block.values().map(|v| v.len()).collect();
            txs_per_block.sort();
            assert_eq!(vec![2, 5, 5], txs_per_block);
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

            assert_eq!(18, count_distinct_values(&txs, &|t| t.transaction_hash));

            assert_eq!(
                2,
                count_distinct_values(&txs, &|t| BlockRange::start(t.block_number))
            );
            assert_eq!(4, count_distinct_values(&txs, &|t| t.block_number));
            assert_eq!(4, count_distinct_values(&txs, &|t| t.block_hash));
        }
    }

    mod build_transactions_for_block_ranges {
        use super::*;

        #[test]
        fn generate_transactions_for_one_block_range_return_one_transaction_by_default() {
            let txs = CardanoTransactionsBuilder::new().build_transactions_for_block_ranges(1);
            assert_eq!(txs.len(), 1);
        }

        #[test]
        fn build_transactions_for_block_ranges_return_the_number_of_block_ranges_requested() {
            let block_ranges = 3;
            let txs =
                CardanoTransactionsBuilder::new().build_transactions_for_block_ranges(block_ranges);

            assert_eq!(txs.len(), 3);

            assert_eq!(
                3,
                count_distinct_values(&txs, &|t| BlockRange::start(t.block_number))
            );
        }

        #[test]
        fn build_transactions_for_block_ranges_return_many_transactions_per_block_when_requested() {
            let txs = CardanoTransactionsBuilder::new()
                .max_transactions_per_block(5)
                .build_transactions_for_block_ranges(3);

            assert_eq!(txs.len(), 3 * 5);

            assert_eq!(3 * 5, count_distinct_values(&txs, &|t| t.transaction_hash));

            assert_eq!(
                3,
                count_distinct_values(&txs, &|t| BlockRange::start(t.block_number))
            );
            assert_eq!(3, count_distinct_values(&txs, &|t| t.block_number));
            assert_eq!(3, count_distinct_values(&txs, &|t| t.block_hash));
        }

        #[test]
        fn build_transactions_for_block_ranges_with_many_blocks_per_block_ranges() {
            let txs = CardanoTransactionsBuilder::new()
                .max_transactions_per_block(5)
                .blocks_per_block_range(2)
                .build_transactions_for_block_ranges(3);

            assert_eq!(txs.len(), 3 * 2 * 5);

            assert_eq!(
                3 * 2 * 5,
                count_distinct_values(&txs, &|t| t.transaction_hash)
            );

            assert_eq!(
                3,
                count_distinct_values(&txs, &|t| BlockRange::start(t.block_number))
            );
            assert_eq!(3 * 2, count_distinct_values(&txs, &|t| t.block_number));
            assert_eq!(3 * 2, count_distinct_values(&txs, &|t| t.block_hash));
        }
    }
}
