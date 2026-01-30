use mithril_common::entities::CardanoBlockWithTransactions;

use crate::database::record::{CardanoBlockRecord, StorableCardanoTransactionRecord};

/// Trait for converting types into their records representations
///
/// Private as it is only meant to be used for types where we can't implement conversion using standard
/// [From] trait (such as `Vec`).
pub(crate) trait IntoRecords {
    type Output;

    fn into_records(self) -> Self::Output;
}

impl IntoRecords for Vec<CardanoBlockWithTransactions> {
    type Output = (
        Vec<CardanoBlockRecord>,
        Vec<StorableCardanoTransactionRecord>,
    );

    fn into_records(self) -> Self::Output {
        let transactions_records = self
            .iter()
            .flat_map(|b| {
                b.transactions_hashes
                    .clone()
                    .into_iter()
                    .map(|h| StorableCardanoTransactionRecord::new(h, &b.block_hash))
            })
            .collect();
        let blocks_records = self.into_iter().map(Into::into).collect();

        (blocks_records, transactions_records)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{BlockNumber, SlotNumber};

    use super::*;

    #[test]
    fn explode_one_block_with_tx_to_records() {
        let input = vec![CardanoBlockWithTransactions::new(
            "block_hash_1",
            BlockNumber(32),
            SlotNumber(59),
            vec!["tx_hash_1", "tx_hash_2", "tx_hash_3"],
        )];

        let (blocks_records, transactions_records) = input.into_records();
        assert_eq!(
            blocks_records,
            vec![CardanoBlockRecord::new(
                "block_hash_1",
                BlockNumber(32),
                SlotNumber(59)
            )]
        );
        assert_eq!(
            transactions_records,
            vec![
                StorableCardanoTransactionRecord::new("tx_hash_1", "block_hash_1"),
                StorableCardanoTransactionRecord::new("tx_hash_2", "block_hash_1"),
                StorableCardanoTransactionRecord::new("tx_hash_3", "block_hash_1")
            ]
        );
    }
}
