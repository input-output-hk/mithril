//! The module used for parsing Cardano transactions

use crate::{
    digesters::ImmutableFile,
    entities::{Beacon, BlockNumber, CardanoTransaction, ImmutableFileNumber, TransactionHash},
    StdResult,
};
use anyhow::Context;
use async_trait::async_trait;
use pallas_traverse::{
    probe::{block_era, Outcome},
    MultiEraBlock,
};
use std::{cmp::min, fs, path::Path};
use tokio::sync::RwLock;

/// A parser that can read cardano transactions in a cardano database
///
/// If you want to mock it using mockall:
/// ```
/// mod test {
///     use anyhow::anyhow;
///     use async_trait::async_trait;
///     use mithril_common::cardano_transactions_parser::TransactionParser;
///     use mithril_common::entities::{Beacon, CardanoTransaction};
///     use mithril_common::StdResult;
///     use mockall::mock;
///     use std::path::Path;
///
///     mock! {
///         pub TransactionParserImpl { }
///
///         #[async_trait]
///         impl TransactionParser for TransactionParserImpl {
///             async fn parse(
///               &self,
///               dirpath: &Path,
///               beacon: &Beacon,
///             ) -> StdResult<Vec<CardanoTransaction>>;
///         }
///     }
///
///     #[test]
///     fn test_mock() {
///         let mut mock = MockTransactionParserImpl::new();
///         mock.expect_parse().return_once(|_, _| {
///             Err(anyhow!("parse error"))
///         });
///     }
/// }
/// ```
#[async_trait]
pub trait TransactionParser: Sync + Send {
    /// Parse the transactions
    async fn parse(&self, dirpath: &Path, beacon: &Beacon) -> StdResult<Vec<CardanoTransaction>>;
}

/// Dumb transaction parser
pub struct DumbTransactionParser {
    transactions: RwLock<Vec<CardanoTransaction>>,
}

impl DumbTransactionParser {
    /// Factory
    pub fn new(transactions: Vec<CardanoTransaction>) -> Self {
        Self {
            transactions: RwLock::new(transactions),
        }
    }

    /// Update transactions returned by `parse`
    pub async fn update_transactions(&self, new_transactions: Vec<CardanoTransaction>) {
        let mut transactions = self.transactions.write().await;
        *transactions = new_transactions;
    }
}

#[async_trait]
impl TransactionParser for DumbTransactionParser {
    async fn parse(&self, _dirpath: &Path, _beacon: &Beacon) -> StdResult<Vec<CardanoTransaction>> {
        Ok(self.transactions.read().await.clone())
    }
}

#[derive(Debug)]
struct Block {
    pub block_number: BlockNumber,
    pub immutable_file_number: ImmutableFileNumber,
    pub transactions: Vec<TransactionHash>,
}

impl Block {
    fn try_convert(
        multi_era_block: MultiEraBlock,
        immutable_file_number: ImmutableFileNumber,
    ) -> StdResult<Self> {
        let mut transactions = Vec::new();
        for tx in &multi_era_block.txs() {
            transactions.push(tx.hash().to_string());
        }
        let block = Block {
            block_number: multi_era_block.number(),
            immutable_file_number,
            transactions,
        };

        Ok(block)
    }
}

/// Cardano transaction parser
pub struct CardanoTransactionParser {}

impl CardanoTransactionParser {
    /// Factory
    pub fn new() -> Self {
        Self {}
    }

    /// Read blocks from immutable file
    fn read_blocks_from_immutable_file(immutable_file: &ImmutableFile) -> StdResult<Vec<Block>> {
        let cbor = fs::read(&immutable_file.path).with_context(|| {
            format!(
                "CardanoTransactionParser could not read from immutable file: {:?}",
                immutable_file.path
            )
        })?;

        let mut blocks_start_byte_index: Vec<_> = (0..cbor.len())
            .filter(|&byte_index| {
                let cbor_header_maybe = &cbor[byte_index..min(byte_index + 2, cbor.len())];
                match block_era(cbor_header_maybe) {
                    Outcome::Matched(_) | Outcome::EpochBoundary => true,
                    Outcome::Inconclusive => false,
                }
            })
            .collect();

        blocks_start_byte_index.push(cbor.len() + 1);

        let mut blocks = Vec::new();
        let mut last_start_byte_index = 0;
        for block_start_index in blocks_start_byte_index.into_iter().skip(1) {
            let maybe_end_byte_index = min(block_start_index, cbor.len());
            if let Ok(multi_era_block) =
                MultiEraBlock::decode(&cbor[last_start_byte_index..maybe_end_byte_index])
            {
                let block = Block::try_convert(multi_era_block, immutable_file.number)
                    .with_context(|| {
                        format!(
                        "CardanoTransactionParser could not read data from block in immutable file: {:?}",
                        immutable_file.path
                    )
                    })?;
                blocks.push(block);
                last_start_byte_index = block_start_index;
            }
        }

        Ok(blocks)
    }
}

impl Default for CardanoTransactionParser {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl TransactionParser for CardanoTransactionParser {
    async fn parse(&self, dirpath: &Path, beacon: &Beacon) -> StdResult<Vec<CardanoTransaction>> {
        let up_to_file_number = beacon.immutable_file_number;
        let immutable_chunks = ImmutableFile::list_completed_in_dir(dirpath)?
            .into_iter()
            .filter(|f| f.number <= up_to_file_number && f.filename.contains("chunk"))
            .collect::<Vec<_>>();
        let mut transactions: Vec<CardanoTransaction> = vec![];

        for immutable_file in &immutable_chunks {
            let blocks =
                Self::read_blocks_from_immutable_file(immutable_file).with_context(|| {
                    format!(
                        "CardanoTransactionParser could read blocks from immutable file: '{}'.",
                        immutable_file.path.display()
                    )
                })?;
            let mut block_transactions = blocks
                .into_iter()
                .flat_map(|block| {
                    block
                        .transactions
                        .into_iter()
                        .map(move |transaction_hash| CardanoTransaction {
                            transaction_hash,
                            block_number: block.block_number,
                            immutable_file_number: block.immutable_file_number,
                        })
                })
                .collect::<Vec<_>>();

            transactions.append(&mut block_transactions);
        }

        Ok(transactions)
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn get_number_of_immutable_chunk_in_dir(dir: &Path) -> usize {
        ImmutableFile::list_completed_in_dir(dir)
            .unwrap()
            .into_iter()
            .map(|i| i.filename.contains("chunk"))
            .len()
    }

    #[tokio::test]
    async fn test_parse_expected_number_of_transactions() {
        // We known the number of transactions in those prebuilt immutables
        let immutable_files = [("00000", 20usize), ("00001", 8), ("00002", 0)];
        let db_path = Path::new("../mithril-test-lab/test_data/immutable/");
        assert!(get_number_of_immutable_chunk_in_dir(db_path) >= 3);

        let beacon = Beacon {
            immutable_file_number: 2,
            ..Beacon::default()
        };
        let tx_count: usize = immutable_files.iter().map(|(_, count)| *count).sum();
        let cardano_transaction_parser = CardanoTransactionParser::new();

        let transactions = cardano_transaction_parser
            .parse(db_path, &beacon)
            .await
            .unwrap();

        assert_eq!(transactions.len(), tx_count);
    }

    #[tokio::test]
    async fn test_parse_up_to_given_beacon() {
        // We known the number of transactions in those prebuilt immutables
        let immutable_files = [("00000", 20usize)];
        let db_path = Path::new("../mithril-test-lab/test_data/immutable/");
        assert!(get_number_of_immutable_chunk_in_dir(db_path) >= 2);

        let beacon = Beacon {
            immutable_file_number: 0,
            ..Beacon::default()
        };
        let tx_count: usize = immutable_files.iter().map(|(_, count)| *count).sum();
        let cardano_transaction_parser = CardanoTransactionParser::new();

        let transactions = cardano_transaction_parser
            .parse(db_path, &beacon)
            .await
            .unwrap();

        assert_eq!(transactions.len(), tx_count);
    }
}
