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
///     use mithril_common::cardano_transaction_parser::TransactionParser;
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

    use std::path::Path;

    use pallas_hardano::storage::immutable::chunk;

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
        let immutable_files = [("00000", 20usize), ("00001", 9), ("00002", 0)];
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

    fn build_chunk_list(dirpath: &Path, beacon: &Beacon) -> StdResult<Vec<ImmutableFile>> {
        let up_to_file_number = beacon.immutable_file_number;

        // TODO The last file is not returned with this method !!!
        let chunk_list = ImmutableFile::list_completed_in_dir(dirpath)?
            .into_iter()
            .filter(|f| f.number <= up_to_file_number && f.filename.contains("chunk"))
            .collect::<Vec<_>>();
        Ok(chunk_list)
    }

    fn extract_name(f: &ImmutableFile) -> String {
        Path::new(&f.filename)
            .file_stem()
            .unwrap()
            .to_string_lossy()
            .into()
    }

    #[tokio::test]
    async fn test_build_chunk_list() {
        let db_path = Path::new("../mithril-test-lab/test_data/immutable/");
        let beacon = Beacon {
            immutable_file_number: 2,
            ..Beacon::default()
        };
        assert_eq!(
            vec!["00000".to_string(), "00001".to_string(),],
            build_chunk_list(db_path, &beacon)
                .unwrap()
                .iter()
                .map(extract_name)
                .collect::<Vec<_>>()
        );

        let beacon = Beacon {
            immutable_file_number: 0,
            ..Beacon::default()
        };
        assert_eq!(
            vec!["00000".to_string()],
            build_chunk_list(db_path, &beacon)
                .unwrap()
                .iter()
                .map(extract_name)
                .collect::<Vec<_>>()
        );
    }

    fn transactions_of_block(
        immutable_file_number: u64,
        block_data: &Vec<u8>,
    ) -> StdResult<Vec<CardanoTransaction>> {
        let block = pallas_traverse::MultiEraBlock::decode(&block_data).unwrap();
        let block_transactions = block
            .txs()
            .iter()
            .map(|t| map_to_cardano_transaction(immutable_file_number, &block, t))
            .collect::<Vec<_>>();

        Ok(block_transactions)
    }

    fn map_to_cardano_transaction(
        immutable_file_number: u64,
        block: &MultiEraBlock<'_>,
        t: &pallas_traverse::MultiEraTx<'_>,
    ) -> CardanoTransaction {
        CardanoTransaction {
            transaction_hash: t.hash().to_string(),
            block_number: block.number(),
            immutable_file_number,
        }
    }

    #[tokio::test]
    async fn test_parse_with_pallas_hardano() -> StdResult<()> {
        let dirpath = Path::new("/tmp/mithril/db/immutable");
        let beacon = Beacon {
            immutable_file_number: 2,
            ..Beacon::default()
        };

        let immutable_chunks = build_chunk_list(dirpath, &beacon)?;
        let mut transactions: Vec<CardanoTransaction> = vec![];

        for immutable_file in &immutable_chunks {
            println!("dirpath: {dirpath:?}");
            println!("number: {}", extract_name(immutable_file));
            let blocks = pallas_hardano::storage::immutable::chunk::read_blocks(
                dirpath,
                &extract_name(immutable_file),
            )
            .unwrap();

            let mut nb_blocks = 0;
            let mut nb_error_blocks = 0;

            for block in blocks {
                nb_blocks += 1;
                match block {
                    Ok(block) => match transactions_of_block(immutable_file.number, &block) {
                        Ok(block_transactions) => transactions.extend(block_transactions),
                        Err(error) => {
                            nb_error_blocks += 1;
                            println!("Error extracting transactions from block: {:?}", error);
                        }
                    },

                    Err(error) => {
                        nb_error_blocks += 1;
                        println!("Error reading block: {:?}", error);
                    }
                }
            }

            for t in &transactions {
                println!("{:?}", t);
            }
            println!("{} blocks, nb errors {}", nb_blocks, nb_error_blocks);
        }
        Ok(())
    }
}
