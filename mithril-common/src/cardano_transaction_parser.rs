//! The module used for parsing Cardano transactions
use crate::{
    digesters::ImmutableFile,
    entities::{Beacon, BlockNumber, CardanoTransaction, ImmutableFileNumber, TransactionHash},
    StdResult,
};
use anyhow::{anyhow, Context};
use async_trait::async_trait;
use pallas_hardano::storage::immutable::chunk::{read_blocks, Reader};
use pallas_traverse::MultiEraBlock;
use std::path::Path;
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
        let cardano_blocks_reader =
            CardanoTransactionParser::cardano_blocks_reader(immutable_file)?;

        let mut blocks = Vec::new();
        for block in cardano_blocks_reader {
            let block = block.map_err(|e| {
                anyhow!(e).context(format!(
                    "Error while reading block in immutable file: '{:?}'",
                    immutable_file.path
                ))
            })?;
            blocks.push(CardanoTransactionParser::convert_to_block(
                &block,
                immutable_file,
            )?);
        }
        Ok(blocks)
    }

    fn convert_to_block(block: &[u8], immutable_file: &ImmutableFile) -> StdResult<Block> {
        let multi_era_block = MultiEraBlock::decode(block).map_err(|e| {
            anyhow!(e).context(format!(
                "Error while decoding block in immutable file: '{:?}'",
                immutable_file.path
            ))
        })?;

        Block::try_convert(multi_era_block, immutable_file.number).with_context(|| {
            format!(
                "CardanoTransactionParser could not read data from block in immutable file: {:?}",
                immutable_file.path
            )
        })
    }

    fn cardano_blocks_reader(immutable_file: &ImmutableFile) -> StdResult<Reader> {
        let dir_path = immutable_file.path.parent().ok_or(anyhow!(format!(
            "Could not retrieve immutable file directory with immutable file path: '{:?}'",
            immutable_file.path
        )))?;
        let file_name = &Path::new(&immutable_file.filename)
            .file_stem()
            .ok_or(anyhow!(format!(
                "Could not extract immutable file name from file: '{}'",
                immutable_file.filename
            )))?
            .to_string_lossy();
        let blocks = read_blocks(dir_path, file_name)?;

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
        let immutable_files = [("00000", 0usize), ("00001", 2), ("00002", 3)];
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
        let immutable_files = [("00000", 0usize), ("00001", 2)];
        let db_path = Path::new("../mithril-test-lab/test_data/immutable/");
        assert!(get_number_of_immutable_chunk_in_dir(db_path) >= 2);

        let beacon = Beacon {
            immutable_file_number: 1,
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
