//! The module used for parsing Cardano transactions
use crate::{
    digesters::ImmutableFile,
    entities::{
        BlockHash, BlockNumber, CardanoTransaction, ImmutableFileNumber, SlotNumber,
        TransactionHash,
    },
    StdResult,
};
use anyhow::{anyhow, Context};
use async_trait::async_trait;
use pallas_hardano::storage::immutable::chunk::{read_blocks, Reader};
use pallas_traverse::MultiEraBlock;
use slog::{error, Logger};
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
///     use mithril_common::entities::{CardanoDbBeacon, CardanoTransaction, ImmutableFileNumber};
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
///               from_immutable: Option<ImmutableFileNumber>,
///               until_immutable: ImmutableFileNumber,
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
    async fn parse(
        &self,
        dirpath: &Path,
        from_immutable: Option<ImmutableFileNumber>,
        until_immutable: ImmutableFileNumber,
    ) -> StdResult<Vec<CardanoTransaction>>;
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
    async fn parse(
        &self,
        _dirpath: &Path,
        _from_immutable: Option<ImmutableFileNumber>,
        _until_immutable: ImmutableFileNumber,
    ) -> StdResult<Vec<CardanoTransaction>> {
        Ok(self.transactions.read().await.clone())
    }
}

#[derive(Debug)]
struct Block {
    pub block_number: BlockNumber,
    pub immutable_file_number: ImmutableFileNumber,
    pub transactions: Vec<TransactionHash>,
    pub slot_number: SlotNumber,
    pub block_hash: BlockHash,
}

impl Block {
    fn convert(multi_era_block: MultiEraBlock, immutable_file_number: ImmutableFileNumber) -> Self {
        let mut transactions = Vec::new();
        for tx in &multi_era_block.txs() {
            transactions.push(tx.hash().to_string());
        }
        Block {
            block_number: multi_era_block.number(),
            immutable_file_number,
            transactions,
            slot_number: multi_era_block.slot(),
            block_hash: multi_era_block.hash().to_string(),
        }
    }
}

/// Cardano transaction parser
pub struct CardanoTransactionParser {
    logger: Logger,
    /// When set to true, no error is returned in case of unparsable block, and an error log is written instead.
    /// This can occur when the crate 'pallas-hardano' doesn't support some non final encoding for a Cardano era.
    /// This situation should only happen on the test networks and not on the mainnet.
    allow_unparsable_block: bool,
}

impl CardanoTransactionParser {
    /// Factory
    pub fn new(logger: Logger, allow_unparsable_block: bool) -> Self {
        Self {
            logger,
            allow_unparsable_block,
        }
    }

    /// Read blocks from immutable file
    fn read_blocks_from_immutable_file(
        &self,
        immutable_file: &ImmutableFile,
    ) -> StdResult<Vec<Block>> {
        let cardano_blocks_reader =
            CardanoTransactionParser::cardano_blocks_reader(immutable_file)?;

        let mut blocks = Vec::new();
        for parsed_block in cardano_blocks_reader {
            let block = parsed_block.with_context(|| {
                format!(
                    "Error while reading block in immutable file: '{:?}'",
                    immutable_file.path
                )
            })?;
            match CardanoTransactionParser::convert_to_block(&block, immutable_file) {
                Ok(convert_to_block) => {
                    blocks.push(convert_to_block);
                }
                Err(err) if self.allow_unparsable_block => {
                    error!(
                        self.logger,
                        "The cbor encoded block could not be parsed";
                        "error" => ?err, "immutable_file_number" => immutable_file.number
                    );
                }
                Err(e) => return Err(e),
            }
        }
        Ok(blocks)
    }

    fn convert_to_block(block: &[u8], immutable_file: &ImmutableFile) -> StdResult<Block> {
        let multi_era_block = MultiEraBlock::decode(block).with_context(|| {
            format!(
                "Error while decoding block in immutable file: '{:?}'",
                immutable_file.path
            )
        })?;

        Ok(Block::convert(multi_era_block, immutable_file.number))
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

#[async_trait]
impl TransactionParser for CardanoTransactionParser {
    async fn parse(
        &self,
        dirpath: &Path,
        from_immutable: Option<ImmutableFileNumber>,
        until_immutable: ImmutableFileNumber,
    ) -> StdResult<Vec<CardanoTransaction>> {
        let up_to_file_number = until_immutable;
        let is_in_bounds = |number: ImmutableFileNumber| match from_immutable {
            Some(from) => from <= number && number <= up_to_file_number,
            None => number <= up_to_file_number,
        };
        let immutable_chunks = ImmutableFile::list_completed_in_dir(dirpath)?
            .into_iter()
            .filter(|f| is_in_bounds(f.number) && f.filename.contains("chunk"))
            .collect::<Vec<_>>();
        let mut transactions: Vec<CardanoTransaction> = vec![];

        for immutable_file in &immutable_chunks {
            let blocks = self
                .read_blocks_from_immutable_file(immutable_file)
                .with_context(|| {
                    format!(
                        "CardanoTransactionParser could read blocks from immutable file: '{}'.",
                        immutable_file.path.display()
                    )
                })?;
            let mut block_transactions = blocks
                .into_iter()
                .flat_map(|block| {
                    block.transactions.into_iter().map(move |transaction_hash| {
                        CardanoTransaction::new(
                            transaction_hash,
                            block.block_number,
                            block.slot_number,
                            block.block_hash.clone(),
                            block.immutable_file_number,
                        )
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

    use crate::test_utils::TempDir;

    use slog::Drain;
    use std::{fs::File, sync::Arc};

    fn get_number_of_immutable_chunk_in_dir(dir: &Path) -> usize {
        ImmutableFile::list_completed_in_dir(dir)
            .unwrap()
            .into_iter()
            .map(|i| i.filename.contains("chunk"))
            .len()
    }

    fn create_file_logger(filepath: &Path) -> slog::Logger {
        let writer = File::create(filepath).unwrap();
        let decorator = slog_term::PlainDecorator::new(writer);
        let drain = slog_term::CompactFormat::new(decorator).build().fuse();
        let drain = slog_async::Async::new(drain).build().fuse();

        Logger::root(Arc::new(drain), slog::o!())
    }

    #[tokio::test]
    async fn test_parse_expected_number_of_transactions() {
        // We known the number of transactions in those prebuilt immutables
        let immutable_files = [("00000", 0usize), ("00001", 2), ("00002", 3)];
        let db_path = Path::new("../mithril-test-lab/test_data/immutable/");
        assert!(get_number_of_immutable_chunk_in_dir(db_path) >= 3);

        let until_immutable_file = 2;
        let tx_count: usize = immutable_files.iter().map(|(_, count)| *count).sum();
        let cardano_transaction_parser =
            CardanoTransactionParser::new(Logger::root(slog::Discard, slog::o!()), false);

        let transactions = cardano_transaction_parser
            .parse(db_path, None, until_immutable_file)
            .await
            .unwrap();

        assert_eq!(transactions.len(), tx_count);
    }

    #[tokio::test]
    async fn test_parse_from_lower_bound_until_upper_bound() {
        // We known the number of transactions in those prebuilt immutables
        let immutable_files = [("00002", 3)];
        let db_path = Path::new("../mithril-test-lab/test_data/immutable/");
        assert!(get_number_of_immutable_chunk_in_dir(db_path) >= 3);

        let until_immutable_file = 2;
        let tx_count: usize = immutable_files.iter().map(|(_, count)| *count).sum();
        let cardano_transaction_parser =
            CardanoTransactionParser::new(Logger::root(slog::Discard, slog::o!()), false);

        let transactions = cardano_transaction_parser
            .parse(db_path, Some(2), until_immutable_file)
            .await
            .unwrap();

        assert_eq!(transactions.len(), tx_count);
    }

    #[tokio::test]
    async fn test_parse_should_error_with_unparsable_block_format() {
        let db_path = Path::new("../mithril-test-lab/test_data/parsing_error/immutable/");
        let until_immutable_file = 4831;
        let cardano_transaction_parser =
            CardanoTransactionParser::new(Logger::root(slog::Discard, slog::o!()), false);

        let result = cardano_transaction_parser
            .parse(db_path, None, until_immutable_file)
            .await;

        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_parse_should_log_error_with_unparsable_block_format() {
        let temp_dir = TempDir::create(
            "cardano_transaction_parser",
            "test_parse_should_log_error_with_unparsable_block_format",
        );
        let filepath = temp_dir.join("test.log");
        let db_path = Path::new("../mithril-test-lab/test_data/parsing_error/immutable/");
        let until_immutable_file = 4831;
        // We create a block to drop the logger and force a flush before we read the log file.
        {
            let cardano_transaction_parser =
                CardanoTransactionParser::new(create_file_logger(&filepath), true);

            cardano_transaction_parser
                .parse(db_path, None, until_immutable_file)
                .await
                .expect_err("parse should have failed");
        }

        let log_file = std::fs::read_to_string(&filepath).unwrap();
        assert!(log_file.contains("The cbor encoded block could not be parsed"));
    }

    #[tokio::test]
    async fn test_parse_up_to_given_beacon() {
        // We known the number of transactions in those prebuilt immutables
        let immutable_files = [("00000", 0usize), ("00001", 2)];
        let db_path = Path::new("../mithril-test-lab/test_data/immutable/");
        assert!(get_number_of_immutable_chunk_in_dir(db_path) >= 2);

        let until_immutable_file = 1;
        let tx_count: usize = immutable_files.iter().map(|(_, count)| *count).sum();
        let cardano_transaction_parser =
            CardanoTransactionParser::new(Logger::root(slog::Discard, slog::o!()), false);

        let transactions = cardano_transaction_parser
            .parse(db_path, None, until_immutable_file)
            .await
            .unwrap();

        assert_eq!(transactions.len(), tx_count);
    }
}
