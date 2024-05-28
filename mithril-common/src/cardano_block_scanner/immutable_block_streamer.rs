use std::collections::VecDeque;
use std::path::Path;

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use pallas_hardano::storage::immutable::chunk::{read_blocks, Reader};
use pallas_traverse::MultiEraBlock;
use slog::{debug, error, Logger};

use crate::cardano_block_scanner::ChainScannedBlocks;
use crate::cardano_block_scanner::{BlockStreamer, ScannedBlock};
use crate::digesters::ImmutableFile;
use crate::StdResult;

/// [Block streamer][BlockStreamer] that streams blocks immutable files per immutable files
pub struct ImmutableBlockStreamer {
    remaining_immutable_files: VecDeque<ImmutableFile>,
    allow_unparsable_block: bool,
    logger: Logger,
}

#[async_trait]
impl BlockStreamer for ImmutableBlockStreamer {
    async fn poll_next(&mut self) -> StdResult<Option<ChainScannedBlocks>> {
        match &self.remaining_immutable_files.pop_front() {
            Some(immutable_file) => {
                debug!(
                    self.logger,
                    "Reading blocks from immutable file: '{}'",
                    immutable_file.path.display()
                );

                let blocks = self
                    .read_blocks_from_immutable_file(immutable_file)
                    .with_context(|| {
                        format!(
                            "BlockStreamer failed to read blocks from immutable file: '{}'.",
                            immutable_file.path.display()
                        )
                    })?;
                Ok(Some(ChainScannedBlocks::RollForwards(blocks)))
            }
            None => Ok(None),
        }
    }
}

impl ImmutableBlockStreamer {
    /// Factory
    pub fn new(
        immutables_chunk_to_stream: Vec<ImmutableFile>,
        allow_unparsable_block: bool,
        logger: Logger,
    ) -> Self {
        Self {
            remaining_immutable_files: VecDeque::from(immutables_chunk_to_stream),
            allow_unparsable_block,
            logger,
        }
    }

    fn read_blocks_from_immutable_file(
        &self,
        immutable_file: &ImmutableFile,
    ) -> StdResult<Vec<ScannedBlock>> {
        let cardano_blocks_reader = Self::cardano_blocks_reader(immutable_file)?;

        let mut blocks = Vec::new();
        for parsed_block in cardano_blocks_reader {
            let block = parsed_block.with_context(|| {
                format!(
                    "Error while reading block in immutable file: '{:?}'",
                    immutable_file.path
                )
            })?;
            match Self::convert_to_block(&block, immutable_file) {
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

    fn convert_to_block(block: &[u8], immutable_file: &ImmutableFile) -> StdResult<ScannedBlock> {
        let multi_era_block = MultiEraBlock::decode(block).with_context(|| {
            format!(
                "Error while decoding block in immutable file: '{:?}'",
                immutable_file.path
            )
        })?;

        Ok(ScannedBlock::convert(
            multi_era_block,
            immutable_file.number,
        ))
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

#[cfg(test)]
mod tests {
    use crate::cardano_block_scanner::BlockStreamerTestExtensions;
    use crate::test_utils::{TempDir, TestLogger};

    use super::*;

    #[tokio::test]
    async fn test_parse_expected_number_of_transactions() {
        fn sum_of_transactions_len(o: Option<ChainScannedBlocks>) -> Option<usize> {
            match o {
                Some(ChainScannedBlocks::RollForwards(b)) => {
                    Some(b.into_iter().map(|b| b.transactions_len()).sum())
                }
                _ => None,
            }
        }

        // We know the number of transactions in those prebuilt immutables
        let immutable_files = [
            ("00000.chunk", 0usize),
            ("00001.chunk", 2),
            ("00002.chunk", 3),
        ];
        let db_path = Path::new("../mithril-test-lab/test_data/immutable/");

        let mut streamer = ImmutableBlockStreamer::new(
            immutable_files
                .iter()
                .map(|(filename, _)| ImmutableFile::new(db_path.join(filename)).unwrap())
                .collect(),
            false,
            TestLogger::stdout(),
        );

        let immutable_blocks = streamer.poll_next().await.unwrap();
        assert_eq!(
            sum_of_transactions_len(immutable_blocks),
            Some(immutable_files[0].1)
        );

        let immutable_blocks = streamer.poll_next().await.unwrap();
        assert_eq!(
            sum_of_transactions_len(immutable_blocks),
            Some(immutable_files[1].1)
        );

        let immutable_blocks = streamer.poll_next().await.unwrap();
        assert_eq!(
            sum_of_transactions_len(immutable_blocks),
            Some(immutable_files[2].1)
        );

        let immutable_blocks = streamer.poll_next().await.unwrap();
        assert!(immutable_blocks.is_none());
    }

    #[tokio::test]
    async fn if_disallowed_reading_unparsable_block_should_fail() {
        let db_path = Path::new("../mithril-test-lab/test_data/parsing_error/immutable/");

        let mut streamer = ImmutableBlockStreamer::new(
            vec![ImmutableFile::new(db_path.join("04831.chunk")).unwrap()],
            false,
            TestLogger::stdout(),
        );
        let result = streamer.poll_all().await;

        assert!(result.is_err());
    }

    #[tokio::test]
    async fn if_allowed_reading_unparsable_block_should_log_an_error() {
        let temp_dir = TempDir::create(
            "cardano_transaction_parser",
            "if_allowed_reading_unparsable_block_should_log_an_error",
        );
        let log_path = temp_dir.join("test.log");
        let db_path = Path::new("../mithril-test-lab/test_data/parsing_error/immutable/");

        // We create a block to drop the logger and force a flush before we read the log file.
        {
            let mut streamer = ImmutableBlockStreamer::new(
                vec![ImmutableFile::new(db_path.join("04831.chunk")).unwrap()],
                true,
                TestLogger::file(&log_path),
            );
            let _res = streamer.poll_all().await;
        }

        let log_file = std::fs::read_to_string(&log_path).unwrap();
        assert!(log_file.contains("The cbor encoded block could not be parsed"));
    }
}
