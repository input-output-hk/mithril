use std::path::Path;

use async_trait::async_trait;
use slog::{warn, Logger};

use crate::cardano_block_scanner::{BlockScanner, BlockStreamer, ImmutableBlockStreamer};
use crate::digesters::ImmutableFile;
use crate::entities::ImmutableFileNumber;
use crate::StdResult;

/// Cardano block scanner
pub struct CardanoBlockScanner {
    logger: Logger,
    /// When set to true, no error is returned in case of unparsable block, and an error log is written instead.
    /// This can occur when the crate 'pallas-hardano' doesn't support some non final encoding for a Cardano era.
    /// This situation should only happen on the test networks and not on the mainnet.
    allow_unparsable_block: bool,
}

impl CardanoBlockScanner {
    /// Factory
    pub fn new(logger: Logger, allow_unparsable_block: bool) -> Self {
        if allow_unparsable_block {
            warn!(
                logger,
                "The 'allow_unparsable_block' option is activated. This option should only be used on test networks.")
        }

        Self {
            logger,
            allow_unparsable_block,
        }
    }
}

#[async_trait]
impl BlockScanner for CardanoBlockScanner {
    async fn scan(
        &self,
        dirpath: &Path,
        from_immutable: Option<ImmutableFileNumber>,
        until_immutable: ImmutableFileNumber,
    ) -> StdResult<Box<dyn BlockStreamer>> {
        let is_in_bounds = |number: ImmutableFileNumber| match from_immutable {
            Some(from) => (from..=until_immutable).contains(&number),
            None => number <= until_immutable,
        };
        let immutable_chunks = ImmutableFile::list_completed_in_dir(dirpath)?
            .into_iter()
            .filter(|f| is_in_bounds(f.number) && f.filename.contains("chunk"))
            .collect::<Vec<_>>();

        Ok(Box::new(ImmutableBlockStreamer::new(
            immutable_chunks,
            self.allow_unparsable_block,
            self.logger.clone(),
        )))
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{TempDir, TestLogger};

    use super::*;

    fn get_number_of_immutable_chunk_in_dir(dir: &Path) -> usize {
        ImmutableFile::list_completed_in_dir(dir)
            .unwrap()
            .into_iter()
            .map(|i| i.filename.contains("chunk"))
            .len()
    }

    #[tokio::test]
    async fn test_parse_from_lower_bound_until_upper_bound() {
        let db_path = Path::new("../mithril-test-lab/test_data/immutable/");
        assert!(get_number_of_immutable_chunk_in_dir(db_path) >= 3);

        let from_immutable_file = 2;
        let until_immutable_file = 2;
        let cardano_transaction_parser = CardanoBlockScanner::new(TestLogger::stdout(), false);

        let mut streamer = cardano_transaction_parser
            .scan(db_path, Some(from_immutable_file), until_immutable_file)
            .await
            .unwrap();
        let immutable_blocks = streamer.poll_all().await.unwrap();

        let min_immutable = immutable_blocks
            .iter()
            .map(|b| b.immutable_file_number)
            .min();
        assert_eq!(min_immutable, Some(from_immutable_file));

        let max_immutable = immutable_blocks
            .iter()
            .map(|b| b.immutable_file_number)
            .max();
        assert_eq!(max_immutable, Some(until_immutable_file));
    }

    #[tokio::test]
    async fn test_parse_up_to_given_beacon() {
        let db_path = Path::new("../mithril-test-lab/test_data/immutable/");
        assert!(get_number_of_immutable_chunk_in_dir(db_path) >= 2);

        let until_immutable_file = 1;
        let cardano_transaction_parser = CardanoBlockScanner::new(TestLogger::stdout(), false);

        let mut streamer = cardano_transaction_parser
            .scan(db_path, None, until_immutable_file)
            .await
            .unwrap();
        let immutable_blocks = streamer.poll_all().await.unwrap();

        let max_immutable = immutable_blocks
            .iter()
            .map(|b| b.immutable_file_number)
            .max();
        assert_eq!(max_immutable, Some(until_immutable_file));
    }

    #[tokio::test]
    async fn test_instantiate_parser_with_allow_unparsable_block_should_log_warning() {
        let temp_dir = TempDir::create(
            "cardano_transaction_parser",
            "test_instantiate_parser_with_allow_unparsable_block_should_log_warning",
        );
        let log_path = temp_dir.join("test.log");

        // We create a block to drop the logger and force a flush before we read the log file.
        {
            let _ = CardanoBlockScanner::new(TestLogger::file(&log_path), true);
        }

        let log_file = std::fs::read_to_string(&log_path).unwrap();
        assert!(log_file.contains("The 'allow_unparsable_block' option is activated. This option should only be used on test networks."));
    }

    #[tokio::test]
    async fn test_instantiate_parser_without_allow_unparsable_block_should_not_log_warning() {
        let temp_dir = TempDir::create(
            "cardano_transaction_parser",
            "test_instantiate_parser_without_allow_unparsable_block_should_not_log_warning",
        );
        let log_path = temp_dir.join("test.log");

        // We create a block to drop the logger and force a flush before we read the log file.
        {
            let _ = CardanoBlockScanner::new(TestLogger::file(&log_path), false);
        }

        let log_file = std::fs::read_to_string(&log_path).unwrap();
        assert!(!log_file.contains("The 'allow_unparsable_block' option is activated. This option should only be used on test networks."));
    }
}
