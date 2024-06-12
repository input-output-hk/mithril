use std::path::Path;
use std::sync::Arc;

use async_trait::async_trait;
use slog::{warn, Logger};
use tokio::sync::Mutex;

use crate::cardano_block_scanner::{BlockScanner, BlockStreamer, ImmutableBlockStreamer};
use crate::chain_reader::ChainBlockReader;
use crate::digesters::ImmutableFile;
use crate::entities::{BlockNumber, ChainPoint, ImmutableFileNumber};
use crate::StdResult;

use super::ChainReaderBlockStreamer;

/// Trait to find the lower bound that should be used by the [block scanner][CardanoBlockScanner] when
/// scanning.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait ImmutableLowerBoundFinder: Send + Sync {
    /// Find the lowest immutable file number that should be scanned by the block scanner.
    async fn find_lower_bound(&self) -> StdResult<Option<ImmutableFileNumber>>;
}

/// Cardano immutable block scanner
///
/// This scanner reads the immutable files in the given directory and returns the blocks.
///
/// Both the lower and upper bounds of the [BlockScanner] are ignored, instead:
/// * for the lower bound: the result of the [ImmutableLowerBoundFinder] is used.
/// * for the upper bound: the latest completed immutable file is used.
pub struct CardanoImmutableBlockScanner {
    logger: Logger,
    /// When set to true, no error is returned in case of unparsable block, and an error log is written instead.
    /// This can occur when the crate 'pallas-hardano' doesn't support some non final encoding for a Cardano era.
    /// This situation should only happen on the test networks and not on the mainnet.
    allow_unparsable_block: bool,
    lower_bound_finder: Arc<dyn ImmutableLowerBoundFinder>,
    rescan_offset: Option<usize>,
}

impl CardanoImmutableBlockScanner {
    /// Factory
    pub fn new(
        logger: Logger,
        allow_unparsable_block: bool,
        lower_bound_finder: Arc<dyn ImmutableLowerBoundFinder>,
        rescan_offset: Option<usize>,
    ) -> Self {
        if allow_unparsable_block {
            warn!(
                logger,
                "The 'allow_unparsable_block' option is activated. This option should only be used on test networks.")
        }

        Self {
            logger,
            allow_unparsable_block,
            lower_bound_finder,
            rescan_offset,
        }
    }

    async fn get_lower_bound(&self) -> StdResult<Option<ImmutableFileNumber>> {
        let highest = self.lower_bound_finder.find_lower_bound().await?;
        let rescan_offset = self.rescan_offset.unwrap_or(0);
        let highest = highest.map(|h| (h + 1).saturating_sub(rescan_offset as u64));
        Ok(highest)
    }
}

#[async_trait]
impl BlockScanner for CardanoImmutableBlockScanner {
    async fn scan(
        &self,
        dirpath: &Path,
        _from: Option<ChainPoint>,
        _until: BlockNumber,
    ) -> StdResult<Box<dyn BlockStreamer>> {
        let lower_bound = self.get_lower_bound().await?;
        let is_in_bounds = |number: ImmutableFileNumber| match lower_bound {
            Some(from) => from <= number,
            None => true,
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

/// Cardano block scanner
///
/// This scanner reads the blocks with a chain block reader
pub struct CardanoBlockScanner {
    chain_reader: Arc<Mutex<dyn ChainBlockReader>>,
    logger: Logger,
}

impl CardanoBlockScanner {
    /// Factory
    pub fn new(chain_reader: Arc<Mutex<dyn ChainBlockReader>>, logger: Logger) -> Self {
        Self {
            chain_reader,
            logger,
        }
    }
}

#[async_trait]
impl BlockScanner for CardanoBlockScanner {
    async fn scan(
        &self,
        _dirpath: &Path,
        from: Option<ChainPoint>,
        until: BlockNumber,
    ) -> StdResult<Box<dyn BlockStreamer>> {
        Ok(Box::new(
            ChainReaderBlockStreamer::try_new(
                self.chain_reader.clone(),
                from,
                until,
                self.logger.clone(),
            )
            .await?,
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::cardano_block_scanner::BlockStreamerTestExtensions;
    use crate::test_utils::{TempDir, TestLogger};

    use super::*;

    fn get_number_of_immutable_chunk_in_dir(dir: &Path) -> usize {
        ImmutableFile::list_completed_in_dir(dir)
            .unwrap()
            .into_iter()
            .map(|i| i.filename.contains("chunk"))
            .len()
    }

    fn lower_bound_finder<F>(finder_mock_config: F) -> Arc<dyn ImmutableLowerBoundFinder>
    where
        F: FnOnce(&mut MockImmutableLowerBoundFinder),
    {
        let mut mock = MockImmutableLowerBoundFinder::new();
        finder_mock_config(&mut mock);
        Arc::new(mock)
    }

    #[tokio::test]
    async fn test_scan_without_lower_bound_ignore_upper_bound() {
        let db_path = Path::new("../mithril-test-lab/test_data/immutable/");
        assert!(get_number_of_immutable_chunk_in_dir(db_path) >= 3);

        let lower_bound_finder = lower_bound_finder(|mock| {
            mock.expect_find_lower_bound().returning(|| Ok(None));
        });
        let cardano_transaction_parser = CardanoImmutableBlockScanner::new(
            TestLogger::stdout(),
            false,
            lower_bound_finder,
            None,
        );

        for until_block_number in [1, 10000] {
            let mut streamer = cardano_transaction_parser
                .scan(db_path, None, until_block_number)
                .await
                .unwrap();
            let immutable_blocks = streamer.poll_all().await.unwrap();

            let max_immutable_file_number = immutable_blocks
                .iter()
                .map(|b| b.immutable_file_number)
                .max();
            // The max highest completed immutable file number is 2
            assert_eq!(
                max_immutable_file_number,
                Some(2),
                "until_chain_point: {until_block_number:?}",
            );
        }
    }

    #[tokio::test]
    async fn test_scan_with_lower_bound_ignore_upper_bound() {
        let db_path = Path::new("../mithril-test-lab/test_data/immutable/");
        assert!(get_number_of_immutable_chunk_in_dir(db_path) >= 3);

        let lower_bound_finder = lower_bound_finder(|mock| {
            mock.expect_find_lower_bound().returning(|| Ok(Some(0)));
        });
        let cardano_transaction_parser = CardanoImmutableBlockScanner::new(
            TestLogger::stdout(),
            false,
            lower_bound_finder,
            None,
        );

        for until_block_number in [1, 10000] {
            let mut streamer = cardano_transaction_parser
                .scan(db_path, Some(ChainPoint::dummy()), until_block_number)
                .await
                .unwrap();
            let immutable_blocks = streamer.poll_all().await.unwrap();

            let max_immutable_file_number = immutable_blocks
                .iter()
                .map(|b| b.immutable_file_number)
                .max();
            // The max highest completed immutable file number is 2
            assert_eq!(
                max_immutable_file_number,
                Some(2),
                "until_chain_point: {until_block_number:?}",
            );
        }
    }

    #[tokio::test]
    async fn test_scan_ignore_given_lower_bound_instead_find_it_using_an_external_service() {
        let db_path = Path::new("../mithril-test-lab/test_data/immutable/");
        assert!(get_number_of_immutable_chunk_in_dir(db_path) >= 3);

        let test_cases = [
            (None, 0),
            // When a lowest immutable file number is found we start from the next immutable (i + 1)
            (Some(1), 2),
        ];
        for (lowest_found_immutable, expected) in test_cases {
            let lower_bound_finder = lower_bound_finder(|mock| {
                mock.expect_find_lower_bound()
                    .return_once(move || Ok(lowest_found_immutable));
            });

            let from = ChainPoint::dummy();
            let cardano_transaction_parser = CardanoImmutableBlockScanner::new(
                TestLogger::stdout(),
                false,
                lower_bound_finder,
                None,
            );

            let mut streamer = cardano_transaction_parser
                .scan(db_path, Some(from), 10000000)
                .await
                .unwrap();
            let immutable_blocks = streamer.poll_all().await.unwrap();

            let min_immutable_file_number = immutable_blocks
                .iter()
                .map(|b| b.immutable_file_number)
                .min();
            assert_eq!(min_immutable_file_number, Some(expected));
        }
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
            let _ = CardanoImmutableBlockScanner::new(
                TestLogger::file(&log_path),
                true,
                lower_bound_finder(|_| {}),
                None,
            );
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
            let _ = CardanoImmutableBlockScanner::new(
                TestLogger::file(&log_path),
                false,
                lower_bound_finder(|_| {}),
                None,
            );
        }

        let log_file = std::fs::read_to_string(&log_path).unwrap();
        assert!(!log_file.contains("The 'allow_unparsable_block' option is activated. This option should only be used on test networks."));
    }

    #[tokio::test]
    async fn change_parsed_lower_bound_when_rescan_limit_is_set() {
        fn scanner_with_offset(
            highest_stored_immutable: ImmutableFileNumber,
            rescan_offset: ImmutableFileNumber,
        ) -> CardanoImmutableBlockScanner {
            let mut store = MockImmutableLowerBoundFinder::new();
            store
                .expect_find_lower_bound()
                .returning(move || Ok(Some(highest_stored_immutable)));

            CardanoImmutableBlockScanner::new(
                TestLogger::stdout(),
                false,
                Arc::new(store),
                Some(rescan_offset as usize),
            )
        }
        let scanner = scanner_with_offset(8, 3);

        let from = scanner.get_lower_bound().await.unwrap();
        // Expected should be: highest_stored_beacon + 1 - rescan_offset
        assert_eq!(Some(6), from);

        let scanner = scanner_with_offset(5, 10);

        let from = scanner.get_lower_bound().await.unwrap();
        // If sub overflow it should be 0
        assert_eq!(Some(0), from);
    }
}
