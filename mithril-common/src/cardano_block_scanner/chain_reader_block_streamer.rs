use std::sync::Arc;

use async_trait::async_trait;
use slog::{debug, Logger};
use tokio::sync::Mutex;

use crate::cardano_block_scanner::BlockStreamer;
use crate::cardano_block_scanner::ChainScannedBlocks;
use crate::chain_reader::{ChainBlockNextAction, ChainBlockReader};
use crate::entities::BlockNumber;
use crate::entities::ChainPoint;
use crate::StdResult;

/// [Block streamer][BlockStreamer] that streams blocks with a \[Chain reader\][\ChainReader\]
pub struct ChainReaderBlockStreamer {
    chain_reader: Arc<Mutex<dyn ChainBlockReader>>,
    until: BlockNumber,
    logger: Logger,
}

#[async_trait]
impl BlockStreamer for ChainReaderBlockStreamer {
    async fn poll_next(&mut self) -> StdResult<Option<ChainScannedBlocks>> {
        debug!(self.logger, "ChainReaderBlockStreamer polls next",);
        let mut chain_reader = self.chain_reader.try_lock()?;
        match chain_reader.get_next_chain_block().await? {
            Some(ChainBlockNextAction::RollForward {
                next_point,
                parsed_block,
            }) => {
                debug!(self.logger, "RollForward ({next_point:?})");
                if next_point.block_number >= self.until {
                    debug!(
                        self.logger,
                        "ChainReaderBlockStreamer received a RollForward({next_point:?}) above threshold block number ({})",
                        next_point.block_number
                    );
                    return Ok(None);
                } else {
                    debug!(
                        self.logger,
                        "ChainReaderBlockStreamer received a RollForward({next_point:?}) below threshold block number ({})",
                        next_point.block_number
                    );
                    chain_reader.set_chain_point(&next_point).await?;
                    Ok(Some(ChainScannedBlocks::RollForwards(vec![parsed_block])))
                }
            }
            Some(ChainBlockNextAction::RollBackward { rollback_point }) => {
                debug!(
                    self.logger,
                    "ChainReaderBlockStreamer received a RollBackward({rollback_point:?})"
                );
                chain_reader.set_chain_point(&rollback_point).await?;
                Ok(Some(ChainScannedBlocks::RollBackward(rollback_point)))
            }
            None => {
                debug!(self.logger, "ChainReaderBlockStreamer received nothing");
                Ok(None)
            }
        }
    }
}

impl ChainReaderBlockStreamer {
    /// Factory
    pub async fn try_new(
        chain_reader: Arc<Mutex<dyn ChainBlockReader>>,
        from: Option<ChainPoint>,
        until: BlockNumber,
        logger: Logger,
    ) -> StdResult<Self> {
        {
            let mut chain_reader_inner = chain_reader.try_lock()?;
            chain_reader_inner
                .set_chain_point(from.as_ref().unwrap_or(&ChainPoint::origin()))
                .await?;
        }
        Ok(Self {
            chain_reader,
            until,
            logger,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::cardano_block_scanner::ScannedBlock;
    use crate::chain_reader::FakeChainReader;
    use crate::test_utils::TestLogger;

    use super::*;

    #[tokio::test]
    async fn test_parse_expected_rollforward_below_block_number_threshold() {
        let logger = TestLogger::stdout();
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![
            ChainBlockNextAction::RollForward {
                next_point: ChainPoint::new(100, 10, "hash-123"),
                parsed_block: ScannedBlock::new("hash-1", 1, 10, 20, Vec::<&str>::new()),
            },
        ]))) as Arc<Mutex<dyn ChainBlockReader>>;
        let mut block_streamer =
            ChainReaderBlockStreamer::try_new(chain_reader, None, 100, logger.clone())
                .await
                .unwrap();

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");

        assert_eq!(
            Some(ChainScannedBlocks::RollForwards(vec![ScannedBlock::new(
                "hash-1",
                1,
                10,
                20,
                Vec::<&str>::new()
            )])),
            scanned_blocks,
        );
    }

    #[tokio::test]
    async fn test_parse_expected_nothing_above_block_number_threshold() {
        let logger = TestLogger::stdout();
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![
            ChainBlockNextAction::RollForward {
                next_point: ChainPoint::new(100, 10, "hash-123"),
                parsed_block: ScannedBlock::new("hash-1", 1, 10, 20, Vec::<&str>::new()),
            },
        ]))) as Arc<Mutex<dyn ChainBlockReader>>;
        let mut block_streamer =
            ChainReaderBlockStreamer::try_new(chain_reader, None, 1, logger.clone())
                .await
                .unwrap();

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");

        assert_eq!(None, scanned_blocks,);
    }

    #[tokio::test]
    async fn test_parse_expected_rollbackward() {
        let logger = TestLogger::stdout();
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![
            ChainBlockNextAction::RollBackward {
                rollback_point: ChainPoint::new(100, 10, "hash-123"),
            },
        ]))) as Arc<Mutex<dyn ChainBlockReader>>;
        let mut block_streamer =
            ChainReaderBlockStreamer::try_new(chain_reader, None, 1, logger.clone())
                .await
                .unwrap();

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");

        assert_eq!(
            Some(ChainScannedBlocks::RollBackward(ChainPoint::new(
                100, 10, "hash-123"
            ))),
            scanned_blocks,
        );
    }

    #[tokio::test]
    async fn test_parse_expected_nothing() {
        let logger = TestLogger::stdout();
        let chain_reader =
            Arc::new(Mutex::new(FakeChainReader::new(vec![]))) as Arc<Mutex<dyn ChainBlockReader>>;
        let mut block_streamer =
            ChainReaderBlockStreamer::try_new(chain_reader, None, 1, logger.clone())
                .await
                .unwrap();

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");

        assert_eq!(scanned_blocks, None);
    }
}
