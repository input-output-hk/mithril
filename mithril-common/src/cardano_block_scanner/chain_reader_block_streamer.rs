use std::sync::Arc;

use async_trait::async_trait;
use slog::{debug, trace, Logger};
use tokio::sync::Mutex;

use crate::cardano_block_scanner::BlockStreamer;
use crate::cardano_block_scanner::ChainScannedBlocks;
use crate::chain_reader::{ChainBlockNextAction, ChainBlockReader};
use crate::entities::BlockNumber;
use crate::entities::ChainPoint;
use crate::StdResult;

/// The action that indicates what to do next with the streamer
#[derive(Debug, Clone, PartialEq)]
enum BlockStreamerNextAction {
    /// Use a [ChainBlockNextAction]
    ChainBlockNextAction(ChainBlockNextAction),
    /// Skip to the next action
    SkipToNextAction,
}

/// [Block streamer][BlockStreamer] that streams blocks with a [Chain block reader][ChainBlockReader]
pub struct ChainReaderBlockStreamer {
    chain_reader: Arc<Mutex<dyn ChainBlockReader>>,
    from: ChainPoint,
    until: BlockNumber,
    max_roll_forwards_per_poll: usize,
    logger: Logger,
}

#[async_trait]
impl BlockStreamer for ChainReaderBlockStreamer {
    async fn poll_next(&mut self) -> StdResult<Option<ChainScannedBlocks>> {
        debug!(self.logger, "ChainReaderBlockStreamer polls next");

        let chain_scanned_blocks: ChainScannedBlocks;
        let mut roll_forwards = vec![];
        loop {
            let block_streamer_next_action = self.get_next_chain_block_action().await?;
            match block_streamer_next_action {
                Some(BlockStreamerNextAction::ChainBlockNextAction(
                    ChainBlockNextAction::RollForward { parsed_block },
                )) => {
                    roll_forwards.push(parsed_block);
                    if roll_forwards.len() >= self.max_roll_forwards_per_poll {
                        return Ok(Some(ChainScannedBlocks::RollForwards(roll_forwards)));
                    }
                }
                Some(BlockStreamerNextAction::ChainBlockNextAction(
                    ChainBlockNextAction::RollBackward {
                        slot_number: rollback_slot_number,
                    },
                )) => {
                    let index_rollback = roll_forwards
                        .iter()
                        .position(|block| block.slot_number == rollback_slot_number);
                    match index_rollback {
                        Some(index_rollback) => {
                            roll_forwards.truncate(index_rollback + 1);
                        }
                        None => {
                            chain_scanned_blocks =
                                ChainScannedBlocks::RollBackward(rollback_slot_number);
                            return Ok(Some(chain_scanned_blocks));
                        }
                    }
                }
                Some(BlockStreamerNextAction::SkipToNextAction) => {
                    continue;
                }
                None => {
                    if roll_forwards.is_empty() {
                        return Ok(None);
                    } else {
                        chain_scanned_blocks = ChainScannedBlocks::RollForwards(roll_forwards);
                        return Ok(Some(chain_scanned_blocks));
                    }
                }
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
        max_roll_forwards_per_poll: usize,
        logger: Logger,
    ) -> StdResult<Self> {
        let from = from.unwrap_or(ChainPoint::origin());
        {
            let mut chain_reader_inner = chain_reader.try_lock()?;
            chain_reader_inner.set_chain_point(&from).await?;
        }
        Ok(Self {
            chain_reader,
            from,
            until,
            max_roll_forwards_per_poll,
            logger,
        })
    }

    async fn get_next_chain_block_action(&self) -> StdResult<Option<BlockStreamerNextAction>> {
        let mut chain_reader = self.chain_reader.try_lock()?;
        match chain_reader.get_next_chain_block().await? {
            Some(ChainBlockNextAction::RollForward { parsed_block }) => {
                if parsed_block.block_number >= self.until {
                    trace!(
                        self.logger,
                        "ChainReaderBlockStreamer received a RollForward above threshold block number ({})",
                        parsed_block.block_number
                    );
                    Ok(None)
                } else {
                    trace!(
                        self.logger,
                        "ChainReaderBlockStreamer received a RollForward below threshold block number ({})",
                        parsed_block.block_number
                    );
                    Ok(Some(BlockStreamerNextAction::ChainBlockNextAction(
                        ChainBlockNextAction::RollForward { parsed_block },
                    )))
                }
            }
            Some(ChainBlockNextAction::RollBackward {
                slot_number: rollback_slot_number,
            }) => {
                debug!(
                    self.logger,
                    "ChainReaderBlockStreamer received a RollBackward({rollback_slot_number:?})"
                );
                let block_streamer_next_action = if rollback_slot_number == self.from.slot_number {
                    BlockStreamerNextAction::SkipToNextAction
                } else {
                    BlockStreamerNextAction::ChainBlockNextAction(
                        ChainBlockNextAction::RollBackward {
                            slot_number: rollback_slot_number,
                        },
                    )
                };
                Ok(Some(block_streamer_next_action))
            }
            None => {
                trace!(self.logger, "ChainReaderBlockStreamer received nothing");
                Ok(None)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::cardano_block_scanner::ScannedBlock;
    use crate::chain_reader::FakeChainReader;
    use crate::test_utils::TestLogger;

    use super::*;

    /// The maximum number of roll forwards during a poll
    const MAX_ROLL_FORWARDS_PER_POLL: usize = 100;

    #[tokio::test]
    async fn test_parse_expected_nothing_above_block_number_threshold() {
        let until_block_number = 10;
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new(
                    "hash-1",
                    until_block_number,
                    100,
                    1,
                    Vec::<&str>::new(),
                ),
            },
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new(
                    "hash-2",
                    until_block_number,
                    100,
                    1,
                    Vec::<&str>::new(),
                ),
            },
        ])));
        let mut block_streamer = ChainReaderBlockStreamer::try_new(
            chain_reader.clone(),
            None,
            until_block_number,
            MAX_ROLL_FORWARDS_PER_POLL,
            TestLogger::stdout(),
        )
        .await
        .unwrap();

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");
        assert_eq!(None, scanned_blocks);

        let mut block_streamer = ChainReaderBlockStreamer::try_new(
            chain_reader,
            None,
            until_block_number + 1,
            MAX_ROLL_FORWARDS_PER_POLL,
            TestLogger::stdout(),
        )
        .await
        .unwrap();

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");
        assert_eq!(
            Some(ChainScannedBlocks::RollForwards(vec![ScannedBlock::new(
                "hash-2",
                until_block_number,
                100,
                1,
                Vec::<&str>::new(),
            )])),
            scanned_blocks
        );
    }

    #[tokio::test]
    async fn test_parse_expected_multiple_rollforwards_below_block_number_threshold() {
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new("hash-1", 1, 10, 1, Vec::<&str>::new()),
            },
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new("hash-2", 2, 20, 1, Vec::<&str>::new()),
            },
        ])));
        let mut block_streamer = ChainReaderBlockStreamer::try_new(
            chain_reader,
            None,
            100,
            MAX_ROLL_FORWARDS_PER_POLL,
            TestLogger::stdout(),
        )
        .await
        .unwrap();

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");

        assert_eq!(
            Some(ChainScannedBlocks::RollForwards(vec![
                ScannedBlock::new("hash-1", 1, 10, 1, Vec::<&str>::new()),
                ScannedBlock::new("hash-2", 2, 20, 1, Vec::<&str>::new())
            ])),
            scanned_blocks,
        );
    }

    #[tokio::test]
    async fn test_parse_expected_maximum_rollforwards_retrieved_per_poll() {
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new("hash-1", 1, 10, 1, Vec::<&str>::new()),
            },
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new("hash-2", 2, 20, 1, Vec::<&str>::new()),
            },
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new("hash-3", 3, 30, 1, Vec::<&str>::new()),
            },
        ])));
        let mut block_streamer = ChainReaderBlockStreamer::try_new(
            chain_reader,
            None,
            100,
            MAX_ROLL_FORWARDS_PER_POLL,
            TestLogger::stdout(),
        )
        .await
        .unwrap();
        block_streamer.max_roll_forwards_per_poll = 2;

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");
        assert_eq!(
            Some(ChainScannedBlocks::RollForwards(vec![
                ScannedBlock::new("hash-1", 1, 10, 1, Vec::<&str>::new()),
                ScannedBlock::new("hash-2", 2, 20, 1, Vec::<&str>::new())
            ])),
            scanned_blocks,
        );

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");
        assert_eq!(
            Some(ChainScannedBlocks::RollForwards(vec![ScannedBlock::new(
                "hash-3",
                3,
                30,
                1,
                Vec::<&str>::new()
            ),])),
            scanned_blocks,
        );

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");
        assert_eq!(None, scanned_blocks);
    }

    #[tokio::test]
    async fn test_parse_expected_nothing_when_rollbackward_on_same_point() {
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![
            ChainBlockNextAction::RollBackward { slot_number: 100 },
        ])));
        let mut block_streamer = ChainReaderBlockStreamer::try_new(
            chain_reader,
            Some(ChainPoint::new(100, 10, "hash-123")),
            1,
            MAX_ROLL_FORWARDS_PER_POLL,
            TestLogger::stdout(),
        )
        .await
        .unwrap();

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");
        assert_eq!(None, scanned_blocks);
    }

    #[tokio::test]
    async fn test_parse_expected_rollbackward_when_on_different_point_and_no_previous_rollforward()
    {
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![
            ChainBlockNextAction::RollBackward { slot_number: 100 },
        ])));
        let mut block_streamer = ChainReaderBlockStreamer::try_new(
            chain_reader,
            None,
            1,
            MAX_ROLL_FORWARDS_PER_POLL,
            TestLogger::stdout(),
        )
        .await
        .unwrap();

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");

        assert_eq!(Some(ChainScannedBlocks::RollBackward(100)), scanned_blocks,);

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");
        assert_eq!(None, scanned_blocks);
    }

    #[tokio::test]
    async fn test_parse_expected_rollforward_when_rollbackward_on_different_point_and_have_previous_rollforwards(
    ) {
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new("hash-8", 80, 8, 1, Vec::<&str>::new()),
            },
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new("hash-9", 90, 9, 1, Vec::<&str>::new()),
            },
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new("hash-10", 100, 10, 1, Vec::<&str>::new()),
            },
            ChainBlockNextAction::RollBackward { slot_number: 9 },
        ])));
        let mut block_streamer = ChainReaderBlockStreamer::try_new(
            chain_reader,
            None,
            1000,
            MAX_ROLL_FORWARDS_PER_POLL,
            TestLogger::stdout(),
        )
        .await
        .unwrap();

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");

        assert_eq!(
            Some(ChainScannedBlocks::RollForwards(vec![
                ScannedBlock::new("hash-8", 80, 8, 1, Vec::<&str>::new()),
                ScannedBlock::new("hash-9", 90, 9, 1, Vec::<&str>::new())
            ])),
            scanned_blocks,
        );
    }

    #[tokio::test]
    async fn test_parse_expected_backward_when_rollbackward_on_different_point_and_does_not_have_previous_rollforwards(
    ) {
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new("hash-8", 80, 8, 1, Vec::<&str>::new()),
            },
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new("hash-9", 90, 9, 1, Vec::<&str>::new()),
            },
            ChainBlockNextAction::RollBackward { slot_number: 3 },
        ])));
        let mut block_streamer = ChainReaderBlockStreamer::try_new(
            chain_reader,
            None,
            1000,
            MAX_ROLL_FORWARDS_PER_POLL,
            TestLogger::stdout(),
        )
        .await
        .unwrap();

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");

        assert_eq!(Some(ChainScannedBlocks::RollBackward(3)), scanned_blocks,);
    }

    #[tokio::test]
    async fn test_parse_expected_nothing() {
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![])));
        let mut block_streamer = ChainReaderBlockStreamer::try_new(
            chain_reader,
            None,
            1,
            MAX_ROLL_FORWARDS_PER_POLL,
            TestLogger::stdout(),
        )
        .await
        .unwrap();

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");

        assert_eq!(scanned_blocks, None);
    }
}
