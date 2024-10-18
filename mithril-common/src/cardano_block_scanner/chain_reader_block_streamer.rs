use std::sync::Arc;

use async_trait::async_trait;
use slog::{debug, trace, Logger};
use tokio::sync::Mutex;

use crate::cardano_block_scanner::BlockStreamer;
use crate::cardano_block_scanner::ChainScannedBlocks;
use crate::chain_reader::{ChainBlockNextAction, ChainBlockReader};
use crate::entities::{BlockNumber, ChainPoint};
use crate::logging::LoggerExtensions;
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
    last_polled_chain_point: Option<ChainPoint>,
    logger: Logger,
}

#[async_trait]
impl BlockStreamer for ChainReaderBlockStreamer {
    async fn poll_next(&mut self) -> StdResult<Option<ChainScannedBlocks>> {
        debug!(self.logger, ">> poll_next");

        let chain_scanned_blocks: ChainScannedBlocks;
        let mut roll_forwards = vec![];
        loop {
            let block_streamer_next_action = self.get_next_chain_block_action().await?;
            match block_streamer_next_action {
                Some(BlockStreamerNextAction::ChainBlockNextAction(
                    ChainBlockNextAction::RollForward { parsed_block },
                )) => {
                    self.last_polled_chain_point = Some(ChainPoint::from(&parsed_block));
                    let parsed_block_number = parsed_block.block_number;
                    roll_forwards.push(parsed_block);
                    if roll_forwards.len() >= self.max_roll_forwards_per_poll
                        || parsed_block_number >= self.until
                    {
                        return Ok(Some(ChainScannedBlocks::RollForwards(roll_forwards)));
                    }
                }
                Some(BlockStreamerNextAction::ChainBlockNextAction(
                    ChainBlockNextAction::RollBackward {
                        chain_point: rollback_chain_point,
                    },
                )) => {
                    self.last_polled_chain_point = Some(rollback_chain_point.clone());
                    let rollback_slot_number = rollback_chain_point.slot_number;
                    let index_rollback = roll_forwards
                        .iter()
                        .position(|block| block.slot_number == rollback_slot_number);
                    match index_rollback {
                        Some(index_rollback) => {
                            debug!(
                                self.logger,
                                "ChainScannedBlocks handled a buffer RollBackward({rollback_slot_number:?})"
                            );
                            roll_forwards.truncate(index_rollback + 1);
                        }
                        None => {
                            debug!(
                                self.logger,
                                "ChainScannedBlocks triggered a full RollBackward({rollback_slot_number:?})"
                            );
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
                    return if roll_forwards.is_empty() {
                        Ok(None)
                    } else {
                        chain_scanned_blocks = ChainScannedBlocks::RollForwards(roll_forwards);
                        Ok(Some(chain_scanned_blocks))
                    }
                }
            }
        }
    }

    fn latest_polled_chain_point(&self) -> Option<ChainPoint> {
        self.last_polled_chain_point.clone()
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
            last_polled_chain_point: None,
            logger: logger.new_with_component_name::<Self>(),
        })
    }

    async fn get_next_chain_block_action(&self) -> StdResult<Option<BlockStreamerNextAction>> {
        let mut chain_reader = self.chain_reader.try_lock()?;
        match chain_reader.get_next_chain_block().await? {
            Some(ChainBlockNextAction::RollForward { parsed_block }) => {
                if parsed_block.block_number > self.until {
                    trace!(
                        self.logger,
                        "Received a RollForward above threshold block number ({})",
                        parsed_block.block_number
                    );
                    Ok(None)
                } else {
                    trace!(
                        self.logger,
                        "Received a RollForward below threshold block number ({})",
                        parsed_block.block_number
                    );
                    Ok(Some(BlockStreamerNextAction::ChainBlockNextAction(
                        ChainBlockNextAction::RollForward { parsed_block },
                    )))
                }
            }
            Some(ChainBlockNextAction::RollBackward {
                chain_point: rollback_chain_point,
            }) => {
                let rollback_slot_number = rollback_chain_point.slot_number;
                trace!(
                    self.logger,
                    "Received a RollBackward({rollback_slot_number:?})"
                );
                let block_streamer_next_action = if rollback_slot_number == self.from.slot_number {
                    BlockStreamerNextAction::SkipToNextAction
                } else {
                    BlockStreamerNextAction::ChainBlockNextAction(
                        ChainBlockNextAction::RollBackward {
                            chain_point: rollback_chain_point,
                        },
                    )
                };
                Ok(Some(block_streamer_next_action))
            }
            None => {
                trace!(self.logger, "Received nothing");
                Ok(None)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::cardano_block_scanner::ScannedBlock;
    use crate::chain_reader::FakeChainReader;
    use crate::entities::SlotNumber;
    use crate::test_utils::TestLogger;

    use super::*;

    /// The maximum number of roll forwards during a poll
    const MAX_ROLL_FORWARDS_PER_POLL: usize = 100;

    #[tokio::test]
    async fn test_parse_expected_nothing_strictly_above_block_number_threshold() {
        let until_block_number = BlockNumber(10);
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new(
                    "hash-1",
                    until_block_number,
                    SlotNumber(100),
                    Vec::<&str>::new(),
                ),
            },
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new(
                    "hash-2",
                    until_block_number,
                    SlotNumber(100),
                    Vec::<&str>::new(),
                ),
            },
        ])));
        let mut block_streamer = ChainReaderBlockStreamer::try_new(
            chain_reader.clone(),
            None,
            until_block_number - 1,
            MAX_ROLL_FORWARDS_PER_POLL,
            TestLogger::stdout(),
        )
        .await
        .unwrap();

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");
        assert_eq!(None, scanned_blocks);
        assert_eq!(None, block_streamer.latest_polled_chain_point());

        let mut block_streamer = ChainReaderBlockStreamer::try_new(
            chain_reader,
            None,
            until_block_number,
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
                SlotNumber(100),
                Vec::<&str>::new(),
            )])),
            scanned_blocks
        );
        assert_eq!(
            block_streamer.latest_polled_chain_point(),
            Some(ChainPoint::new(
                SlotNumber(100),
                until_block_number,
                "hash-2",
            ))
        );
    }

    #[tokio::test]
    async fn test_parse_expected_multiple_rollforwards_up_to_block_number_threshold() {
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new(
                    "hash-1",
                    BlockNumber(1),
                    SlotNumber(10),
                    Vec::<&str>::new(),
                ),
            },
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new(
                    "hash-2",
                    BlockNumber(2),
                    SlotNumber(20),
                    Vec::<&str>::new(),
                ),
            },
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new(
                    "hash-3",
                    BlockNumber(3),
                    SlotNumber(30),
                    Vec::<&str>::new(),
                ),
            },
        ])));
        let mut block_streamer = ChainReaderBlockStreamer::try_new(
            chain_reader.clone(),
            None,
            BlockNumber(2),
            MAX_ROLL_FORWARDS_PER_POLL,
            TestLogger::stdout(),
        )
        .await
        .unwrap();

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");

        assert_eq!(
            Some(ChainScannedBlocks::RollForwards(vec![
                ScannedBlock::new("hash-1", BlockNumber(1), SlotNumber(10), Vec::<&str>::new()),
                ScannedBlock::new("hash-2", BlockNumber(2), SlotNumber(20), Vec::<&str>::new())
            ])),
            scanned_blocks,
        );

        let chain_reader_total_remaining_next_actions =
            chain_reader.lock().await.get_total_remaining_next_actions();
        assert_eq!(1, chain_reader_total_remaining_next_actions);

        assert_eq!(
            block_streamer.latest_polled_chain_point(),
            Some(ChainPoint::new(SlotNumber(20), BlockNumber(2), "hash-2"))
        );
    }

    #[tokio::test]
    async fn test_parse_expected_all_rollforwards_below_threshold_when_above_highest_block_number()
    {
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new(
                    "hash-1",
                    BlockNumber(1),
                    SlotNumber(10),
                    Vec::<&str>::new(),
                ),
            },
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new(
                    "hash-2",
                    BlockNumber(2),
                    SlotNumber(20),
                    Vec::<&str>::new(),
                ),
            },
        ])));
        let mut block_streamer = ChainReaderBlockStreamer::try_new(
            chain_reader.clone(),
            None,
            BlockNumber(100),
            MAX_ROLL_FORWARDS_PER_POLL,
            TestLogger::stdout(),
        )
        .await
        .unwrap();

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");

        assert_eq!(
            Some(ChainScannedBlocks::RollForwards(vec![
                ScannedBlock::new("hash-1", BlockNumber(1), SlotNumber(10), Vec::<&str>::new()),
                ScannedBlock::new("hash-2", BlockNumber(2), SlotNumber(20), Vec::<&str>::new())
            ])),
            scanned_blocks,
        );
        assert_eq!(
            block_streamer.latest_polled_chain_point(),
            Some(ChainPoint::new(SlotNumber(20), BlockNumber(2), "hash-2"))
        );
    }

    #[tokio::test]
    async fn test_parse_expected_maximum_rollforwards_retrieved_per_poll() {
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new(
                    "hash-1",
                    BlockNumber(1),
                    SlotNumber(10),
                    Vec::<&str>::new(),
                ),
            },
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new(
                    "hash-2",
                    BlockNumber(2),
                    SlotNumber(20),
                    Vec::<&str>::new(),
                ),
            },
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new(
                    "hash-3",
                    BlockNumber(3),
                    SlotNumber(30),
                    Vec::<&str>::new(),
                ),
            },
        ])));
        let mut block_streamer = ChainReaderBlockStreamer::try_new(
            chain_reader,
            None,
            BlockNumber(100),
            MAX_ROLL_FORWARDS_PER_POLL,
            TestLogger::stdout(),
        )
        .await
        .unwrap();
        block_streamer.max_roll_forwards_per_poll = 2;

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");
        assert_eq!(
            Some(ChainScannedBlocks::RollForwards(vec![
                ScannedBlock::new("hash-1", BlockNumber(1), SlotNumber(10), Vec::<&str>::new()),
                ScannedBlock::new("hash-2", BlockNumber(2), SlotNumber(20), Vec::<&str>::new())
            ])),
            scanned_blocks,
        );
        assert_eq!(
            block_streamer.latest_polled_chain_point(),
            Some(ChainPoint::new(SlotNumber(20), BlockNumber(2), "hash-2"))
        );

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");
        assert_eq!(
            Some(ChainScannedBlocks::RollForwards(vec![ScannedBlock::new(
                "hash-3",
                BlockNumber(3),
                SlotNumber(30),
                Vec::<&str>::new()
            ),])),
            scanned_blocks,
        );
        assert_eq!(
            block_streamer.latest_polled_chain_point(),
            Some(ChainPoint::new(SlotNumber(30), BlockNumber(3), "hash-3"))
        );

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");
        assert_eq!(None, scanned_blocks);
        assert_eq!(
            block_streamer.latest_polled_chain_point(),
            Some(ChainPoint::new(SlotNumber(30), BlockNumber(3), "hash-3"))
        );
    }

    #[tokio::test]
    async fn test_parse_expected_nothing_when_rollbackward_on_same_point() {
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![
            ChainBlockNextAction::RollBackward {
                chain_point: ChainPoint::new(SlotNumber(100), BlockNumber(10), "hash-123"),
            },
        ])));
        let mut block_streamer = ChainReaderBlockStreamer::try_new(
            chain_reader,
            Some(ChainPoint::new(
                SlotNumber(100),
                BlockNumber(10),
                "hash-123",
            )),
            BlockNumber(1),
            MAX_ROLL_FORWARDS_PER_POLL,
            TestLogger::stdout(),
        )
        .await
        .unwrap();

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");
        assert_eq!(None, scanned_blocks);
        assert_eq!(block_streamer.latest_polled_chain_point(), None);
    }

    #[tokio::test]
    async fn test_parse_expected_rollbackward_when_on_different_point_and_no_previous_rollforward()
    {
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![
            ChainBlockNextAction::RollBackward {
                chain_point: ChainPoint::new(SlotNumber(100), BlockNumber(10), "hash-10"),
            },
        ])));
        let mut block_streamer = ChainReaderBlockStreamer::try_new(
            chain_reader,
            None,
            BlockNumber(1),
            MAX_ROLL_FORWARDS_PER_POLL,
            TestLogger::stdout(),
        )
        .await
        .unwrap();

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");

        assert_eq!(
            Some(ChainScannedBlocks::RollBackward(SlotNumber(100))),
            scanned_blocks,
        );
        assert_eq!(
            block_streamer.latest_polled_chain_point(),
            Some(ChainPoint::new(SlotNumber(100), BlockNumber(10), "hash-10"))
        );

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");
        assert_eq!(None, scanned_blocks);
        assert_eq!(
            block_streamer.latest_polled_chain_point(),
            Some(ChainPoint::new(SlotNumber(100), BlockNumber(10), "hash-10"))
        );
    }

    #[tokio::test]
    async fn test_parse_expected_rollforward_when_rollbackward_on_different_point_and_have_previous_rollforwards(
    ) {
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new(
                    "hash-8",
                    BlockNumber(80),
                    SlotNumber(8),
                    Vec::<&str>::new(),
                ),
            },
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new(
                    "hash-9",
                    BlockNumber(90),
                    SlotNumber(9),
                    Vec::<&str>::new(),
                ),
            },
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new(
                    "hash-10",
                    BlockNumber(100),
                    SlotNumber(10),
                    Vec::<&str>::new(),
                ),
            },
            ChainBlockNextAction::RollBackward {
                chain_point: ChainPoint::new(SlotNumber(9), BlockNumber(90), "hash-9"),
            },
        ])));
        let mut block_streamer = ChainReaderBlockStreamer::try_new(
            chain_reader,
            None,
            BlockNumber(1000),
            MAX_ROLL_FORWARDS_PER_POLL,
            TestLogger::stdout(),
        )
        .await
        .unwrap();

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");

        assert_eq!(
            Some(ChainScannedBlocks::RollForwards(vec![
                ScannedBlock::new("hash-8", BlockNumber(80), SlotNumber(8), Vec::<&str>::new()),
                ScannedBlock::new("hash-9", BlockNumber(90), SlotNumber(9), Vec::<&str>::new())
            ])),
            scanned_blocks,
        );
        assert_eq!(
            block_streamer.latest_polled_chain_point(),
            Some(ChainPoint::new(SlotNumber(9), BlockNumber(90), "hash-9",))
        );
    }

    #[tokio::test]
    async fn test_parse_expected_backward_when_rollbackward_on_different_point_and_does_not_have_previous_rollforwards(
    ) {
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new(
                    "hash-8",
                    BlockNumber(80),
                    SlotNumber(8),
                    Vec::<&str>::new(),
                ),
            },
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new(
                    "hash-9",
                    BlockNumber(90),
                    SlotNumber(9),
                    Vec::<&str>::new(),
                ),
            },
            ChainBlockNextAction::RollBackward {
                chain_point: ChainPoint::new(SlotNumber(3), BlockNumber(30), "hash-3"),
            },
        ])));
        let mut block_streamer = ChainReaderBlockStreamer::try_new(
            chain_reader,
            None,
            BlockNumber(1000),
            MAX_ROLL_FORWARDS_PER_POLL,
            TestLogger::stdout(),
        )
        .await
        .unwrap();

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");

        assert_eq!(
            Some(ChainScannedBlocks::RollBackward(SlotNumber(3))),
            scanned_blocks,
        );
        assert_eq!(
            block_streamer.latest_polled_chain_point(),
            Some(ChainPoint::new(SlotNumber(3), BlockNumber(30), "hash-3",))
        );
    }

    #[tokio::test]
    async fn test_parse_expected_nothing() {
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![])));
        let mut block_streamer = ChainReaderBlockStreamer::try_new(
            chain_reader,
            None,
            BlockNumber(1),
            MAX_ROLL_FORWARDS_PER_POLL,
            TestLogger::stdout(),
        )
        .await
        .unwrap();

        let scanned_blocks = block_streamer.poll_next().await.expect("poll_next failed");

        assert_eq!(scanned_blocks, None);
    }

    #[tokio::test]
    async fn test_latest_polled_chain_point_is_none_if_nothing_was_polled() {
        let chain_reader = Arc::new(Mutex::new(FakeChainReader::new(vec![])));
        let block_streamer = ChainReaderBlockStreamer::try_new(
            chain_reader,
            None,
            BlockNumber(1),
            MAX_ROLL_FORWARDS_PER_POLL,
            TestLogger::stdout(),
        )
        .await
        .unwrap();

        assert_eq!(block_streamer.latest_polled_chain_point(), None);
    }
}
