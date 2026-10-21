use std::cmp::Reverse;
use std::iter;
use std::sync::Arc;
use std::time::Duration;

use async_trait::async_trait;
use slog::Logger;
use tokio::sync::Mutex;

use mithril_common::StdResult;
use mithril_common::entities::{BlockNumber, BlockNumberOffset};

use crate::chain_reader::ChainBlockReader;
use crate::chain_scanner::block_streamer_with_throttling::BlockStreamerWithThrottling;
use crate::chain_scanner::{BlockScanner, BlockStreamer, StoredChainPointRetriever};
use crate::entities::RawCardanoPoint;

use super::ChainReaderBlockStreamer;

/// Cardano block scanner
///
/// This scanner reads the blocks with a chain block reader, intersecting the chain of the node at
/// the start point or at a stored block below it when the start point was rolled back.
pub struct CardanoBlockScanner {
    chain_reader: Arc<Mutex<dyn ChainBlockReader>>,
    stored_chain_point_retriever: Arc<dyn StoredChainPointRetriever>,
    max_roll_forwards_per_poll: usize,
    throttling_interval: Option<Duration>,
    logger: Logger,
}

impl CardanoBlockScanner {
    /// Security parameter of Cardano, the deepest rollback a node can make
    const SECURITY_PARAMETER: u64 = 2160;

    /// Factory
    pub fn new(
        chain_reader: Arc<Mutex<dyn ChainBlockReader>>,
        stored_chain_point_retriever: Arc<dyn StoredChainPointRetriever>,
        max_roll_forwards_per_poll: usize,
        logger: Logger,
    ) -> Self {
        Self {
            chain_reader,
            stored_chain_point_retriever,
            max_roll_forwards_per_poll,
            throttling_interval: None,
            logger,
        }
    }

    /// Set the optional throttling interval for the block scanner
    ///
    /// This ensures that the scanner will not poll the chain reader more frequently than the
    /// specified interval.
    pub fn set_throttling_interval(mut self, interval: Option<Duration>) -> Self {
        if interval.is_none_or(|i| i.is_zero()) {
            self.throttling_interval = None;
        } else {
            self.throttling_interval = interval;
        }
        self
    }

    /// Candidates of the intersection with the chain of the node, most recent first.
    ///
    /// They are the start point and the stored blocks at Fibonacci depths below the highest stored
    /// block, so that the chain is intersected at the most recent of them the node knows, the
    /// rollback to it being streamed when the blocks above were rolled back.
    async fn intersection_candidates(
        &self,
        from: Option<RawCardanoPoint>,
    ) -> StdResult<Vec<RawCardanoPoint>> {
        let mut candidates: Vec<RawCardanoPoint> = from.into_iter().collect();
        if let Some(highest_stored_chain_point) =
            self.stored_chain_point_retriever.get_highest_chain_point().await?
        {
            let block_numbers =
                Self::intersection_candidate_depths(highest_stored_chain_point.block_number)
                    .into_iter()
                    .map(|depth| highest_stored_chain_point.block_number - BlockNumberOffset(depth))
                    .collect();
            let stored_chain_points = self
                .stored_chain_point_retriever
                .get_chain_points_by_block_numbers(block_numbers)
                .await?;
            candidates.extend(stored_chain_points.iter().map(RawCardanoPoint::from));
        }
        candidates.sort_by_key(|candidate| {
            (Reverse(candidate.slot_number), candidate.block_hash.clone())
        });
        candidates.dedup();

        Ok(candidates)
    }

    /// Depths below the highest stored block of the stored candidates.
    ///
    /// They follow the strategy of the chain-sync client of the Cardano node: 0, the Fibonacci
    /// numbers below the limit, then the limit, which is the security parameter or the highest
    /// stored block number when smaller.
    fn intersection_candidate_depths(highest_stored_block_number: BlockNumber) -> Vec<u64> {
        let limit = Self::SECURITY_PARAMETER.min(*highest_stored_block_number);
        let fibonacci = iter::successors(Some((1u64, 2u64)), |(current, next)| {
            Some((*next, current + next))
        })
        .map(|(current, _next)| current);
        iter::once(0)
            .chain(fibonacci.take_while(|depth| *depth < limit))
            .chain((limit > 0).then_some(limit))
            .collect()
    }
}

#[async_trait]
impl BlockScanner for CardanoBlockScanner {
    async fn scan(
        &self,
        from: Option<RawCardanoPoint>,
        until: BlockNumber,
    ) -> StdResult<Box<dyn BlockStreamer>> {
        let candidates = self.intersection_candidates(from).await?;
        let streamer = Box::new(
            ChainReaderBlockStreamer::try_new(
                self.chain_reader.clone(),
                candidates,
                until,
                self.max_roll_forwards_per_poll,
                self.logger.clone(),
            )
            .await?,
        );

        match self.throttling_interval {
            Some(interval) => Ok(Box::new(BlockStreamerWithThrottling::new(
                streamer, interval,
            ))),
            None => Ok(streamer),
        }
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::SlotNumber;

    use crate::chain_reader::MockChainBlockReader;
    use crate::entities::ScannedBlock;
    use crate::test::TestLogger;
    use crate::test::double::InMemoryChainDataStore;

    use super::*;

    fn stored_blocks_from_10_to_20() -> Vec<ScannedBlock> {
        (10..=20)
            .map(|block_number| {
                ScannedBlock::new(
                    format!("hash-{block_number}"),
                    BlockNumber(block_number),
                    SlotNumber(block_number * 10),
                    Vec::<&str>::new(),
                )
            })
            .collect()
    }

    fn point_of_block(blocks: &[ScannedBlock], block_number: u64) -> RawCardanoPoint {
        let block = blocks
            .iter()
            .find(|block| block.block_number == BlockNumber(block_number))
            .unwrap();
        RawCardanoPoint::from(block)
    }

    fn points_of_blocks(blocks: &[ScannedBlock], block_numbers: &[u64]) -> Vec<RawCardanoPoint> {
        block_numbers
            .iter()
            .map(|block_number| point_of_block(blocks, *block_number))
            .collect()
    }

    fn chain_reader_expecting_candidates(
        expected_candidates: Vec<RawCardanoPoint>,
    ) -> MockChainBlockReader {
        let mut chain_reader = MockChainBlockReader::new();
        chain_reader
            .expect_set_chain_point()
            .withf(move |candidates| candidates == expected_candidates.as_slice())
            .return_once(|_| Ok(()));
        chain_reader
    }

    fn scanner_of_stored_blocks(
        chain_reader: MockChainBlockReader,
        stored_blocks: &[ScannedBlock],
    ) -> CardanoBlockScanner {
        CardanoBlockScanner::new(
            Arc::new(Mutex::new(chain_reader)),
            Arc::new(
                InMemoryChainDataStore::builder()
                    .with_blocks_and_transactions(stored_blocks)
                    .build(),
            ),
            100,
            TestLogger::stdout(),
        )
    }

    #[test]
    fn setting_interval_of_zero_is_same_as_setting_it_to_none() {
        let scanner = scanner_of_stored_blocks(MockChainBlockReader::new(), &[]);

        assert!(scanner.throttling_interval.is_none());

        let scanner = scanner.set_throttling_interval(Some(Duration::from_secs(0)));
        assert!(scanner.throttling_interval.is_none());
    }

    #[test]
    fn the_candidate_depths_are_the_fibonacci_offsets_of_the_cardano_node_up_to_the_security_parameter()
     {
        assert_eq!(
            vec![
                0, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2160
            ],
            CardanoBlockScanner::intersection_candidate_depths(BlockNumber(10_000))
        );
    }

    #[test]
    fn the_candidate_depths_end_at_the_highest_stored_block_number_when_the_store_is_shorter() {
        assert_eq!(
            vec![0, 1, 2, 3, 5, 8, 13, 20],
            CardanoBlockScanner::intersection_candidate_depths(BlockNumber(20))
        );
    }

    #[test]
    fn the_candidate_depths_hold_no_duplicate_at_the_boundaries() {
        assert_eq!(
            vec![0],
            CardanoBlockScanner::intersection_candidate_depths(BlockNumber(0))
        );
        assert_eq!(
            vec![0, 1],
            CardanoBlockScanner::intersection_candidate_depths(BlockNumber(1))
        );
        assert_eq!(
            vec![0, 1, 2, 3, 5, 8, 13],
            CardanoBlockScanner::intersection_candidate_depths(BlockNumber(13))
        );
    }

    #[tokio::test]
    async fn the_candidates_are_the_start_point_and_the_stored_blocks_at_fibonacci_depths_most_recent_first()
     {
        let stored_blocks = stored_blocks_from_10_to_20();
        let chain_reader = chain_reader_expecting_candidates(points_of_blocks(
            &stored_blocks,
            &[20, 19, 18, 17, 15, 12],
        ));
        let scanner = scanner_of_stored_blocks(chain_reader, &stored_blocks);

        scanner
            .scan(Some(point_of_block(&stored_blocks, 20)), BlockNumber(1000))
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn a_start_point_below_the_highest_stored_block_is_ordered_among_the_stored_candidates() {
        let stored_blocks = stored_blocks_from_10_to_20();
        let chain_reader = chain_reader_expecting_candidates(points_of_blocks(
            &stored_blocks,
            &[20, 19, 18, 17, 16, 15, 12],
        ));
        let scanner = scanner_of_stored_blocks(chain_reader, &stored_blocks);

        scanner
            .scan(Some(point_of_block(&stored_blocks, 16)), BlockNumber(1000))
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn scanning_without_a_start_point_nor_stored_blocks_intersects_at_the_origin() {
        let chain_reader = chain_reader_expecting_candidates(vec![RawCardanoPoint::origin()]);
        let scanner = scanner_of_stored_blocks(chain_reader, &[]);

        scanner.scan(None, BlockNumber(1000)).await.unwrap();
    }
}
