use std::time::Duration;

use async_trait::async_trait;
use tokio::sync::Mutex;
use tokio::time::Interval;

use mithril_common::StdResult;

use crate::chain_scanner::{BlockStreamer, ChainScannedBlocks};
use crate::entities::RawCardanoPoint;

/// A decorator of [BlockStreamer] that throttles the block polling
pub struct BlockStreamerWithThrottling {
    wrapped_streamer: Box<dyn BlockStreamer>,
    interval: Mutex<Interval>,
}

impl BlockStreamerWithThrottling {
    /// Create a new instance of `BlockStreamerWithThrottling`.
    pub fn new(wrapped_streamer: Box<dyn BlockStreamer>, min_interval: Duration) -> Self {
        Self {
            wrapped_streamer,
            interval: Mutex::new(tokio::time::interval(min_interval)),
        }
    }
}

#[async_trait]
impl BlockStreamer for BlockStreamerWithThrottling {
    async fn poll_next(&mut self) -> StdResult<Option<ChainScannedBlocks>> {
        let mut interval = self.interval.lock().await;
        interval.tick().await;

        self.wrapped_streamer.poll_next().await
    }

    fn last_polled_point(&self) -> Option<RawCardanoPoint> {
        self.wrapped_streamer.last_polled_point()
    }
}

#[cfg(test)]
mod tests {
    use tokio::time::Instant;

    use mithril_common::entities::{BlockNumber, SlotNumber};

    use crate::entities::ScannedBlock;
    use crate::test::double::DumbBlockStreamer;

    use super::*;

    fn blocks_set() -> [ScannedBlock; 4] {
        [
            ScannedBlock::new(
                "block_hash-1",
                BlockNumber(10),
                SlotNumber(15),
                vec!["tx_hash-1"],
            ),
            ScannedBlock::new(
                "block_hash-2",
                BlockNumber(20),
                SlotNumber(25),
                vec!["tx_hash-2"],
            ),
            ScannedBlock::new(
                "block_hash-3",
                BlockNumber(30),
                SlotNumber(35),
                vec!["tx_hash-3"],
            ),
            ScannedBlock::new(
                "block_hash-4",
                BlockNumber(40),
                SlotNumber(45),
                vec!["tx_hash-4"],
            ),
        ]
    }

    #[tokio::test(flavor = "current_thread", start_paused = true)]
    async fn first_poll_succeeds_immediately_without_throttling() {
        let mut streamer = BlockStreamerWithThrottling::new(
            Box::new(DumbBlockStreamer::new().forwards(vec![blocks_set().to_vec()])),
            Duration::from_millis(100),
        );

        let start = Instant::now();
        streamer.poll_next().await.unwrap();

        assert_eq!(start.elapsed(), Duration::from_millis(0));
    }

    #[tokio::test(flavor = "current_thread", start_paused = true)]
    async fn consecutive_polls_are_throttled_by_min_interval() {
        let mut streamer = BlockStreamerWithThrottling::new(
            Box::new(DumbBlockStreamer::new().forwards(vec![blocks_set().to_vec()])),
            Duration::from_millis(100),
        );

        let start = Instant::now();

        // First poll - immediate
        streamer.poll_next().await.unwrap();
        assert_eq!(start.elapsed(), Duration::from_millis(0));

        streamer.poll_next().await.unwrap();
        assert_eq!(start.elapsed(), Duration::from_millis(100));
    }

    #[tokio::test(flavor = "current_thread", start_paused = true)]
    async fn throttling_waits_only_for_remaining_time() {
        let mut streamer = BlockStreamerWithThrottling::new(
            Box::new(DumbBlockStreamer::new().forwards(vec![blocks_set().to_vec()])),
            Duration::from_millis(100),
        );

        // First poll - immediate
        streamer.poll_next().await.unwrap();

        // Advance time by 60ms (less than the configured 100ms interval)
        tokio::time::advance(Duration::from_millis(60)).await;
        let before_second_poll = Instant::now();

        // Second poll should only wait the remaining 40ms
        streamer.poll_next().await.unwrap();
        assert_eq!(before_second_poll.elapsed(), Duration::from_millis(40));
    }

    #[tokio::test(flavor = "current_thread", start_paused = true)]
    async fn no_throttling_if_enough_time_elapsed_between_polls() {
        let mut streamer = BlockStreamerWithThrottling::new(
            Box::new(DumbBlockStreamer::new().forwards(vec![blocks_set().to_vec()])),
            Duration::from_millis(100),
        );

        // First poll - immediate
        streamer.poll_next().await.unwrap();

        // Advance time beyond the throttle interval
        tokio::time::advance(Duration::from_millis(150)).await;
        let before_second_poll = Instant::now();

        // Second poll should succeed immediately
        streamer.poll_next().await.unwrap();
        assert_eq!(before_second_poll.elapsed(), Duration::from_millis(0));
    }

    #[tokio::test]
    async fn delegates_to_wrapped_streamer() {
        let expected_point = RawCardanoPoint::new(SlotNumber(100), "block_hash-last");

        let streamer = BlockStreamerWithThrottling::new(
            Box::new(DumbBlockStreamer::new().set_last_polled_point(Some(expected_point.clone()))),
            Duration::from_millis(100),
        );

        assert_eq!(Some(expected_point), streamer.last_polled_point());
    }
}
