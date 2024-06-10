use std::collections::VecDeque;
use std::path::Path;

use async_trait::async_trait;
use tokio::sync::RwLock;

use crate::cardano_block_scanner::ChainScannedBlocks;
use crate::cardano_block_scanner::{BlockScanner, BlockStreamer, ScannedBlock};
use crate::entities::{BlockNumber, ChainPoint};
use crate::StdResult;

/// Dumb block scanner
pub struct DumbBlockScanner {
    blocks: RwLock<Vec<ScannedBlock>>,
}

impl DumbBlockScanner {
    /// Factory
    pub fn new(blocks: Vec<ScannedBlock>) -> Self {
        Self {
            blocks: RwLock::new(blocks),
        }
    }

    /// Update blocks returned used the streamer constructed by `scan`
    pub async fn update_blocks(&self, new_blocks: Vec<ScannedBlock>) {
        let mut blocks = self.blocks.write().await;
        *blocks = new_blocks;
    }
}

#[async_trait]
impl BlockScanner for DumbBlockScanner {
    async fn scan(
        &self,
        _dirpath: &Path,
        _from: Option<ChainPoint>,
        _until: BlockNumber,
    ) -> StdResult<Box<dyn BlockStreamer>> {
        let blocks = self.blocks.read().await.clone();
        Ok(Box::new(
            DumbBlockStreamer::new(vec![]).forwards(vec![blocks]),
        ))
    }
}

/// Dumb block streamer
pub struct DumbBlockStreamer {
    streamer_responses: VecDeque<ChainScannedBlocks>,
}

impl DumbBlockStreamer {
    /// Factory - the resulting streamer can be polled one time for each list of blocks given
    pub fn new(blocks: Vec<Vec<ScannedBlock>>) -> Self {
        Self {
            streamer_responses: blocks
                .into_iter()
                .map(ChainScannedBlocks::RollForwards)
                .collect(),
        }
    }

    pub fn forwards(mut self, blocks: Vec<Vec<ScannedBlock>>) -> Self {
        let mut source: VecDeque<_> = blocks
            .into_iter()
            .map(ChainScannedBlocks::RollForwards)
            .collect();
        self.streamer_responses.append(&mut source);

        self
    }

    pub fn rollback(mut self, chain_point: ChainPoint) -> Self {
        self.streamer_responses
            .push_back(ChainScannedBlocks::RollBackward(chain_point));
        self
    }
}

#[async_trait]
impl BlockStreamer for DumbBlockStreamer {
    async fn poll_next(&mut self) -> StdResult<Option<ChainScannedBlocks>> {
        Ok(self.streamer_responses.pop_front())
    }
}

#[cfg(test)]
mod tests {
    use fixed::consts::E;

    use crate::cardano_block_scanner::BlockStreamerTestExtensions;

    use super::*;

    #[tokio::test]
    async fn polling_without_set_of_block_return_none() {
        let mut streamer = DumbBlockStreamer::new(vec![]).forwards(vec![]);
        let blocks = streamer.poll_next().await.unwrap();
        assert_eq!(blocks, None);
    }

    #[tokio::test]
    async fn polling_with_one_set_of_block_returns_some_once() {
        let expected_blocks = vec![ScannedBlock::new("hash-1", 1, 10, 20, Vec::<&str>::new())];
        let mut streamer = DumbBlockStreamer::new(vec![]).forwards(vec![expected_blocks.clone()]);

        let blocks = streamer.poll_next().await.unwrap();
        assert_eq!(
            blocks,
            Some(ChainScannedBlocks::RollForwards(expected_blocks))
        );

        let blocks = streamer.poll_next().await.unwrap();
        assert_eq!(blocks, None);
    }

    #[tokio::test]
    async fn polling_with_multiple_sets_of_blocks_returns_some_once() {
        let expected_blocks = vec![
            vec![ScannedBlock::new("hash-1", 1, 10, 20, Vec::<&str>::new())],
            vec![
                ScannedBlock::new("hash-2", 2, 11, 21, Vec::<&str>::new()),
                ScannedBlock::new("hash-3", 3, 12, 22, Vec::<&str>::new()),
            ],
            vec![ScannedBlock::new("hash-4", 4, 13, 23, Vec::<&str>::new())],
        ];
        let mut streamer = DumbBlockStreamer::new(vec![]).forwards(expected_blocks.clone());

        let blocks = streamer.poll_next().await.unwrap();
        assert_eq!(
            blocks,
            Some(ChainScannedBlocks::RollForwards(expected_blocks[0].clone()))
        );

        let blocks = streamer.poll_next().await.unwrap();
        assert_eq!(
            blocks,
            Some(ChainScannedBlocks::RollForwards(expected_blocks[1].clone()))
        );

        let blocks = streamer.poll_next().await.unwrap();
        assert_eq!(
            blocks,
            Some(ChainScannedBlocks::RollForwards(expected_blocks[2].clone()))
        );

        let blocks = streamer.poll_next().await.unwrap();
        assert_eq!(blocks, None);
    }

    #[tokio::test]
    async fn dumb_scanned_construct_a_streamer_based_on_its_stored_blocks() {
        let expected_blocks = vec![ScannedBlock::new("hash-1", 1, 10, 20, Vec::<&str>::new())];

        let scanner = DumbBlockScanner::new(expected_blocks.clone());
        let mut streamer = scanner.scan(Path::new("dummy"), None, 5).await.unwrap();

        let blocks = streamer.poll_all().await.unwrap();
        assert_eq!(blocks, expected_blocks);
    }

    #[tokio::test]
    async fn polling_with_can_return_roll_backward() {
        let expected_blocks = vec![
            vec![ScannedBlock::new("hash-1", 1, 10, 20, Vec::<&str>::new())],
            vec![ScannedBlock::new("hash-4", 4, 13, 23, Vec::<&str>::new())],
        ];

        let expected_chain_point = ChainPoint::new(10, 2, "block-hash");

        let mut streamer = DumbBlockStreamer::new(vec![])
            .forwards(expected_blocks.clone())
            .rollback(expected_chain_point.clone());

        let blocks = streamer.poll_next().await.unwrap();
        assert_eq!(
            blocks,
            Some(ChainScannedBlocks::RollForwards(expected_blocks[0].clone()))
        );

        let blocks = streamer.poll_next().await.unwrap();
        assert_eq!(
            blocks,
            Some(ChainScannedBlocks::RollForwards(expected_blocks[1].clone()))
        );

        let blocks = streamer.poll_next().await.unwrap();
        assert_eq!(
            blocks,
            Some(ChainScannedBlocks::RollBackward(
                expected_chain_point.clone()
            ))
        );

        let blocks = streamer.poll_next().await.unwrap();
        assert_eq!(blocks, None);
    }
}
