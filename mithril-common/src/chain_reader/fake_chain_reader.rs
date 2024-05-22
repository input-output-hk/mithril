use async_trait::async_trait;
use std::{collections::VecDeque, sync::Mutex};

use crate::{entities::ChainPoint, StdResult};

use super::{ChainBlockNextAction, ChainBlockReader};

/// [FakeChainReader] is a fake implementation of [ChainBlockReader] for testing purposes.
pub struct FakeChainReader {
    chain_point_next_actions: Mutex<VecDeque<ChainBlockNextAction>>,
}

impl FakeChainReader {
    /// Creates a new [FakeChainReader] instance.
    pub fn new(chain_point_next_actions: Vec<ChainBlockNextAction>) -> Self {
        Self {
            chain_point_next_actions: Mutex::new(chain_point_next_actions.into()),
        }
    }

    /// Intersect the point of the chain with the given point.
    async fn intersect_point(&mut self, _point: &ChainPoint) -> StdResult<()> {
        Ok(())
    }
}

#[async_trait]
impl ChainBlockReader for FakeChainReader {
    async fn set_chain_point(&mut self, point: &ChainPoint) -> StdResult<()> {
        self.intersect_point(point).await
    }

    async fn get_next_chain_block(&mut self) -> StdResult<Option<ChainBlockNextAction>> {
        Ok(self.chain_point_next_actions.lock().unwrap().pop_front())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_get_next_chain_block() {
        let chain_point = ChainPoint {
            slot_number: 1,
            block_number: 1,
            block_hash: "point-hash-1".to_string(),
        };
        let expected_chain_point_next_actions = vec![
            ChainBlockNextAction::RollForward {
                next_point: ChainPoint {
                    slot_number: 1,
                    block_number: 1,
                    block_hash: "point-hash-1".to_string(),
                },
                raw_block: vec![],
            },
            ChainBlockNextAction::RollForward {
                next_point: ChainPoint {
                    slot_number: 2,
                    block_number: 2,
                    block_hash: "point-hash-2".to_string(),
                },
                raw_block: vec![],
            },
            ChainBlockNextAction::RollBackward {
                rollback_point: ChainPoint {
                    slot_number: 1,
                    block_number: 1,
                    block_hash: "point-hash-1".to_string(),
                },
            },
        ];
        let mut chain_reader = FakeChainReader::new(expected_chain_point_next_actions.clone());
        chain_reader.set_chain_point(&chain_point).await.unwrap();

        let mut chain_point_next_actions = vec![];
        while let Some(chain_block_next_action) = chain_reader.get_next_chain_block().await.unwrap()
        {
            chain_point_next_actions.push(chain_block_next_action);
        }

        assert_eq!(expected_chain_point_next_actions, chain_point_next_actions);
    }
}
