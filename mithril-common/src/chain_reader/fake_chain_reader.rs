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
}

#[async_trait]
impl ChainBlockReader for FakeChainReader {
    async fn set_chain_point(&mut self, _point: &ChainPoint) -> StdResult<()> {
        Ok(())
    }

    async fn get_next_chain_block(&mut self) -> StdResult<Option<ChainBlockNextAction>> {
        Ok(self.chain_point_next_actions.lock().unwrap().pop_front())
    }
}

#[cfg(test)]
mod tests {
    use crate::cardano_block_scanner::ScannedBlock;

    use super::*;

    fn build_chain_point(id: u64) -> ChainPoint {
        ChainPoint {
            slot_number: id,
            block_number: id,
            block_hash: format!("point-hash-{id}"),
        }
    }

    #[tokio::test]
    async fn test_get_next_chain_block() {
        let expected_chain_point_next_actions = vec![
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new("hash-1", 1, 10, 20, Vec::<&str>::new()),
            },
            ChainBlockNextAction::RollForward {
                parsed_block: ScannedBlock::new("hash-2", 2, 11, 21, Vec::<&str>::new()),
            },
            ChainBlockNextAction::RollBackward {
                slot_number: build_chain_point(1).slot_number,
            },
        ];

        let mut chain_reader = FakeChainReader::new(expected_chain_point_next_actions.clone());

        let mut chain_point_next_actions = vec![];
        while let Some(chain_block_next_action) = chain_reader.get_next_chain_block().await.unwrap()
        {
            chain_point_next_actions.push(chain_block_next_action);
        }

        assert_eq!(expected_chain_point_next_actions, chain_point_next_actions);
    }
}
