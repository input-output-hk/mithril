use std::path::Path;

use async_trait::async_trait;
use tokio::sync::RwLock;

use crate::cardano_block_scanner::{BlockScanner, BlockStreamer, ScannedBlock};
use crate::entities::ImmutableFileNumber;
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

    /// Update blocks returned by `parse`
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
        _from_immutable: Option<ImmutableFileNumber>,
        _until_immutable: ImmutableFileNumber,
    ) -> StdResult<Box<dyn BlockStreamer>> {
        // let iter = self.blocks.read().await.clone().into_iter();
        // Ok(Box::new(iter))
        todo!()
    }
}
