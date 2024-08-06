use std::sync::Arc;

use async_trait::async_trait;
use slog::Logger;
use tokio::sync::Mutex;

use crate::cardano_block_scanner::{BlockScanner, BlockStreamer};
use crate::chain_reader::ChainBlockReader;
use crate::entities::{BlockNumber, ChainPoint};
use crate::StdResult;

use super::ChainReaderBlockStreamer;

/// Cardano block scanner
///
/// This scanner reads the blocks with a chain block reader
pub struct CardanoBlockScanner {
    chain_reader: Arc<Mutex<dyn ChainBlockReader>>,
    max_roll_forwards_per_poll: usize,
    logger: Logger,
}

impl CardanoBlockScanner {
    /// Factory
    pub fn new(
        chain_reader: Arc<Mutex<dyn ChainBlockReader>>,
        max_roll_forwards_per_poll: usize,
        logger: Logger,
    ) -> Self {
        Self {
            chain_reader,
            max_roll_forwards_per_poll,
            logger,
        }
    }
}

#[async_trait]
impl BlockScanner for CardanoBlockScanner {
    async fn scan(
        &self,
        from: Option<ChainPoint>,
        until: BlockNumber,
    ) -> StdResult<Box<dyn BlockStreamer>> {
        Ok(Box::new(
            ChainReaderBlockStreamer::try_new(
                self.chain_reader.clone(),
                from,
                until,
                self.max_roll_forwards_per_poll,
                self.logger.clone(),
            )
            .await?,
        ))
    }
}
