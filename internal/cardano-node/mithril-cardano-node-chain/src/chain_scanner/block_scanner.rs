use std::sync::Arc;

use async_trait::async_trait;
use mithril_common::StdResult;
use mithril_common::entities::BlockNumber;
use slog::Logger;
use tokio::sync::Mutex;

use super::ChainReaderBlockStreamer;
use crate::chain_reader::ChainBlockReader;
use crate::chain_scanner::{BlockScanner, BlockStreamer};
use crate::entities::RawCardanoPoint;

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
        from: Option<RawCardanoPoint>,
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
