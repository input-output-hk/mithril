use std::path::Path;

use async_trait::async_trait;

use crate::cardano_block_scanner::ScannedBlock;
use crate::entities::{BlockNumber, ChainPoint};
use crate::StdResult;

/// A scanner that can read cardano transactions in a cardano database
///
/// If you want to mock it using mockall:
/// ```
/// mod test {
///     use std::path::Path;
///
///     use anyhow::anyhow;
///     use async_trait::async_trait;
///     use mockall::mock;
///
///     use mithril_common::cardano_block_scanner::{BlockScanner, BlockStreamer};
///     use mithril_common::entities::{BlockNumber, ChainPoint};
///     use mithril_common::StdResult;
///
///     mock! {
///         pub BlockScannerImpl { }
///
///         #[async_trait]
///         impl BlockScanner for BlockScannerImpl {
///             async fn scan(
///               &self,
///               dirpath: &Path,
///               from: Option<ChainPoint>,
///               until: BlockNumber,
///             ) -> StdResult<Box<dyn BlockStreamer>>;
///         }
///     }
///
///     #[test]
///     fn test_mock() {
///         let mut mock = MockBlockScannerImpl::new();
///         mock.expect_scan().return_once(|_, _| {
///             Err(anyhow!("parse error"))
///         });
///     }
/// }
/// ```
#[async_trait]
pub trait BlockScanner: Sync + Send {
    /// Scan the transactions
    async fn scan(
        &self,
        dirpath: &Path,
        from: Option<ChainPoint>,
        until: BlockNumber,
    ) -> StdResult<Box<dyn BlockStreamer>>;
}

/// [ChainScannedBlocks] allows to scan new blocks and handle rollbacks
#[derive(Debug, Clone, PartialEq)]
pub enum ChainScannedBlocks {
    /// Roll forward on the chain to the next list of [ScannedBlock]
    RollForwards(Vec<ScannedBlock>),
    /// Roll backward on the chain to the previous [ChainPoint]
    RollBackward(ChainPoint),
}

/// Trait that define how blocks are streamed from a Cardano database
#[async_trait]
pub trait BlockStreamer: Sync + Send {
    /// Stream the next available blocks
    async fn poll_next(&mut self) -> StdResult<Option<ChainScannedBlocks>>;
}

cfg_test_tools! {
    /// Tests extensions methods for the [BlockStreamer] trait.
    #[async_trait]
    pub trait BlockStreamerTestExtensions{
        /// Stream all the available blocks, may be very memory intensive
        async fn poll_all(&mut self) -> StdResult<Vec<ScannedBlock>>;
    }

    #[async_trait]
    impl <S: BlockStreamer + ?Sized> BlockStreamerTestExtensions for S {
        async fn poll_all(&mut self) -> StdResult<Vec<ScannedBlock>> {
            let mut all_blocks = Vec::new();
            while let Some(next_blocks) = self.poll_next().await? {
                match next_blocks {
                    ChainScannedBlocks::RollForwards(mut forward_blocks) => {
                        all_blocks.append(&mut forward_blocks);
                    }
                    ChainScannedBlocks::RollBackward(_) => {
                        return Err(anyhow::anyhow!("poll_all: RollBackward not supported"));
                    }
                };
            }
            Ok(all_blocks)
        }
    }

}
