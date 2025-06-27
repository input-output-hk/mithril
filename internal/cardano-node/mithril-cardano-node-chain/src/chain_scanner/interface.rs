use async_trait::async_trait;

use mithril_common::StdResult;
use mithril_common::entities::{BlockNumber, SlotNumber};

use crate::entities::{RawCardanoPoint, ScannedBlock};

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
///     use mithril_common::entities::{BlockNumber};
///     use mithril_common::StdResult;
///
///     use mithril_cardano_node_chain::chain_scanner::{BlockScanner, BlockStreamer};
///     use mithril_cardano_node_chain::entities::{RawCardanoPoint};
///
///     mock! {
///         pub BlockScannerImpl { }
///
///         #[async_trait]
///         impl BlockScanner for BlockScannerImpl {
///             async fn scan(
///               &self,
///               from: Option<RawCardanoPoint>,
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
        from: Option<RawCardanoPoint>,
        until: BlockNumber,
    ) -> StdResult<Box<dyn BlockStreamer>>;
}

/// [ChainScannedBlocks] allows to scan new blocks and handle rollbacks
#[derive(Debug, Clone, PartialEq)]
pub enum ChainScannedBlocks {
    /// Roll forward on the chain to the next list of [ScannedBlock]
    RollForwards(Vec<ScannedBlock>),
    /// Roll backward on the chain to the previous [SlotNumber]
    RollBackward(SlotNumber),
}

/// Trait that define how blocks are streamed from a Cardano database
#[async_trait]
pub trait BlockStreamer: Sync + Send {
    /// Stream the next available blocks
    async fn poll_next(&mut self) -> StdResult<Option<ChainScannedBlocks>>;

    /// Get the last polled point of the chain
    fn last_polled_point(&self) -> Option<RawCardanoPoint>;
}
