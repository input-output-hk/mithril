use std::path::Path;

use async_trait::async_trait;

use crate::cardano_block_scanner::ScannedBlock;
use crate::entities::ImmutableFileNumber;
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
///     use mithril_common::cardano_block_scanner::{BlockScanner, BlockStreamer, ScannedBlock};
///     use mithril_common::entities::{CardanoDbBeacon, CardanoTransaction, ImmutableFileNumber};
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
///               from_immutable: Option<ImmutableFileNumber>,
///               until_immutable: ImmutableFileNumber,
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
        from_immutable: Option<ImmutableFileNumber>,
        until_immutable: ImmutableFileNumber,
    ) -> StdResult<Box<dyn BlockStreamer>>;
}

/// Trait that define how blocks are streamed from a Cardano database
#[async_trait]
pub trait BlockStreamer: Sync + Send {
    /// Stream the next available blocks
    async fn poll_next(&mut self) -> StdResult<Option<Vec<ScannedBlock>>>;

    /// Stream all the available blocks, may be very memory intensive
    async fn poll_all(&mut self) -> StdResult<Vec<ScannedBlock>> {
        let mut blocks = Vec::new();
        while let Some(mut next_blocks) = self.poll_next().await? {
            blocks.append(&mut next_blocks);
        }
        Ok(blocks)
    }
}
