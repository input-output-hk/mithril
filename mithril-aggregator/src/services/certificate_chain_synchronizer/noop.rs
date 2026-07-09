use mithril_common::StdResult;
use mithril_common::entities::Epoch;

use crate::services::CertificateChainSynchronizer;

/// A noop [CertificateChainSynchronizer] for leader aggregators
pub struct MithrilCertificateChainSynchronizerNoop;

#[async_trait::async_trait]
impl CertificateChainSynchronizer for MithrilCertificateChainSynchronizerNoop {
    async fn synchronize_certificate_chain(&self, _epoch: Epoch, _force: bool) -> StdResult<()> {
        Ok(())
    }
}
