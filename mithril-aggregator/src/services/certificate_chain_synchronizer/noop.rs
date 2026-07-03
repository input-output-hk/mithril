use mithril_common::StdResult;

use crate::services::{CertificateChainSynchronizationOutcome, CertificateChainSynchronizer};

/// A noop [CertificateChainSynchronizer] for leader aggregators
pub struct MithrilCertificateChainSynchronizerNoop;

#[async_trait::async_trait]
impl CertificateChainSynchronizer for MithrilCertificateChainSynchronizerNoop {
    async fn synchronize_certificate_chain(
        &self,
        _force: bool,
    ) -> StdResult<CertificateChainSynchronizationOutcome> {
        Ok(CertificateChainSynchronizationOutcome::AlreadyUpToDate)
    }
}
