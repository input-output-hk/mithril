use async_trait::async_trait;
use mithril_common::StdResult;

use mithril_common::entities::CertificatePending;

/// Store for [CertificatePending].
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait CertificatePendingStorer: Sync + Send {
    /// Fetch the current [CertificatePending] if any.
    async fn get(&self) -> StdResult<Option<CertificatePending>>;

    /// Save the given [CertificatePending].
    async fn save(&self, certificate: CertificatePending) -> StdResult<()>;

    /// Remove and return the current [CertificatePending] if any.
    async fn remove(&self) -> StdResult<Option<CertificatePending>>;
}
