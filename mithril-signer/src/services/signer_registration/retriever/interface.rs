use mithril_common::StdResult;

use crate::RegisteredSigners;

/// Service responsible for retrieving the signer's registration from the mithril network
#[cfg_attr(test, mockall::automock)]
#[async_trait::async_trait]
pub trait SignersRegistrationRetriever: Sync + Send {
    /// Retrieves signer's registration from the mithril network
    async fn retrieve_all_signer_registrations(&self) -> StdResult<Option<RegisteredSigners>>;
}
