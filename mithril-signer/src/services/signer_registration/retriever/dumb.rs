use tokio::sync::RwLock;

use mithril_common::StdResult;
use mithril_common::test::double::Dummy;

use crate::RegisteredSigners;
use crate::services::signer_registration::retriever::interface::SignersRegistrationRetriever;

/// Dumb `SignersRegistrationRetriever` implementation for testing
pub struct DumbSignersRegistrationRetriever {
    epoch_settings: RwLock<Option<RegisteredSigners>>,
}

impl Default for DumbSignersRegistrationRetriever {
    fn default() -> Self {
        Self {
            epoch_settings: RwLock::new(Some(RegisteredSigners::dummy())),
        }
    }
}

#[async_trait::async_trait]
impl SignersRegistrationRetriever for DumbSignersRegistrationRetriever {
    async fn retrieve_all_signer_registrations(&self) -> StdResult<Option<RegisteredSigners>> {
        let epoch_settings = self.epoch_settings.read().await.clone();

        Ok(epoch_settings)
    }
}
