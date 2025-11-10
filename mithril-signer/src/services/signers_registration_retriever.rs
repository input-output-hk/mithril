use async_trait::async_trait;

use mithril_aggregator_client::AggregatorHttpClient;
use mithril_aggregator_client::query::GetEpochSettingsQuery;
use mithril_common::{StdResult, messages::TryFromMessageAdapter};

use crate::entities::RegisteredSigners;
use crate::message_adapters::FromEpochSettingsAdapter;

/// Service responsible for retrieving the signer's registration from the mithril network
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait SignersRegistrationRetriever: Sync + Send {
    /// Retrieves signer's registration from the mithril network
    async fn retrieve_all_signer_registrations(&self) -> StdResult<Option<RegisteredSigners>>;
}

#[async_trait]
impl SignersRegistrationRetriever for AggregatorHttpClient {
    async fn retrieve_all_signer_registrations(&self) -> StdResult<Option<RegisteredSigners>> {
        let message = self.send(GetEpochSettingsQuery::current()).await?;
        let epoch_settings = FromEpochSettingsAdapter::try_adapt(message)?;

        Ok(Some(epoch_settings))
    }
}

#[cfg(test)]
pub(crate) mod dumb {
    use mithril_common::test::double::Dummy;
    use tokio::sync::RwLock;

    use super::*;

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

    #[async_trait]
    impl SignersRegistrationRetriever for DumbSignersRegistrationRetriever {
        async fn retrieve_all_signer_registrations(&self) -> StdResult<Option<RegisteredSigners>> {
            let epoch_settings = self.epoch_settings.read().await.clone();

            Ok(epoch_settings)
        }
    }
}
