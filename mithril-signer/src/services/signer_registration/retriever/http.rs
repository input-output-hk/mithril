use mithril_aggregator_client::{AggregatorHttpClient, query::GetEpochSettingsQuery};
use mithril_common::{StdResult, messages::TryFromMessageAdapter};

use crate::services::SignersRegistrationRetriever;
use crate::{FromEpochSettingsAdapter, RegisteredSigners};

#[async_trait::async_trait]
impl SignersRegistrationRetriever for AggregatorHttpClient {
    async fn retrieve_all_signer_registrations(&self) -> StdResult<Option<RegisteredSigners>> {
        let message = self.send(GetEpochSettingsQuery::current()).await?;
        let epoch_settings = FromEpochSettingsAdapter::try_adapt(message)?;

        Ok(Some(epoch_settings))
    }
}
