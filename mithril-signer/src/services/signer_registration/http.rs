use anyhow::Context;

use mithril_aggregator_client::{AggregatorHttpClient, query::PostRegisterSignerQuery};
use mithril_common::{
    StdResult,
    entities::{Epoch, Signer},
    messages::TryToMessageAdapter,
};

use crate::ToRegisterSignerMessageAdapter;
use crate::services::SignerRegistrationPublisher;

#[async_trait::async_trait]
impl SignerRegistrationPublisher for AggregatorHttpClient {
    async fn register_signer(&self, epoch: Epoch, signer: &Signer) -> StdResult<()> {
        let register_signer_message =
            ToRegisterSignerMessageAdapter::try_adapt((epoch, signer.to_owned()))
                .with_context(|| "Failed to adapt message to register signer message")?;

        self.send(PostRegisterSignerQuery::new(register_signer_message))
            .await?;

        Ok(())
    }
}
