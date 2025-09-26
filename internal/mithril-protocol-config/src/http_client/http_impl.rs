use anyhow::anyhow;
use std::{collections::BTreeSet, sync::Arc, time::Duration};

use crate::{
    HTTP_REQUEST_TIMEOUT_DURATION,
    http_client::aggregator_client::{AggregatorClient, AggregatorHTTPClient},
    interface::MithrilNetworkConfigurationProvider,
    model::MithrilNetworkConfiguration,
};
use async_trait::async_trait;
use mithril_common::StdResult;
use mithril_common::api_version::APIVersionProvider;

struct HttpMithrilNetworkConfigurationProvider {
    aggregator_client: AggregatorHTTPClient,
    logger: slog::Logger,
}

impl HttpMithrilNetworkConfigurationProvider {
    pub fn new(
        aggregator_endpoint: String,
        relay_endpoint: Option<String>,
        api_version_provider: Arc<APIVersionProvider>,
        logger: slog::Logger,
    ) -> Self {
        let aggregator_client = AggregatorHTTPClient::new(
            aggregator_endpoint.clone(),
            relay_endpoint.clone(),
            api_version_provider.clone(),
            Some(Duration::from_millis(HTTP_REQUEST_TIMEOUT_DURATION)),
            logger.clone(),
        );

        Self {
            aggregator_client,
            logger,
        }
    }
}

#[async_trait]
impl MithrilNetworkConfigurationProvider for HttpMithrilNetworkConfigurationProvider {
    async fn get(&self) -> StdResult<MithrilNetworkConfiguration> {
        let Some(epoch_settings) = self.aggregator_client.retrieve_epoch_settings().await? else {
            return Err(anyhow!("Failed to retrieve epoch settings"));
        };

        let available_signed_entity_types = BTreeSet::new(); // To be implemented to be retrieve from /aggreagator-features from aggregator_client.rs
        let signed_entity_types_config = vec![]; // To be implemented

        Ok(MithrilNetworkConfiguration {
            epoch: epoch_settings.epoch,
            signer_registration_protocol_parameters: epoch_settings
                .signer_registration_protocol_parameters,
            available_signed_entity_types,
            signed_entity_types_config,
        })
    }
}
