use anyhow::anyhow;
use std::{collections::HashMap, sync::Arc, time::Duration};

use crate::{
    http_client::aggregator_client::{
        AggregatorClient, AggregatorHTTPClient, HTTP_REQUEST_TIMEOUT_DURATION,
    },
    interface::MithrilNetworkConfigurationProvider,
    model::{MithrilNetworkConfiguration, SignedEntityTypeConfiguration},
};
use async_trait::async_trait;
use mithril_common::api_version::APIVersionProvider;
use mithril_common::{StdResult, entities::SignedEntityTypeDiscriminants};

pub struct HttpMithrilNetworkConfigurationProvider {
    aggregator_client: AggregatorHTTPClient,
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

        Self { aggregator_client }
    }
}

#[async_trait]
impl MithrilNetworkConfigurationProvider for HttpMithrilNetworkConfigurationProvider {
    async fn get_network_configuration(&self) -> StdResult<MithrilNetworkConfiguration> {
        let Some(epoch_settings) = self.aggregator_client.retrieve_epoch_settings().await? else {
            return Err(anyhow!("Failed to retrieve epoch settings"));
        };

        let aggregator_features = self.aggregator_client.retrieve_aggregator_features().await?;
        let available_signed_entity_types = aggregator_features.capabilities.signed_entity_types;

        let mut signed_entity_types_config = HashMap::new();
        signed_entity_types_config.insert(
            SignedEntityTypeDiscriminants::CardanoTransactions,
            SignedEntityTypeConfiguration::CardanoTransactions(
                epoch_settings.cardano_transactions_signing_config.ok_or_else(|| {
                    anyhow!("Cardano transactions signing config is missing in epoch settings")
                })?,
            ),
        );

        Ok(MithrilNetworkConfiguration {
            epoch: epoch_settings.epoch,
            signer_registration_protocol_parameters: epoch_settings
                .signer_registration_protocol_parameters,
            available_signed_entity_types,
            signed_entity_types_config,
        })
    }
}
