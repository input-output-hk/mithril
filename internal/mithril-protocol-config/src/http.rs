//! HTTP implementation of MithrilNetworkConfigurationProvider.

use anyhow::{Context, anyhow};
use async_trait::async_trait;
use std::sync::Arc;

use mithril_aggregator_client::AggregatorHttpClient;
use mithril_aggregator_client::query::GetProtocolConfigurationQuery;
use mithril_common::StdResult;
use mithril_common::entities::Epoch;

use crate::interface::MithrilNetworkConfigurationProvider;
use crate::model::{MithrilNetworkConfiguration, MithrilNetworkConfigurationForEpoch};

/// Structure implementing MithrilNetworkConfigurationProvider using HTTP.
pub struct HttpMithrilNetworkConfigurationProvider {
    aggregator_http_client: Arc<AggregatorHttpClient>,
}

impl HttpMithrilNetworkConfigurationProvider {
    /// HttpMithrilNetworkConfigurationProvider factory
    pub fn new(aggregator_http_client: Arc<AggregatorHttpClient>) -> Self {
        Self {
            aggregator_http_client,
        }
    }
}

#[async_trait]
impl MithrilNetworkConfigurationProvider for HttpMithrilNetworkConfigurationProvider {
    async fn get_network_configuration(
        &self,
        epoch: Epoch,
    ) -> StdResult<MithrilNetworkConfiguration> {
        let aggregation_epoch =
            epoch.offset_to_signer_retrieval_epoch().with_context(|| {
                format!("MithrilNetworkConfigurationProvider could not compute aggregation epoch from epoch: {epoch}")
            })?;
        let next_aggregation_epoch = epoch.offset_to_next_signer_retrieval_epoch();
        let registration_epoch = epoch.offset_to_next_signer_retrieval_epoch().next();

        let configuration_for_aggregation: MithrilNetworkConfigurationForEpoch = self
            .aggregator_http_client
            .send(GetProtocolConfigurationQuery::epoch(aggregation_epoch))
            .await?
            .ok_or(anyhow!(
                "Missing network configuration for aggregation epoch {aggregation_epoch}"
            ))?
            .into();

        let configuration_for_next_aggregation = self
            .aggregator_http_client
            .send(GetProtocolConfigurationQuery::epoch(next_aggregation_epoch))
            .await?
            .ok_or(anyhow!(
                "Missing network configuration for next aggregation epoch {next_aggregation_epoch}"
            ))?
            .into();

        let configuration_for_registration = self
            .aggregator_http_client
            .send(GetProtocolConfigurationQuery::epoch(registration_epoch))
            .await?
            .ok_or(anyhow!(
                "Missing network configuration for registration epoch {registration_epoch}"
            ))?
            .into();

        Ok(MithrilNetworkConfiguration {
            epoch,
            configuration_for_aggregation,
            configuration_for_next_aggregation,
            configuration_for_registration,
        })
    }
}

#[cfg(test)]
mod tests {
    use httpmock::MockServer;
    use std::sync::Arc;

    use mithril_common::entities::ProtocolParameters;
    use mithril_common::messages::ProtocolConfigurationMessage;
    use mithril_common::test::double::Dummy;

    use crate::test::test_tools::TestLogger;

    use super::*;

    #[tokio::test]
    async fn test_get_network_configuration_retrieve_configurations_for_aggregation_next_aggregation_and_registration()
     {
        let configuration_epoch_41 = ProtocolConfigurationMessage {
            protocol_parameters: ProtocolParameters::new(1000, 100, 0.1),
            ..Dummy::dummy()
        };
        let configuration_epoch_42 = ProtocolConfigurationMessage {
            protocol_parameters: ProtocolParameters::new(2000, 200, 0.2),
            ..Dummy::dummy()
        };
        let configuration_epoch_43 = ProtocolConfigurationMessage {
            protocol_parameters: ProtocolParameters::new(3000, 300, 0.3),
            ..Dummy::dummy()
        };

        let server = MockServer::start();
        server.mock(|when, then| {
            when.path("/protocol-configuration/41");
            then.status(200)
                .body(serde_json::to_string(&configuration_epoch_41).unwrap());
        });
        server.mock(|when, then| {
            when.path("/protocol-configuration/42");
            then.status(200)
                .body(serde_json::to_string(&configuration_epoch_42).unwrap());
        });
        server.mock(|when, then| {
            when.path("/protocol-configuration/43");
            then.status(200)
                .body(serde_json::to_string(&configuration_epoch_43).unwrap());
        });

        let mithril_configuration_provider =
            HttpMithrilNetworkConfigurationProvider::new(Arc::new(
                AggregatorHttpClient::builder(server.base_url())
                    .with_logger(TestLogger::stdout())
                    .build()
                    .unwrap(),
            ));

        let configuration = mithril_configuration_provider
            .get_network_configuration(Epoch(42))
            .await
            .expect("should have configuration");

        assert_eq!(
            configuration.configuration_for_aggregation.protocol_parameters,
            ProtocolParameters::new(1000, 100, 0.1)
        );

        assert_eq!(
            configuration.configuration_for_next_aggregation.protocol_parameters,
            ProtocolParameters::new(2000, 200, 0.2)
        );

        assert_eq!(
            configuration.configuration_for_registration.protocol_parameters,
            ProtocolParameters::new(3000, 300, 0.3)
        );
    }
}
