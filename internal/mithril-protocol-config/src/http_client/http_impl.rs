//! HTTP implementation of MithrilNetworkConfigurationProvider.

use anyhow::{Context, anyhow};
use async_trait::async_trait;
use std::sync::Arc;

use mithril_common::StdResult;
use mithril_common::entities::Epoch;
use mithril_common::messages::ProtocolConfigurationMessage;

use crate::interface::MithrilNetworkConfigurationProvider;
use crate::model::{EpochConfiguration, MithrilNetworkConfiguration};

/// Trait to retrieve protocol configuration
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait ProtocolConfigurationRetriever: Sync + Send {
    /// Retrieves protocol configuration for a given epoch from the aggregator
    async fn retrieve_protocol_configuration(
        &self,
        epoch: Epoch,
    ) -> StdResult<ProtocolConfigurationMessage>;
}

/// Structure implementing MithrilNetworkConfigurationProvider using HTTP.
pub struct HttpMithrilNetworkConfigurationProvider {
    protocol_configuration_retriever: Arc<dyn ProtocolConfigurationRetriever>,
}

impl HttpMithrilNetworkConfigurationProvider {
    /// HttpMithrilNetworkConfigurationProvider factory
    pub fn new(protocol_configuration_retriever: Arc<dyn ProtocolConfigurationRetriever>) -> Self {
        Self {
            protocol_configuration_retriever,
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

        let configuration_for_aggregation: EpochConfiguration = self
            .protocol_configuration_retriever
            .retrieve_protocol_configuration(aggregation_epoch)
            .await?
            .into();

        let configuration_for_next_aggregation = self
            .protocol_configuration_retriever
            .retrieve_protocol_configuration(next_aggregation_epoch)
            .await?
            .into();

        let configuration_for_registration = self
            .protocol_configuration_retriever
            .retrieve_protocol_configuration(registration_epoch)
            .await?
            .into();

        configuration_for_aggregation.signed_entity_types_config.cardano_transactions.clone()
            .ok_or_else(|| {
                anyhow!(format!("Cardano transactions signing config is missing in aggregation configuration for epoch {epoch}"))
            })?;

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
    use mockall::predicate::eq;
    use std::sync::Arc;

    use mithril_common::{
        entities::{Epoch, ProtocolParameters},
        messages::ProtocolConfigurationMessage,
        test::double::Dummy,
    };

    use crate::{
        http_client::http_impl::{
            HttpMithrilNetworkConfigurationProvider, MockProtocolConfigurationRetriever,
        },
        interface::MithrilNetworkConfigurationProvider,
    };

    #[tokio::test]
    async fn test_get_network_configuration_retrieve_configurations_for_aggregation_next_aggregation_and_registration()
     {
        let mut protocol_configuration_retriever = MockProtocolConfigurationRetriever::new();

        protocol_configuration_retriever
            .expect_retrieve_protocol_configuration()
            .once()
            .with(eq(Epoch(41)))
            .returning(|_| {
                Ok(ProtocolConfigurationMessage {
                    protocol_parameters: ProtocolParameters::new(1000, 100, 0.1),
                    ..Dummy::dummy()
                })
            });

        protocol_configuration_retriever
            .expect_retrieve_protocol_configuration()
            .once()
            .with(eq(Epoch(42)))
            .returning(|_| {
                Ok(ProtocolConfigurationMessage {
                    protocol_parameters: ProtocolParameters::new(2000, 200, 0.2),
                    ..Dummy::dummy()
                })
            });

        protocol_configuration_retriever
            .expect_retrieve_protocol_configuration()
            .once()
            .with(eq(Epoch(43)))
            .returning(|_| {
                Ok(ProtocolConfigurationMessage {
                    protocol_parameters: ProtocolParameters::new(3000, 300, 0.3),
                    ..Dummy::dummy()
                })
            });

        let mithril_configuration_provider = HttpMithrilNetworkConfigurationProvider::new(
            Arc::new(protocol_configuration_retriever),
        );

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
