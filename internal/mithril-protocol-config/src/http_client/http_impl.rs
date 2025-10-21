//! HTTP implementation of MithrilNetworkConfigurationProvider.

use anyhow::{Context, anyhow};
use async_trait::async_trait;
use std::sync::Arc;

use mithril_common::StdResult;
use mithril_common::entities::Epoch;
use mithril_common::messages::ProtocolConfigurationMessage;

use crate::interface::MithrilNetworkConfigurationProvider;
use crate::model::{MithrilNetworkConfiguration, SignedEntityTypeConfiguration};

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
        let signer_retrieval_epoch =
            epoch.offset_to_signer_retrieval_epoch().with_context(|| {
                format!("MithrilNetworkConfigurationProvider could not compute signer retrieval epoch from epoch: {epoch}")
            })?;
        let signer_registration_epoch = epoch.offset_to_next_signer_retrieval_epoch().next();

        let protocol_configuration = self
            .protocol_configuration_retriever
            .retrieve_protocol_configuration(signer_retrieval_epoch)
            .await?;

        let signer_registration_protocol_configuration = self
            .protocol_configuration_retriever
            .retrieve_protocol_configuration(signer_registration_epoch)
            .await?;

        let available_signed_entity_types =
            protocol_configuration.available_signed_entity_types.clone();

        let cardano_transactions = protocol_configuration
            .cardano_transactions_signing_config
            .ok_or_else(|| {
                anyhow!(format!("Cardano transactions signing config is missing in protocol configuration for epoch {epoch}"))
            })?;

        let signed_entity_types_config = SignedEntityTypeConfiguration {
            cardano_transactions: Some(cardano_transactions),
        };

        Ok(MithrilNetworkConfiguration {
            epoch,
            signer_registration_protocol_parameters: signer_registration_protocol_configuration
                .protocol_parameters,
            available_signed_entity_types,
            signed_entity_types_config,
        })
    }
}

#[cfg(test)]
mod tests {
    use mockall::predicate::eq;
    use std::sync::Arc;

    use mithril_common::{
        entities::{BlockNumber, CardanoTransactionsSigningConfig, Epoch, ProtocolParameters},
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
    async fn test_get_network_configuration_retrieve_signer_protocol_parameters_from_next_epoch() {
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
            .with(eq(Epoch(43)))
            .returning(|_| {
                Ok(ProtocolConfigurationMessage {
                    protocol_parameters: ProtocolParameters::new(2000, 200, 0.2),
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
            configuration.signer_registration_protocol_parameters,
            ProtocolParameters::new(2000, 200, 0.2)
        );
    }

    #[tokio::test]
    async fn test_get_network_configuration_retrieve_cardano_transaction_config_from_previous_epoch()
     {
        let mut protocol_configuration_retriever = MockProtocolConfigurationRetriever::new();

        protocol_configuration_retriever
            .expect_retrieve_protocol_configuration()
            .once()
            .with(eq(Epoch(41)))
            .returning(|_| {
                Ok(ProtocolConfigurationMessage {
                    cardano_transactions_signing_config: Some(CardanoTransactionsSigningConfig {
                        security_parameter: BlockNumber(111),
                        step: BlockNumber(222),
                    }),
                    ..Dummy::dummy()
                })
            });

        protocol_configuration_retriever
            .expect_retrieve_protocol_configuration()
            .once()
            .with(eq(Epoch(43)))
            .returning(|_| {
                Ok(ProtocolConfigurationMessage {
                    cardano_transactions_signing_config: Some(CardanoTransactionsSigningConfig {
                        security_parameter: BlockNumber(333),
                        step: BlockNumber(444),
                    }),
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

        let actual_cardano_transations = configuration
            .signed_entity_types_config
            .cardano_transactions
            .expect("should have cardano_transactions");

        assert_eq!(
            actual_cardano_transations,
            CardanoTransactionsSigningConfig {
                security_parameter: BlockNumber(111),
                step: BlockNumber(222),
            }
        );
    }
}
