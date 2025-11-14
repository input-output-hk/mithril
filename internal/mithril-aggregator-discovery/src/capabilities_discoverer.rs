use std::sync::Arc;

use mithril_common::{StdResult, messages::AggregatorCapabilities};

use crate::{AggregatorDiscoverer, AggregatorEndpoint, MithrilNetwork};

/// An aggregator discoverer for specific capabilities.
pub struct CapableAggregatorDiscoverer {
    capabilities: AggregatorCapabilities,
    inner_discoverer: Arc<dyn AggregatorDiscoverer>,
}

impl CapableAggregatorDiscoverer {
    /// Creates a new `CapableAggregatorDiscoverer` instance with the provided capabilities.
    pub fn new(
        capabilities: AggregatorCapabilities,
        inner_discoverer: Arc<dyn AggregatorDiscoverer>,
    ) -> Self {
        Self {
            capabilities,
            inner_discoverer,
        }
    }

    /// Check if the available capabilities match the required capabilities.
    ///
    /// Returns true if:
    /// - The aggregate signature types are the same.
    /// - All required signed entity types are included in the available signed entity types.
    fn capabilities_match(
        required: &AggregatorCapabilities,
        available: &AggregatorCapabilities,
    ) -> bool {
        if available.aggregate_signature_type != required.aggregate_signature_type {
            return false;
        }

        let available_signed_entity_types = &available.signed_entity_types;
        let required_signed_entity_types = &required.signed_entity_types;

        required_signed_entity_types
            .iter()
            .all(|req| available_signed_entity_types.contains(req))
    }
}

/// An iterator over aggregator endpoints filtered by capabilities.
struct CapableAggregatorDiscovererIterator {
    capabilities: AggregatorCapabilities,
    inner_iterator: Box<dyn Iterator<Item = AggregatorEndpoint>>,
}

impl Iterator for CapableAggregatorDiscovererIterator {
    type Item = AggregatorEndpoint;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(aggregator_endpoint) = self.inner_iterator.next() {
            let aggregator_endpoint_clone = aggregator_endpoint.clone();
            let aggregator_capabilities = tokio::task::block_in_place(move || {
                tokio::runtime::Handle::current().block_on(async move {
                    aggregator_endpoint_clone.retrieve_capabilities().await
                })
            });
            if let Ok(aggregator_capabilities) = aggregator_capabilities {
                if CapableAggregatorDiscoverer::capabilities_match(
                    &self.capabilities,
                    &aggregator_capabilities,
                ) {
                    return Some(aggregator_endpoint);
                }
            }
        }

        None
    }
}

#[async_trait::async_trait]
impl AggregatorDiscoverer for CapableAggregatorDiscoverer {
    async fn get_available_aggregators(
        &self,
        network: MithrilNetwork,
    ) -> StdResult<Box<dyn Iterator<Item = AggregatorEndpoint>>> {
        let aggregator_endpoints = self.inner_discoverer.get_available_aggregators(network).await?;

        Ok(Box::new(CapableAggregatorDiscovererIterator {
            capabilities: self.capabilities.clone(),
            inner_iterator: aggregator_endpoints,
        }))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use httpmock::MockServer;
    use serde_json::json;

    use mithril_common::{
        AggregateSignatureType,
        entities::SignedEntityTypeDiscriminants::{
            CardanoDatabase, CardanoStakeDistribution, CardanoTransactions,
        },
        messages::AggregatorFeaturesMessage,
    };

    use super::*;

    fn create_aggregator_features_message(
        capabilities: AggregatorCapabilities,
    ) -> AggregatorFeaturesMessage {
        AggregatorFeaturesMessage {
            open_api_version: "1.0.0".to_string(),
            documentation_url: "https://docs".to_string(),
            capabilities,
        }
    }

    #[test]
    fn capabilities_match_success() {
        let required = AggregatorCapabilities {
            aggregate_signature_type: AggregateSignatureType::Concatenation,
            signed_entity_types: BTreeSet::from([CardanoTransactions, CardanoStakeDistribution]),
            cardano_transactions_prover: None,
        };

        let available = AggregatorCapabilities {
            aggregate_signature_type: AggregateSignatureType::Concatenation,
            signed_entity_types: BTreeSet::from([
                CardanoTransactions,
                CardanoStakeDistribution,
                CardanoDatabase,
            ]),
            cardano_transactions_prover: None,
        };

        assert!(CapableAggregatorDiscoverer::capabilities_match(
            &required, &available
        ));
    }

    #[test]
    fn capabilities_match_failure() {
        let required = AggregatorCapabilities {
            aggregate_signature_type: AggregateSignatureType::Concatenation,
            signed_entity_types: BTreeSet::from([CardanoTransactions, CardanoStakeDistribution]),
            cardano_transactions_prover: None,
        };

        let available = AggregatorCapabilities {
            aggregate_signature_type: AggregateSignatureType::Concatenation,
            signed_entity_types: BTreeSet::from([CardanoTransactions]),
            cardano_transactions_prover: None,
        };

        assert!(!CapableAggregatorDiscoverer::capabilities_match(
            &required, &available
        ));
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn get_available_aggregators_success() {
        let capabilities = AggregatorCapabilities {
            aggregate_signature_type: AggregateSignatureType::Concatenation,
            signed_entity_types: BTreeSet::from([CardanoStakeDistribution, CardanoTransactions]),
            cardano_transactions_prover: None,
        };
        let aggregator_server = MockServer::start();
        let aggregator_server_mock = aggregator_server.mock(|when, then| {
            when.path("/");
            then.status(200)
                .body(json!(create_aggregator_features_message(capabilities)).to_string());
        });
        let discoverer = CapableAggregatorDiscoverer::new(
            AggregatorCapabilities {
                aggregate_signature_type: AggregateSignatureType::Concatenation,
                signed_entity_types: BTreeSet::from([CardanoTransactions]),
                cardano_transactions_prover: None,
            },
            Arc::new(crate::test::double::AggregatorDiscovererFake::new(vec![
                Ok(vec![AggregatorEndpoint::new(aggregator_server.url("/"))]),
            ])),
        );

        let mut aggregators = discoverer
            .get_available_aggregators(MithrilNetwork::new("release-devnet".into()))
            .await
            .unwrap();

        let next_aggregator = aggregators.next();
        aggregator_server_mock.assert();
        assert_eq!(
            Some(AggregatorEndpoint::new(aggregator_server.url("/"))),
            next_aggregator
        );
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn get_available_aggregators_succeeds_when_aggregator_capabilities_do_not_match() {
        let capabilities = AggregatorCapabilities {
            aggregate_signature_type: AggregateSignatureType::Concatenation,
            signed_entity_types: BTreeSet::from([CardanoTransactions]),
            cardano_transactions_prover: None,
        };
        let aggregator_server = MockServer::start();
        let aggregator_server_mock = aggregator_server.mock(|when, then| {
            when.path("/");
            then.status(200)
                .body(json!(create_aggregator_features_message(capabilities)).to_string());
        });
        let discoverer = CapableAggregatorDiscoverer::new(
            AggregatorCapabilities {
                aggregate_signature_type: AggregateSignatureType::Concatenation,
                signed_entity_types: BTreeSet::from([CardanoDatabase]),
                cardano_transactions_prover: None,
            },
            Arc::new(crate::test::double::AggregatorDiscovererFake::new(vec![
                Ok(vec![AggregatorEndpoint::new(aggregator_server.url("/"))]),
            ])),
        );

        let mut aggregators = discoverer
            .get_available_aggregators(MithrilNetwork::new("release-devnet".into()))
            .await
            .unwrap();

        let next_aggregator = aggregators.next();
        aggregator_server_mock.assert();
        assert!(next_aggregator.is_none());
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn get_available_aggregators_succeeds_when_one_aggregator_returns_an_error() {
        let aggregator_server_1 = MockServer::start();
        let aggregator_server_mock_1 = aggregator_server_1.mock(|when, then| {
            when.path("/");
            then.status(500);
        });
        let capabilities_2 = AggregatorCapabilities {
            aggregate_signature_type: AggregateSignatureType::Concatenation,
            signed_entity_types: BTreeSet::from([CardanoStakeDistribution, CardanoDatabase]),
            cardano_transactions_prover: None,
        };
        let aggregator_server_2 = MockServer::start();
        let aggregator_server_mock_2 = aggregator_server_2.mock(|when, then| {
            when.path("/");
            then.status(200)
                .body(json!(create_aggregator_features_message(capabilities_2)).to_string());
        });
        let discoverer = CapableAggregatorDiscoverer::new(
            AggregatorCapabilities {
                aggregate_signature_type: AggregateSignatureType::Concatenation,
                signed_entity_types: BTreeSet::from([CardanoDatabase]),
                cardano_transactions_prover: None,
            },
            Arc::new(crate::test::double::AggregatorDiscovererFake::new(vec![
                Ok(vec![
                    AggregatorEndpoint::new(aggregator_server_1.url("/")),
                    AggregatorEndpoint::new(aggregator_server_2.url("/")),
                ]),
            ])),
        );

        let mut aggregators = discoverer
            .get_available_aggregators(MithrilNetwork::new("release-devnet".into()))
            .await
            .unwrap();

        let next_aggregator = aggregators.next();
        aggregator_server_mock_1.assert();
        aggregator_server_mock_2.assert();
        assert_eq!(
            Some(AggregatorEndpoint::new(aggregator_server_2.url("/"))),
            next_aggregator
        );
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn get_available_aggregators_succeeds_and_makes_minimum_calls_to_aggregators() {
        let aggregator_server_1 = MockServer::start();
        let aggregator_server_mock_1 = aggregator_server_1.mock(|when, then| {
            when.path("/");
            then.status(500);
        });
        let capabilities_2 = AggregatorCapabilities {
            aggregate_signature_type: AggregateSignatureType::Concatenation,
            signed_entity_types: BTreeSet::from([CardanoStakeDistribution]),
            cardano_transactions_prover: None,
        };
        let aggregator_server_2 = MockServer::start();
        let aggregator_server_mock_2 = aggregator_server_2.mock(|when, then| {
            when.path("/");
            then.status(200)
                .body(json!(create_aggregator_features_message(capabilities_2)).to_string());
        });
        let capabilities_3 = AggregatorCapabilities {
            aggregate_signature_type: AggregateSignatureType::Concatenation,
            signed_entity_types: BTreeSet::from([CardanoDatabase]),
            cardano_transactions_prover: None,
        };
        let aggregator_server_3 = MockServer::start();
        let aggregator_server_mock_3 = aggregator_server_3.mock(|when, then| {
            when.path("/");
            then.status(200)
                .body(json!(create_aggregator_features_message(capabilities_3)).to_string());
        });
        let capabilities_4 = AggregatorCapabilities {
            aggregate_signature_type: AggregateSignatureType::Concatenation,
            signed_entity_types: BTreeSet::from([CardanoDatabase]),
            cardano_transactions_prover: None,
        };
        let aggregator_server_4 = MockServer::start();
        let aggregator_server_mock_4 = aggregator_server_4.mock(|when, then| {
            when.path("/");
            then.status(200)
                .body(json!(create_aggregator_features_message(capabilities_4)).to_string());
        });
        let discoverer = CapableAggregatorDiscoverer::new(
            AggregatorCapabilities {
                aggregate_signature_type: AggregateSignatureType::Concatenation,
                signed_entity_types: BTreeSet::from([CardanoDatabase]),
                cardano_transactions_prover: None,
            },
            Arc::new(crate::test::double::AggregatorDiscovererFake::new(vec![
                Ok(vec![
                    AggregatorEndpoint::new(aggregator_server_1.url("/")),
                    AggregatorEndpoint::new(aggregator_server_2.url("/")),
                    AggregatorEndpoint::new(aggregator_server_3.url("/")),
                    AggregatorEndpoint::new(aggregator_server_4.url("/")),
                ]),
            ])),
        );

        let mut aggregators = discoverer
            .get_available_aggregators(MithrilNetwork::new("release-devnet".into()))
            .await
            .unwrap();

        let next_aggregator = aggregators.next();
        aggregator_server_mock_1.assert();
        aggregator_server_mock_2.assert();
        aggregator_server_mock_3.assert();
        assert_eq!(0, aggregator_server_mock_4.calls());
        assert_eq!(
            Some(AggregatorEndpoint::new(aggregator_server_3.url("/"))),
            next_aggregator
        );
    }
}
