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
        for aggregator_endpoint in &mut self.inner_iterator {
            let aggregator_capabilities = tokio::runtime::Handle::current()
                .block_on(async { aggregator_endpoint.retrieve_capabilities().await });
            if CapableAggregatorDiscoverer::capabilities_match(
                &self.capabilities,
                &aggregator_capabilities.ok()?,
            ) {
                return Some(aggregator_endpoint);
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

    use mithril_common::{
        AggregateSignatureType,
        entities::SignedEntityTypeDiscriminants::{
            CardanoDatabase, CardanoStakeDistribution, CardanoTransactions,
        },
    };

    use super::*;

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

    #[tokio::test]
    async fn get_available_aggregators_success() {
        todo!()
    }

    #[tokio::test]
    async fn get_available_aggregators_success_when_one_aggregator_capabilities_does_not_match() {
        todo!()
    }

    #[tokio::test]
    async fn get_available_aggregators_success_when_one_aggregator_retruns_an_error() {
        todo!()
    }
}
