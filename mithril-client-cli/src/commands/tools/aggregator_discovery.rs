use clap::Parser;

use mithril_client::{
    AggregatorDiscoveryType, ClientBuilder, MithrilResult, RequiredAggregatorCapabilities,
    common::{AggregateSignatureType, MithrilNetwork, SignedEntityTypeDiscriminants},
};

use crate::{
    CommandContext,
    utils::{ProgressOutputType, ProgressPrinter},
};

/// Clap command to select an aggregator from the available ones with automatic discovery.
#[derive(Parser, Debug, Clone)]
pub struct AggregatorDiscoveryCommand {
    /// Mithril network name
    network: MithrilNetwork,

    /// Maximum number of entries to retrieve
    #[clap(long, default_value_t = 1)]
    max_entries: usize,

    /// Signed entity types to consider for the discovery
    ///
    /// If not provided, all signed entity types are considered.
    #[clap(long, value_parser, num_args = 0.., value_delimiter = ',')]
    signed_entity_types: Vec<SignedEntityTypeDiscriminants>,

    /// Aggregate signature types to consider for the discovery
    ///
    /// If not provided, all aggregate signature types are considered.
    #[clap(long, value_parser, num_args = 0.., value_delimiter = ',')]
    aggregate_signature_types: Vec<AggregateSignatureType>,
}

impl AggregatorDiscoveryCommand {
    /// Main command execution
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        let progress_output_type = if context.is_json_output_enabled() {
            ProgressOutputType::JsonReporter
        } else {
            ProgressOutputType::Tty
        };
        let progress_printer = ProgressPrinter::new(progress_output_type, 1);
        let required_capabilities = self.build_required_capabilities();
        let client_builder =
            ClientBuilder::new(AggregatorDiscoveryType::Automatic(self.network.clone()))
                .with_capabilities(required_capabilities);
        let aggregator_endpoints = client_builder
            .discover_aggregator(&self.network)?
            .take(self.max_entries);
        progress_printer.report_step(
            1,
            &format!(
                "Discovering at most {} aggregator endpoints:",
                self.max_entries
            ),
        )?;
        let mut found_aggregators = 0;
        for endpoint in aggregator_endpoints {
            progress_printer.report_step(1, &format!("Found: {endpoint}"))?;
            found_aggregators += 1;
        }
        if found_aggregators == 0 {
            progress_printer.report_step(
                1,
                "- No aggregator endpoint found matching the requirements.",
            )?;
        }

        Ok(())
    }

    fn build_required_capabilities(&self) -> RequiredAggregatorCapabilities {
        if self.signed_entity_types.is_empty() && self.aggregate_signature_types.is_empty() {
            return RequiredAggregatorCapabilities::All;
        }

        let mut required_capabilities = vec![];
        if !self.signed_entity_types.is_empty() {
            let mut required_capabilities_signed_entity_types = vec![];
            for signed_entity_type in &self.signed_entity_types {
                required_capabilities_signed_entity_types.push(
                    RequiredAggregatorCapabilities::SignedEntityType(*signed_entity_type),
                );
            }
            required_capabilities.push(RequiredAggregatorCapabilities::And(
                required_capabilities_signed_entity_types,
            ));
        }

        if !self.aggregate_signature_types.is_empty() {
            let mut required_capabilities_aggregate_signature_types = vec![];
            for aggregate_signature_type in &self.aggregate_signature_types {
                required_capabilities_aggregate_signature_types.push(
                    RequiredAggregatorCapabilities::AggregateSignatureType(
                        *aggregate_signature_type,
                    ),
                );
            }
            required_capabilities.push(RequiredAggregatorCapabilities::And(
                required_capabilities_aggregate_signature_types,
            ));
        }
        if required_capabilities.len() == 1 {
            required_capabilities.into_iter().next().unwrap()
        } else {
            RequiredAggregatorCapabilities::And(required_capabilities)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mithril_client::common::SignedEntityTypeDiscriminants;

    #[test]
    fn test_build_required_capabilities_all() {
        let command = AggregatorDiscoveryCommand {
            network: MithrilNetwork::dummy(),
            max_entries: 1,
            signed_entity_types: vec![],
            aggregate_signature_types: vec![],
        };

        let required_capabilities = command.build_required_capabilities();
        assert_eq!(required_capabilities, RequiredAggregatorCapabilities::All);
    }

    #[test]
    fn test_build_required_capabilities_signed_entity_types() {
        let command = AggregatorDiscoveryCommand {
            network: MithrilNetwork::dummy(),
            max_entries: 1,
            signed_entity_types: vec![
                SignedEntityTypeDiscriminants::CardanoTransactions,
                SignedEntityTypeDiscriminants::CardanoStakeDistribution,
            ],
            aggregate_signature_types: vec![],
        };

        let required_capabilities = command.build_required_capabilities();

        assert_eq!(
            required_capabilities,
            RequiredAggregatorCapabilities::And(vec![
                RequiredAggregatorCapabilities::SignedEntityType(
                    SignedEntityTypeDiscriminants::CardanoTransactions
                ),
                RequiredAggregatorCapabilities::SignedEntityType(
                    SignedEntityTypeDiscriminants::CardanoStakeDistribution
                ),
            ])
        );
    }

    #[test]
    fn test_build_required_capabilities_aggregate_signature_types() {
        let command = AggregatorDiscoveryCommand {
            network: MithrilNetwork::dummy(),
            max_entries: 1,
            signed_entity_types: vec![],
            aggregate_signature_types: vec![AggregateSignatureType::Concatenation],
        };
        let required_capabilities = command.build_required_capabilities();

        assert_eq!(
            required_capabilities,
            RequiredAggregatorCapabilities::And(vec![
                RequiredAggregatorCapabilities::AggregateSignatureType(
                    AggregateSignatureType::Concatenation
                ),
            ])
        );
    }

    #[test]
    fn test_build_required_capabilities_both() {
        let command = AggregatorDiscoveryCommand {
            network: MithrilNetwork::dummy(),
            max_entries: 1,
            signed_entity_types: vec![
                SignedEntityTypeDiscriminants::CardanoTransactions,
                SignedEntityTypeDiscriminants::CardanoStakeDistribution,
            ],
            aggregate_signature_types: vec![AggregateSignatureType::Concatenation],
        };
        let required_capabilities = command.build_required_capabilities();

        assert_eq!(
            required_capabilities,
            RequiredAggregatorCapabilities::And(vec![
                RequiredAggregatorCapabilities::And(vec![
                    RequiredAggregatorCapabilities::SignedEntityType(
                        SignedEntityTypeDiscriminants::CardanoTransactions
                    ),
                    RequiredAggregatorCapabilities::SignedEntityType(
                        SignedEntityTypeDiscriminants::CardanoStakeDistribution
                    ),
                ]),
                RequiredAggregatorCapabilities::And(vec![
                    RequiredAggregatorCapabilities::AggregateSignatureType(
                        AggregateSignatureType::Concatenation
                    ),
                ]),
            ])
        );
    }
}
