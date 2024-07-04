use std::collections::BTreeSet;

use serde::{Deserialize, Serialize};

use crate::entities::SignedEntityTypeDiscriminants;

/// Message advertised by an Aggregator to inform about its features
#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct AggregatorFeaturesMessage {
    /// Version of the OpenAPI specification
    pub open_api_version: String,

    /// URL of the documentation
    pub documentation_url: String,

    /// Capabilities of the Aggregator
    pub capabilities: AggregatorCapabilities,
}

impl AggregatorFeaturesMessage {
    /// Create a dummy AggregatorFeaturesMessage
    pub fn dummy() -> Self {
        AggregatorFeaturesMessage {
            open_api_version: "0.0.1".to_string(),
            documentation_url: "https://example.com".to_string(),
            capabilities: AggregatorCapabilities {
                signed_entity_types: BTreeSet::from([
                    SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                ]),
                cardano_transactions_prover: None,
            },
        }
    }
}

/// Capabilities of an Aggregator
#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct AggregatorCapabilities {
    /// Signed entity types that are signed by the aggregator
    pub signed_entity_types: BTreeSet<SignedEntityTypeDiscriminants>,

    /// Cardano transactions prover capabilities
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cardano_transactions_prover: Option<CardanoTransactionsProverCapabilities>,
}

/// Cardano transactions prover capabilities
#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct CardanoTransactionsProverCapabilities {
    /// Maximum number of hashes allowed for a single request
    pub max_hashes_allowed_by_request: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn golden_message() -> AggregatorFeaturesMessage {
        AggregatorFeaturesMessage {
            open_api_version: "0.0.1".to_string(),
            documentation_url: "https://example.com".to_string(),
            capabilities: AggregatorCapabilities {
                signed_entity_types: BTreeSet::from([
                    SignedEntityTypeDiscriminants::CardanoTransactions,
                ]),
                cardano_transactions_prover: Some(CardanoTransactionsProverCapabilities {
                    max_hashes_allowed_by_request: 100,
                }),
            },
        }
    }

    // Test the retro compatibility with possible future upgrades.
    #[test]
    fn test_v1() {
        let json = r#"{
            "open_api_version": "0.0.1",
            "documentation_url": "https://example.com",
            "capabilities": {
                "signed_entity_types": ["CardanoTransactions"],
                "cardano_transactions_prover": {
                    "max_hashes_allowed_by_request": 100
                }
            }
        }"#;

        let message: AggregatorFeaturesMessage = serde_json::from_str(json).unwrap();

        assert_eq!(golden_message(), message);
    }
}
