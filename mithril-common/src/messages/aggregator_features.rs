use std::collections::BTreeSet;

use serde::{Deserialize, Serialize};

use crate::entities::{CardanoTransactionsSigningConfig, SignedEntityTypeDiscriminants};

/// Message advertised by an Aggregator to inform about its features
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
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
                cardano_transactions_signing_config: None,
            },
        }
    }
}

/// Capabilities of an Aggregator
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AggregatorCapabilities {
    /// Signed entity types that are signed by the aggregator
    pub signed_entity_types: BTreeSet<SignedEntityTypeDiscriminants>,

    /// Cardano transactions prover capabilities
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cardano_transactions_prover: Option<CardanoTransactionsProverCapabilities>,

    /// Cardano transactions signing configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cardano_transactions_signing_config: Option<CardanoTransactionsSigningConfig>,
}

/// Cardano transactions prover capabilities
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct CardanoTransactionsProverCapabilities {
    /// Maximum number of hashes allowed for a single request
    pub max_hashes_allowed_by_request: usize,
}

#[cfg(test)]
mod tests {
    use crate::entities::BlockNumber;

    use super::*;

    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    struct AggregatorFeaturesMessagePrevious {
        pub open_api_version: String,
        pub documentation_url: String,
        pub capabilities: AggregatorCapabilitiesPrevious,
    }

    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    struct AggregatorCapabilitiesPrevious {
        pub signed_entity_types: BTreeSet<SignedEntityTypeDiscriminants>,
        #[serde(skip_serializing_if = "Option::is_none")]
        pub cardano_transactions_prover: Option<CardanoTransactionsProverCapabilities>,
    }

    fn golden_message_previous() -> AggregatorFeaturesMessagePrevious {
        AggregatorFeaturesMessagePrevious {
            open_api_version: "0.0.1".to_string(),
            documentation_url: "https://example.com".to_string(),
            capabilities: AggregatorCapabilitiesPrevious {
                signed_entity_types: BTreeSet::from([
                    SignedEntityTypeDiscriminants::CardanoTransactions,
                ]),
                cardano_transactions_prover: Some(CardanoTransactionsProverCapabilities {
                    max_hashes_allowed_by_request: 100,
                }),
            },
        }
    }

    fn golden_message_actual() -> AggregatorFeaturesMessage {
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
                cardano_transactions_signing_config: Some(CardanoTransactionsSigningConfig {
                    security_parameter: BlockNumber(70),
                    step: BlockNumber(20),
                }),
            },
        }
    }

    const ACTUAL_JSON: &str = r#"{
        "open_api_version": "0.0.1",
        "documentation_url": "https://example.com",
        "capabilities": {
            "signed_entity_types": ["CardanoTransactions"],
            "cardano_transactions_prover": {
                "max_hashes_allowed_by_request": 100
            },
            "cardano_transactions_signing_config": {
                "security_parameter": 70,
                "step": 20
            }
        }
    }"#;

    // Test the retro compatibility with possible future upgrades.
    #[test]
    fn test_actual_json_deserialized_into_previous_message() {
        let json = ACTUAL_JSON;
        let message: AggregatorFeaturesMessagePrevious = serde_json::from_str(json).unwrap();

        assert_eq!(golden_message_previous(), message);
    }

    #[test]
    fn test_actual_json_deserialized_into_actual_message() {
        let json = ACTUAL_JSON;
        let message: AggregatorFeaturesMessage = serde_json::from_str(json).unwrap();

        assert_eq!(golden_message_actual(), message);
    }
}
